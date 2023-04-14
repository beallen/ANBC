#
# Title: Creation of species distribution models
# Created: August 30th, 2022
# Last Updated: April 14th, 2023
# Author: Brandon Allen
# Objectives: Develop species distribution models for species with sufficient detections.
# Keywords: Notes, Standardization, Species Models, Visualization
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
# 3) Some accession values don't have IDs.
# 4) Some species with IDs don't have lat long coordinates or other site identifiers.
# 5) Need to resolve site matching
#
###################
# Standardization # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(corrplot)
library(ggplot2)
require(ggnewscale)
library(MetBrewer)
library(pROC)

# Load data
load("data/processed/landcover/veg-hf_800m_wide_simplified.Rdata")
load("data/processed/species/species-wide-form.Rdata")
load("data/base/sites/anbc-climate-summaries.Rdata")
site.lookup <- read.csv("data/lookup/site-lookup-all-surveys.csv")
species.lookup <- read.csv("data/lookup/species-lookup.csv")

# Filter for species of interest
species.lookup <- species.lookup[species.lookup$Species %in% colnames(species.wide)[7:77], ]

# Filter for the projects of interest
site.lookup <- site.lookup[!is.na(site.lookup$SurveyDays), ] # Remove sites with missing survey effort
site.lookup <- site.lookup[site.lookup$Project %in% c("AEP", "ANBC", "ACA"), ] # Keep only the data with surveys
#site.info <- site.info[site.info$NRNAME != "Grassland", ]
#site.lookup <- site.lookup[site.lookup$UID %in% site.info$UID, ]

species.wide$UID <- paste(species.wide$Project, species.wide$SiteID, species.wide$Year, sep = "_")
rownames(species.wide) <- species.wide$UID
species.wide <- species.wide[site.lookup$UID, ]
veg.data <- veg.data[site.lookup$UID, ]
colnames(veg.data)[1] <- "UID"
rownames(climate.data) <- climate.data$UID
climate.data <- climate.data[site.lookup$UID,]

model.data <- merge.data.frame(climate.data, site.lookup[, -c(2:6)], by = "UID") # Merge site and climate
model.data <- merge.data.frame(model.data, veg.data, by = "UID") # Add vegetation
model.data <- merge.data.frame(model.data, species.wide[, -c(1:6)], by = "UID") # Add species abundance
model.data$SurveyDays2 <- model.data$SurveyDays * model.data$SurveyDays

# Determine which species have sufficient data to model
species.list <- colSums(ifelse(species.wide[, -c(1:6)] > 0, 1, 0))
species.list <- species.list[species.list > 20]
species.list <- names(species.list)[!(names(species.list) %in% "UID")]

##################
# Species Models # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assess redundant variables
corr <- cor(model.data[, c(5:13, 15:29)])
corrplot(corr)

# We are going to combine the stand classes further 
model.data$DecidMixed <- rowSums(model.data[, c("Deciduous", "Mixedwood")])
model.data$PineSpruce <- rowSums(model.data[, c("Pine", "Spruce")])
model.data$WetlandSwamp <- rowSums(model.data[, c("Wetland", "Swamp")])

# We are not going to model hard linear features (bees aren't actively using it)

# Define list for storing results
species.results <- list()

# Bombus
for (species in species.list[3:20]) {
  
  # Define the singular model
  model.data$Species <- model.data[, species]
  
  # ggplot(data = model.data) +
  #   geom_point(aes(x = Longitude, y = Latitude, color = Species))
  
  species.results[[species]] <- glm(Species~ DecidMixed + PineSpruce + TreedBogFen + GrassShrub + WetlandSwamp + 
                                      UrbInd + SoftLin + Crop + Pasture + Forestry + 
                                      SurveyDays + SurveyDays2, 
                                    family = "poisson",
                                    data = model.data, 
                                    maxit = 250)
  print(auc(ifelse(model.data$Species > 1, 1, 0), predict(species.results[[species]], type = "response")))
  
}

# Non bombus
for (species in species.list[c(1,2,21,22)]) {
  
  # Define the singular model
  model.data$Species <- model.data[, species]
  temp.data <- model.data[model.data$Project != "AEP", ]
  
  species.results[[species]] <- glm(Species~ DecidMixed + PineSpruce + TreedBogFen + GrassShrub + WetlandSwamp + 
                                      UrbInd + SoftLin + Crop + Pasture + Forestry +
                                      SurveyDays + SurveyDays2, 
                                    family = "poisson",
                                    data = temp.data, 
                                    maxit = 250)
  
  print(auc(ifelse(temp.data$Species > 1, 1, 0), predict(species.results[[species]], type = "response")))
  
  
}

#################
# Visualization #
#################

# Load the kgrid landcover layer
load("data/processed/landcover/kgrid-processed.Rdata")
load("data/base/mapping/provincial-boundary.Rdata")
load("data/base/mapping/kgrid_mapping.Rdata")

# We are going to combine the stand classes further 
kgrid.pred$DecidMixed <- rowSums(kgrid.pred[, c("Deciduous", "Mixedwood")])
kgrid.pred$PineSpruce <- rowSums(kgrid.pred[, c("Pine", "Spruce")])
kgrid.pred$WetlandSwamp <- rowSums(kgrid.pred[, c("Wetland", "Swamp")])

for (species in names(species.results)) {
  
  # Create predictions for the two models
  kgrid.pred[, species] <- predict(species.results[[species]], newdata = cbind(kgrid.pred, data.frame(SurveyDays = 0, SurveyDays2 = 0)), type = "response")
  kgrid.pred[, species] <- ifelse(kgrid.pred[, species] >= quantile(kgrid.pred[, species], 0.99), 
                                  quantile(kgrid.pred[, species], 0.99), 
                                  kgrid.pred[, species])
  
}

# Merge the two kgrid version for visualization
kgrid.pred <- merge.data.frame(kgrid.pred, kgrid[, c(1,5,6)], by = "LinkID")

for (species in names(species.results)) {
  
  # Plot species model (scaled between 0-1)
  kgrid.pred$Abundance <- kgrid.pred[, species] / max(kgrid.pred[, species])
  
  species.plot <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
    scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_raster(data = kgrid.pred , aes(x = x, y = y, fill = Abundance)) +
    scale_fill_gradientn(name = paste0("Relative\nAbundance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
    scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "none") +
    theme_light() +
    ggtitle(species) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size=18),
          axis.text.y = element_text(size=18),
          panel.grid.major.y = element_blank(),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16),
          legend.key.size = unit(1, "cm"), 
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.15, 0.15))
  
  ggsave(filename = paste0("results/figures/species/", species, "/map.jpeg"), 
         plot = species.plot,
         height = 900,
         width = 600, 
         dpi = 72,
         quality = 100,
         units = "px")
  
  #
  # Coefficient plots
  #

  main.coef <- data.frame(Coefficient = names(species.results[[species]]$coefficients),
                          Value = link(species.results[[species]]$coefficients))
  main.coef <- main.coef[2:11, ]
  main.coef$Coefficient <- factor(main.coef$Coefficient, levels = main.coef$Coefficient)
  main.coef$Value <- main.coef$Value / max(main.coef$Value)
  main.coef$Color <-   c("#748838", "#663301", "#448278", "#C89222", "#1D3991",
                         "#9D350B", "#532E8C","#FE9929", "#DCCF63", "#63A70C")
  
  veg.plot <- ggplot(data = main.coef, aes(x = Coefficient, y = Value, fill = Color)) +
    geom_bar(stat = "identity", fill = main.coef$Color) +
    labs(x = "Coefficient", y = "Relative Abundance") +
    theme_light() +
    theme(axis.title = element_text(size=16),
          axis.text.x = element_text(size=16, angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(size=16),
          title = element_text(size=12),
          legend.text = element_text(size=16),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  ggsave(filename = paste0("results/figures/species/", species, "/veghf.jpeg"), 
         plot = veg.plot,
         height = 600,
         width = 1200, 
         dpi = 72,
         quality = 100,
         units = "px")
  
  rm(species.plot, veg.plot)
  print(species)
  
}


