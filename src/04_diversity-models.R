#
# Title: Creation of diversity models.
# Created: March 27th, 2023
# Last Updated: March 27th, 2023
# Author: Brandon Allen
# Objectives: Develop diversity based models for the two groups (Bombus, Others)
# Keywords: Notes, Standardization, Richness, 
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) When modeling Bombus, use the ANBC, AEP, and ACA sites
# 2) When modeling the other genus, use only the ANBC and ACA sites
# 3) The presence only data is not included in the modeling of site richness.
# 4) For now, standardize by the number of survey days. Consider exploring rarefaction.
# 5) For making these models, we will need to consider survey effort a lot. Depending on the subset of the available data, 
# we might only have a few sites with information
#
###################
# Standardization # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
require(ggnewscale)
library(MetBrewer)

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

site.richness <- species.wide[, c(78, 7:77)]
site.richness[, -1] <- ifelse(site.richness[, -1] >= 1, 1, 0)
site.richness$BombusRich <- rowSums(site.richness[, species.lookup[species.lookup$Genus == "Bombus", "AnalysisName"]])
site.richness$AllRich <- rowSums(site.richness[, species.lookup$AnalysisName])

model.data <- merge.data.frame(climate.data, site.lookup[, -c(2:6)], by = "UID") # Merge site and climate
model.data <- merge.data.frame(model.data, veg.data, by = "UID") # Add vegetation
model.data <- merge.data.frame(model.data, site.richness[, c("UID", "BombusRich", "AllRich")], by = "UID") # Add unscaled richness
model.data <- merge.data.frame(model.data, species.wide[, -c(1:6)], by = "UID") # Add species abundance
model.data$SurveyDays2 <- model.data$SurveyDays * model.data$SurveyDays

############
# Richness #
############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Assess redundant variables
library(corrplot)
corr <- cor(model.data[, c(5:13, 15:29)])
corrplot(corr)

#
# Bombus
#

bombus.data <- model.data

bombus.model <- glm(BombusRich ~ Deciduous + Mixedwood + Pine + Spruce + TreedBogFen + Swamp + GrassShrub + Wetland + 
                      UrbInd + SoftLin + Crop + Pasture + Forestry + 
                      Longitude + FFP + MAT +
                      SurveyDays + SurveyDays2, 
                 family = "poisson",
                 data = bombus.data, 
                 maxit = 250)

#
# All
#

all.data <- model.data[model.data$Project %in% c("ACA", "ANBC"), ]

all.model <- glm(AllRich ~ Deciduous + Mixedwood + Pine + Spruce + TreedBogFen + Swamp + GrassShrub + Wetland + 
                   UrbInd + SoftLin + Crop + Pasture + Forestry + 
                   Longitude + FFP + MAT +
                   SurveyDays + SurveyDays2,  
                   family = "poisson",
                   data = all.data, 
                   maxit = 250)

# Load the kgrid landcover layer
load("data/processed/landcover/kgrid-processed.Rdata")
load("data/base/mapping/provincial-boundary.Rdata")
load("data/base/mapping/kgrid_mapping.Rdata")

# Create predictions for the two models
kgrid.pred$Bombus <- predict(bombus.model, newdata = cbind(kgrid.pred, data.frame(SurveyDays = 10, SurveyDays2 = 100)), type = "response")
kgrid.pred$Bombus <- ifelse(kgrid.pred$Bombus >= quantile(kgrid.pred$Bombus, 0.999), quantile(kgrid.pred$Bombus, 0.999), kgrid.pred$Bombus) # Truncate
kgrid.pred$All <- predict(all.model, newdata = cbind(kgrid.pred, data.frame(SurveyDays = 10, SurveyDays2 = 100)), type = "response")
kgrid.pred$All <- ifelse(kgrid.pred$All >= quantile(kgrid.pred$All, 0.999), quantile(kgrid.pred$All, 0.999), kgrid.pred$All) # Truncate

# Rescale between 0-1
kgrid.pred$Bombus <- kgrid.pred$Bombus / max(kgrid.pred$Bombus)
kgrid.pred$All <- kgrid.pred$All / max(kgrid.pred$All)

# Align with the raster kgrid
kgrid <- kgrid[kgrid.pred$LinkID, ]
kgrid$BombusRich <- kgrid.pred$Bombus
kgrid$AllRich <- kgrid.pred$All

# Bombus model
bombus.plot <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
  scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_raster(data = kgrid , aes(x = x, y = y, fill = BombusRich)) +
  scale_fill_gradientn(name = paste0("Relative\nRichness"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,5), guide = "none") +
  theme_light() +
  ggtitle("Bombus Richness") +
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

# All model
all.plot <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
  scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_raster(data = kgrid , aes(x = x, y = y, fill = AllRich)) +
  scale_fill_gradientn(name = paste0("Relative\nRichness"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,5), guide = "none") +
  theme_light() +
  ggtitle("All Species Richness") +
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


# Create a map of field surveys
data.projected <- st_as_sf(x = all.data, 
                           coords = c("Longitude", "Latitude"),
                           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Weighted based on survey effort
data.projected$AllRich <- data.projected$AllRich / data.projected$SurveyDays
data.projected$AllRich <- data.projected$AllRich / max(data.projected$AllRich)

occ.plot <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
  scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = data.projected, aes(colour = AllRich, fill = AllRich, size = 1)) +
  scale_size(guide = "none") +
  scale_fill_gradientn(name = paste0("Relative\nRichness"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "none") +
  ggtitle("All Species Richness") +
  theme_light() +
  theme(axis.title = element_text(size=24),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24),
        title = element_text(size=24),
        legend.text = element_text(size=24),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c(0.25, 0.15))
###############
# Rarefaction #
###############
# # Rarefy the data for Bombus
# bombus.data <- species.wide[paste0(species.wide$Project, species.wide$SiteID) %in% paste0(site.lookup$Project, site.lookup$SiteID), ]
# bombus.data <- bombus.data[bombus.data$Project %in% c("AEP", "ACA", "ANBC"), ]
# # Remove sites with fewer than 10 specimens
# bombus.data <- bombus.data[rowSums(bombus.data[, -c(1:6)]) >= 10, ]
# raremax <- min(rowSums(bombus.data[, -c(1:6)]))
# a <- rarefy(bombus.data[, -c(1:6)], sample = raremax)
# 
# glm(a ~ bombus.data$Latitude, family = "binomial")
# 
# # Rarefy the data for non-Bombus
# # Rarefy the data
# raremax <- min(rowSums(species.wide[, -c(1:6)]))
# rarefy(species.wide[, -c(1:6)], sample = 20)
# 
# head(species.wide)
# 
# 
# 
# data(BCI)
# S <- specnumber(BCI) # observed number of species
# (raremax <- min(rowSums(BCI)))
# Srare <- rarefy(BCI, raremax)
# plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
# abline(0, 1)
# rarecurve(BCI, step = 20, sample = raremax, col = "blue", cex = 0.6)
