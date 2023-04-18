#
# Title: Creation of diversity models.
# Created: March 27th, 2023
# Last Updated: April 18th, 2023
# Author: Brandon Allen
# Objectives: Develop diversity based models for the two groups (Bombus, Others)
# Keywords: Notes, Species Richness, Functional Diversity
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
####################
# Species Richness #
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(corrplot)
library(ggpubr)
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

species.wide$UID <- paste(species.wide$Project, species.wide$SiteID, species.wide$Year, sep = "_")
rownames(species.wide) <- species.wide$UID
species.wide <- species.wide[site.lookup$UID, ]
veg.data <- veg.data[site.lookup$UID, ]
colnames(veg.data)[1] <- "UID"
rownames(climate.data) <- climate.data$UID
climate.data <- climate.data[site.lookup$UID,]

site.richness <- species.wide[, c(78, 7:77)]
site.richness[, -1] <- ifelse(site.richness[, -1] >= 1, 1, 0)
site.richness$Richness <- rowSums(site.richness[, species.lookup$AnalysisName])

model.data <- merge.data.frame(climate.data, site.lookup[, -c(2:6)], by = "UID") # Merge site and climate
model.data <- merge.data.frame(model.data, veg.data, by = "UID") # Add vegetation
model.data <- merge.data.frame(model.data, site.richness[, c("UID", "Richness")], by = "UID") # Add unscaled richness
model.data <- merge.data.frame(model.data, species.wide[, -c(1:6)], by = "UID") # Add species abundance
model.data$SurveyDays2 <- model.data$SurveyDays * model.data$SurveyDays

# Assess redundant variables
corr <- cor(model.data[, c(5:13, 15:27)])
corrplot(corr)

#
# Richness model
#

all.data <- model.data[model.data$Project %in% c("ACA", "ANBC"), ]

all.model <- glm(Richness ~ DecidMixed + PineSpruce + TreedBogFenSwamp + Wetland + GrassShrub + 
                   UrbInd + Crop + Pasture + ForestryYoung + ForestryOld +
                   SurveyDays,  
                   family = "poisson",
                   data = all.data, 
                   maxit = 250)

plot(poisson()$linkinv(all.model$coefficients)[-1])
names(all.model$coefficients)[-1]

# Check general model fit
plot(predict(all.model, type = "response") ~ all.data$Richness)
cor(predict(all.model, type = "response"), all.data$Richness)

# Load the kgrid landcover layer
load("data/processed/landcover/kgrid-processed.Rdata")
load("data/base/mapping/provincial-boundary.Rdata")
load("data/base/mapping/kgrid_mapping.Rdata")

# Create predictions for the two models
kgrid.cur$Richness <- predict(all.model, newdata = cbind(kgrid.cur, data.frame(SurveyDays = 0)), type = "response")
kgrid.cur$Richness <- ifelse(kgrid.cur$Richness >= quantile(kgrid.cur$Richness, 0.999), quantile(kgrid.cur$Richness, 0.999), kgrid.cur$Richness) # Truncate
kgrid.ref$Richness <- predict(all.model, newdata = cbind(kgrid.ref, data.frame(SurveyDays = 0,
                                                                               UrbInd = 0,
                                                                               Crop = 0,
                                                                               Pasture = 0,
                                                                               ForestryYoung = 0,
                                                                               ForestryOld = 0)), type = "response")
kgrid.ref$Richness <- ifelse(kgrid.ref$Richness >= quantile(kgrid.ref$Richness, 0.999), quantile(kgrid.ref$Richness, 0.999), kgrid.ref$Richness) # Truncate


# Align with the raster kgrid
kgrid <- kgrid[kgrid.cur$LinkID, ]
kgrid$RichnessCurrent <- kgrid.cur$Richness
kgrid$RichnessReference <- kgrid.ref$Richness
kgrid$RichnessDifference <- kgrid.cur$Richness - kgrid.ref$Richness

# Current richness
current.rich <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
  scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_raster(data = kgrid , aes(x = x, y = y, fill = RichnessCurrent)) +
  scale_fill_gradientn(name = paste0("Richness"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,25), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,25), guide = "none") +
  theme_light() +
  ggtitle("Current") +
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

# Reference richness
reference.rich <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
  scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_raster(data = kgrid , aes(x = x, y = y, fill = RichnessReference)) +
  scale_fill_gradientn(name = paste0("Richness"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,25), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,25), guide = "none") +
  theme_light() +
  ggtitle("Reference") +
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

# Difference richness
difference.rich <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(rep("grey", 6), 0.2)) +
  scale_color_manual(values =  alpha(rep("grey", 6), 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_raster(data = kgrid , aes(x = x, y = y, fill = RichnessDifference)) +
  scale_fill_gradientn(name = paste0("Difference"), colors = (met.brewer(name = "OKeeffe1", n = 100, type = "continuous")), limits = c(-15,15), guide = "colourbar") +
  scale_color_gradientn(colors = (met.brewer(name = "OKeeffe1", n = 100, type = "continuous")), limits = c(-15,15), guide = "none") +  theme_light() +
  ggtitle("Difference") +
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

ggsave(filename = paste0("results/figures/diversity/species-richness.jpeg"), 
       plot = ggarrange(current.rich,
                        reference.rich, 
                        difference.rich, ncol = 3, nrow = 1),
       height = 900,
       width = 1800, 
       dpi = 72,
       quality = 100,
       units = "px")

#
# Coefficient plot
#

main.coef <- data.frame(Coef = names(all.model$coefficients),
                        Mean = poisson()$linkinv(all.model$coefficients))
main.coef <- main.coef[-c(1, nrow(main.coef)), ]
main.coef$Coef <- c("Deciduous/Mixedwood", "Pine/Spruce", "Treed Bog/Fen/Swamp",
                    "Wetland", "Grass/Shrub", "Urban/Industrial", "Crop", "Pasture",
                    "Forestry (Young)", "Forestry (Old)")
main.coef$Coef <- factor(main.coef$Coef, levels = main.coef$Coef)

coef.plot <- ggplot(data = main.coef, aes(x = Coef, y = Mean, fill = Coef)) +
  geom_bar(stat="identity", fill = met.brewer("Cross", n = 10, type = "continuous")) +
  labs(x = "Vegetation", y = "Richness") +
  theme_light() +
  theme(axis.title = element_text(size=16),
        axis.text.x = element_text(size=16, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_text(size=16),
        title = element_text(size=12),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = paste0("results/figures/diversity/richness-coefficients.jpeg"), 
       plot = coef.plot,
       height = 600,
       width = 800, 
       dpi = 72,
       quality = 100,
       units = "px")

rm(list=ls())
gc()

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

########################
# Functional Diversity #
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(corrplot)
library(ggplot2)
require(ggnewscale)
library(MetBrewer)

# Load data
load("data/processed/landcover/veg-hf_800m_wide_simplified.Rdata")
load("data/processed/species/species-wide-form.Rdata")
load("data/base/sites/anbc-climate-summaries.Rdata")
site.lookup <- read.csv("data/lookup/site-lookup-all-surveys.csv")
species.lookup <- read.csv("data/lookup/species-lookup.csv")

# Filter for the projects of interest
site.lookup <- site.lookup[!is.na(site.lookup$SurveyDays), ] # Remove sites with missing survey effort
site.lookup <- site.lookup[site.lookup$Project %in% c("AEP", "ANBC", "ACA"), ] # Keep only the data with surveys

# Standardize the data sets
species.wide$UID <- paste(species.wide$Project, species.wide$SiteID, species.wide$Year, sep = "_")
rownames(species.wide) <- species.wide$UID
species.wide <- species.wide[site.lookup$UID, ]
veg.data <- veg.data[site.lookup$UID, ]
colnames(veg.data)[1] <- "UID"
rownames(climate.data) <- climate.data$UID
climate.data <- climate.data[site.lookup$UID,]

# Remove species with no detections
species.wide <- species.wide[, !c(colnames(species.wide) %in% colnames(species.wide)[7:77][colSums(species.wide[, 7:77]) == 0])]

# Filter for species of interest
species.lookup <- species.lookup[species.lookup$Species %in% colnames(species.wide)[7:(ncol(species.wide) - 1)], ]

# Merge into a single data set
model.data <- merge.data.frame(climate.data, site.lookup[, -c(2:6)], by = "UID") # Merge site and climate
model.data <- merge.data.frame(model.data, veg.data, by = "UID") # Add vegetation
model.data <- merge.data.frame(model.data, species.wide[, -c(1:6)], by = "UID") # Add species abundance

# Evaluate the traits the distribution of traits
table(species.lookup$Nesting.Behavior) # Suitable
nrow(species.lookup) - sum(table(species.lookup$Nesting.Behavior))

table(species.lookup$Voltinism) #Unstuitable
nrow(species.lookup) - sum(table(species.lookup$Voltinism))

table(species.lookup$Sociality) # Suitable
nrow(species.lookup) - sum(table(species.lookup$Sociality))

table(species.lookup$Foraging.Behavior) # Unsuitable
nrow(species.lookup) - sum(table(species.lookup$Foraging.Behavior))

table(species.lookup$Tongue.Length) # Suitable
nrow(species.lookup) - sum(table(species.lookup$Tongue.Length))

# Lets subset the species with traits of interest
species.lookup <- species.lookup[species.lookup$Nesting.Behavior %in% c("Cavity", "Ground"), ]
species.lookup <- species.lookup[species.lookup$Sociality %in% c("Eusocial", "Primitively Eusocial", "Solitary"), ]
species.lookup$Sociality[species.lookup$Sociality == "Primitively Eusocial"] <- "Eusocial"
table(species.lookup$Nesting.Behavior, species.lookup$Sociality, species.lookup$Tongue.Length)

# Further subset the species list
model.data <- model.data[, c(rep(TRUE, 29), colnames(model.data)[30:99] %in% species.lookup$Species)]

# Now we can run the fourth corner analysis.
# Need to chat with Ermias about this
