#
# Title: Cleaning of species data and creation of range maps
# Created: August 30th, 2022
# Last Updated: October 18th, 2022
# Author: Brandon Allen
# Objectives: Clean and create basic summaries for the species data
# Keywords: Notes, Species data, Range maps, Use availability, Species Models
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
################
# Species data # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(readxl)
library(sf)

# Load data
anbc.data <- read_excel("data/base/species/ANBC_monitoringdata_2018.xlsx", sheet = "combined")
aep.data <- read.csv("data/base/species/AEP_monitoringdata_2018.csv")
spp.lookup <- read.csv("data/lookup/species-lookup.csv")
site.lookup <- read.csv("data/lookup/site-lookup.csv")

# Filter species list to those we wish to analyze
spp.lookup <- spp.lookup[spp.lookup$Analyze, ]

# Create a single species column for the aep data
aep.data$Species <- paste(aep.data$genus, aep.data$specificEpithet)

# Filter entries to the species of interest
anbc.data <- anbc.data[anbc.data$Species %in% spp.lookup$Species, ]
aep.data <- aep.data[aep.data$Species %in% spp.lookup$Species, ]

# Standardize columns and merge data sets
anbc.data <- anbc.data[, c("Project", "SiteID", "Species")]
aep.data <- aep.data[, c("Project", "SiteID", "Species")]

# Create a merged data site
spp.data <- rbind.data.frame(anbc.data,
                             aep.data)

rm(anbc.data, aep.data)

# Review specimens
table(spp.data$Species)

# Create abundance for each site
spp.data <- table(spp.data$SiteID, spp.data$Species)
class(spp.data) <- "matrix"
spp.data <- as.data.frame(spp.data)
spp.data$SiteID <- rownames(spp.data)

# Merge with site lookup and create a projection
spp.data <- merge.data.frame(site.lookup, spp.data, by = "SiteID")

# Save the results
save(spp.data, file = "data/processed/species/species-abundance.Rdata")

##############
# Range Maps #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(ggnewscale)
library(MetBrewer)
library(sf)

# Load data
boundary.in <- read_sf("data/base/gis/NRNAMEdissolve.shp")
load("data/processed/species/species-abundance.Rdata")
site.lookup <- read.csv("data/lookup/site-lookup.csv")
spp.lookup <- read.csv("data/lookup/species-lookup.csv")

# Standardize to presence/absence
spp.data[, -c(1:4)][spp.data[, -c(1:4)] > 1] <- 1

# Loop through each unique species
for (species in colnames(spp.data)[-c(1:4)]) {
  
  # Create directory to store the results
  dir.create(paste0("results/figures/species/", species))
  
  # If the species is in the genus Bombus, use all sites. Otherwise, only report on the ANBC surveys
  if(spp.lookup[spp.lookup$Species == species, "Genus"] == "Bombus") {
    
    spp.temp <- spp.data
    
  } else {
    
    spp.temp <- spp.data[spp.data$SiteID %in% site.lookup[site.lookup$Project == "ANBC", "SiteID"], ]
    
  }
  # Create the spatial projection
  data.projected <- st_as_sf(x = spp.temp, 
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Modify the data frame to map only single species
  data.projected$Detections <- ifelse(as.numeric(as.data.frame(data.projected[, species])[,1]) == 1, "Present", "Absent")
  
  png(filename = paste0("results/figures/species/", species, "/range-map.png"),
      height = 2400,
      width = 1600, 
      res = 300)
  
  print(ggplot() + 
          geom_sf(data = boundary.in, mapping = aes(colour = "#000000", fill = c(alpha(c("#c969a1"), c(0.2)),
                                                                                 alpha(c("#ee8577"), c(0.2)),
                                                                                 alpha(c("#ffbb44"), c(0.2)),
                                                                                 alpha(c("#859b6c"), c(0.2)),
                                                                                 alpha(c("#62929a"), c(0.2)),
                                                                                 alpha(c("#004f63"), c(0.2)))), show.legend = FALSE) +
          scale_color_manual(values = c("#000000")) +
          scale_fill_manual(values = c(alpha(c("#c969a1"), c(0.2)),
                                       alpha(c("#ee8577"), c(0.2)),
                                       alpha(c("#ffbb44"), c(0.2)),
                                       alpha(c("#859b6c"), c(0.2)),
                                       alpha(c("#62929a"), c(0.2)),
                                       alpha(c("#004f63"), c(0.2)))) +
          new_scale_color() +
          new_scale_fill() +
          geom_sf(data = data.projected, aes(colour = Detections, fill = Detections, shape = Detections)) +
          scale_shape_manual(values = c(21,21)) +
          scale_color_manual(values = c("#000000", "#000000")) +
          scale_fill_manual(values = alpha(c("#72bcd5", "#ef8a47"), c(1, 1))) +
          ggtitle(species) + 
          theme_light() +
          theme(axis.title = element_text(size=16),
                axis.text.x = element_text(size=16),
                axis.text.y = element_text(size=16),
                title = element_text(size=16),
                legend.text = element_text(size=14),
                legend.title = element_blank(),
                legend.background = element_rect(fill='transparent'),
                axis.line = element_line(colour = "black"),
                panel.border = element_rect(colour = "black", fill=NA, size=1),
                legend.position = c(0.25, 0.17)))
  
  dev.off()
  
}

rm(list=ls())
gc()

####################
# Use availability # 
####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##################
# Habitat models # This will need a complete overhaul!!
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(pROC)

# Load species and landcover data
load("data/processed/species/species-abundance.Rdata")
load("data/processed/landcover/veg-hf_2018_800m_wide_simplified.Rdata")

# Standardize to presence/absence
spp.data[, -c(1:4)][spp.data[, -c(1:4)] > 1] <- 1

# Filter the species and landcover data to include only sites outside the Grasslands
site.exclude <- c(11, 26, 27, 76, 78, 94, 95, 102, 103, 114, 116)
spp.data <- spp.data[!(spp.data$SiteID %in% site.exclude), ]
veg.data <- veg.data[!(veg.data$SiteID %in% site.exclude), ]

# Merge the species data with the landcover data
model.data <- merge.data.frame(spp.data, veg.data, by = "SiteID")
rm(spp.data, veg.data, site.exclude)

# Create basic habitat model for Bombus Mixtus
model.data$pa <- model.data$`Bombus mixtus`
spp.model <- glm(pa ~ Deciduous + Mixedwood + Pine + Spruce + TreedBogFen + Swamp + GrassShrub + Wetland + UrbInd + SoftLin + HardLin + Crop + Pasture + Forestry + Latitude + Longitude, 
                 family = "binomial",
                 data = model.data, 
                 maxit = 250)

# Determine basic model fit
auc(model.data$pa, plogis(predict(spp.model))) # Reasonable fit

# Create a simplified landcover
veg.lookup <- read.csv("data/lookup/lookup-veg-hf-age-v2020.csv")
unique.veg <- unique(veg.lookup$UseAvail_BEA)
unique.veg <- unique.veg[!(unique.veg %in% "EXCLUDE")]

load("data/base/landcover/veghf_w2w_2018_wide_water.RData")
veg.cur <- as.data.frame(as.matrix(dd_2018$veg_current))

veg.data <- data.frame(LinkID = rownames(veg.cur))

for(veg in unique.veg) {
  
  # Identify columns of interest
  veg.id <- veg.lookup[veg.lookup$UseAvail_BEA %in% veg, "ID"]
  
  # Check if row sum will fail
  if(length(veg.id) == 1) {
    
    veg.combined <- data.frame(Veg = veg.cur[, veg.id])
    
  } else {
    
    veg.combined <- data.frame(Veg = rowSums(veg.cur[, colnames(veg.cur) %in% veg.id]))
    
  }
  
  colnames(veg.combined)[1] <- veg
  
  veg.data <- cbind.data.frame(veg.data,
                               veg.combined)
  
  rm(veg.combined)
  
}

rm(dd_2018, lts, ltv)

#
# Create a spatial prediction
#
load("data/base/landcover/kgrid_table_km.Rdata")

kgrid <- data.frame(LinkID = kgrid$Row_Col,
                    Latitude = kgrid$POINT_X,
                    Longitude = kgrid$POINT_Y,
                    NaturalRegion = kgrid$NRNAME)

# Remove grassland
kgrid <- kgrid[kgrid$NaturalRegion != "Grassland", ]

kgrid <- merge.data.frame(kgrid, veg.data, by = "LinkID")

kgrid$Abundance <- plogis(predict(spp.model, newdata = kgrid))

# Load kgrid and overlap region
kgrid.map <- read_sf(dsn = "data/base/gis/ABMI.gdb", layer = "Grid_1KM")
kgrid.map <- kgrid.map[kgrid.map$GRID_LABEL %in% kgrid$LinkID, ]
rownames(kgrid) <- kgrid$LinkID
kgrid <- kgrid[kgrid.map$GRID_LABEL, ]
kgrid.map$Cur <- kgrid$Abundance

# Load the shapefile for the provincial boundary
library(tidyverse)
boundary.in <- read_sf("data/base/gis/NRNAMEdissolve.shp")

# Combine all natural regions
boundary.in <- boundary.in %>% 
  summarise(Area = sum(Area))

# Make a plain background
boundary.in$Boundary <- "Boundary"

#
# Visualize
#
# Current Abundance
png(file = paste0("test.png"),
    width = 1800,
    height = 2400, 
    res = 300)

cur.map <- ggplot() +
  geom_sf(data = boundary.in, show.legend = FALSE) +
  geom_sf(data = kgrid.map, aes(colour = Cur, fill = Cur, shape = "22"), show.legend = TRUE) +
  scale_shape_manual(values = c(22), guide = "none") +
  scale_fill_gradientn(name = paste0("Relative\nAbundance"), colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "colourbar") +
  scale_color_gradientn(colors = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous")), limits = c(0,1), guide = "none") +
  theme_light() +
  theme_abmi(font = "Montserrat") +
  theme(axis.title = element_text(size=36),
        axis.text.x = element_text(size=36),
        axis.text.y = element_text(size=36),
        title = element_text(size=36),
        legend.text = element_text(size=36),
        legend.title = element_text(size=36, lineheight = 0.25, vjust = -6),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c(0.20, 0.17))

print(cur.map)

dev.off()
