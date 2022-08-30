#
# Title: Cleaning of species data and creation of range maps
# Created: August 30th, 2022
# Last Updated: August 30th, 2022
# Author: Brandon Allen
# Objectives: Clean and create basic summaries for the species data
# Keywords: Notes, Species data, Range maps
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
# 3) Some accession values don't have IDs.
# 4) Some species with IDs don't have lat long coordinates or other site identifiers.
################
# Species data # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(ggnewscale)
library(MetBrewer)
library(readxl)
library(sf)

# Load data
spp.data <- read_excel("data/base/species/ANBC2018_monitoringdata.xlsx", sheet = "combined")
spp.lookup <- read_excel("data/base/species/ANBC2018_monitoringdata.xlsx", sheet = "vlookup values")

# Filter the lookup table to include only genus/species IDs
spp.lookup <- spp.lookup[!is.na(spp.lookup$Genus), ]

# Filter entries to the species of interest
spp.data <- spp.data[spp.data$Species %in% spp.lookup$Species, ]

# Review specimens
table(spp.data$Species)

# Create presence/absence for each site
site.lookup <- data.frame(ID = NA,
                          Lat = spp.data$`Decimal Latitude`,
                          Long = spp.data$`Decimal Longtitude`,
                          Lat_Long = paste0(spp.data$`Decimal Latitude`, "_", spp.data$`Decimal Longtitude`))
site.lookup <- site.lookup[!duplicated(site.lookup), ]
site.lookup <- site.lookup[!is.na(site.lookup$Lat), ]
site.lookup$ID <- 1:nrow(site.lookup)

spp.data$Lat_Long <- paste0(spp.data$`Decimal Latitude`, "_", spp.data$`Decimal Longtitude`)
spp.data$ID <- site.lookup$ID[match(spp.data$Lat_Long, site.lookup$Lat_Long)]

spp.data <- table(spp.data$ID, spp.data$Species)
class(spp.data) <- "matrix"
spp.data <- ifelse(spp.data > 0, 1, 0)
spp.data <- as.data.frame(spp.data)
spp.data$ID <- rownames(spp.data)

# Add lat long
spp.data <- merge.data.frame(site.lookup[, 1:3], spp.data, by = "ID")

# Visualization
# Convert into shapefile of vorrect projection
data.projected <- st_as_sf(x = spp.data, 
                        coords = c("Long", "Lat"),
                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load the shapefile for the natural subregions
boundary.in <- read_sf("data/base/gis/NRNAMEdissolve.shp")

# Loop through each unique species

for (species in colnames(spp.data)[-c(1:3)]) {
  
  ##############
  # Range Maps #
  ##############
  
  # Create directory to store the results
  dir.create(paste0("results/figures/species/", species))

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
