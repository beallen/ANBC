#
# Title: Cleaning of species data and creation of range maps
# Created: March 21st, 2023
# Last Updated: March 21st, 2023
# Author: Brandon Allen
# Objectives: Clean and create the basic summaries for the set of species we are reporting on
# Keywords: Notes, Species data, Range maps
#

#########
# Notes # ANBC
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) The ANBC sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
# 3) Some accession values don't have IDs.
# 4) Some species with IDs don't have lat long coordinates or other site identifiers.
# 5) Need to resolve site matching
# 6) Because there is some missing data in the ANBC sites, we have incorrect survey effort for a few. Exclude for now (SurveyEffort = NA)
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

# Load the species and site lookup table
spp.lookup <- read.csv("data/lookup/species-lookup.csv")
spp.lookup <- spp.lookup[spp.lookup$Reporting, ]
site.lookup <- read.csv("data/lookup/site-lookup.csv")

#############
# ANBC Data #
#############

# Load data
anbc.data <- read_excel("data/base/species/ANBC_monitoringdata_2018.xlsx", sheet = "combined")

# Filter entries to the species of interest
anbc.data <- anbc.data[anbc.data$Species %in% spp.lookup$Species, ]

# Standardize
anbc.data <- anbc.data[, c("Project", "SiteID", "Species")]
anbc.data$Year <- 2018
anbc.data <- merge.data.frame(anbc.data, site.lookup[site.lookup$Project == "ANBC", c("SiteID", "Latitude", "Longitude")], by = "SiteID")

# Correct the name for Megachile melanophaea
anbc.data$Species[anbc.data$Species == "Megachile melanophea"] <- "Megachile melanophaea"

############
# AEP Data #
############

# Load data
aep.data <- read.csv("data/base/species/AEP_monitoringdata_2018.csv")

# Filter entries to the species of interest
aep.data$Species <- paste(aep.data$genus, aep.data$specificEpithet)
aep.data <- aep.data[aep.data$Species %in% spp.lookup$Species, ]

# Standardize
aep.data <- aep.data[, c("Project", "SiteID", "Species", "year")]
colnames(aep.data) <- c("Project", "SiteID", "Species", "Year")
aep.data <- merge.data.frame(aep.data, site.lookup[site.lookup$Project == "AEP", c("SiteID", "Latitude", "Longitude")], by = "SiteID")

############
# ACA Data #
############

# Load data
aca.data <- read.csv("data/base/species/ACA_monitoringdata_2019.csv")

# Filter entries to the species of interest
aca.data <- aca.data[aca.data$species %in% spp.lookup$Species, ]

# Standardize
aca.data <- aca.data[, c("Project", "SiteID", "species", "year")]
colnames(aca.data) <- c("Project", "SiteID", "Species", "Year")
aca.data <- merge.data.frame(aca.data, site.lookup[site.lookup$Project == "ACA", c("SiteID", "Latitude", "Longitude")], by = "SiteID")

####################
# iNaturalist Data # Because this is observation data, there will be lots of locations. 
#################### Just filter and use the coordinates stored in the raw data. 

# Load data
iNaturalist.data <- read.csv("data/base/species/iNaturalist_monitoringdata.csv")

# Filter entries to the species of interest
iNaturalist.data <- iNaturalist.data[iNaturalist.data$stateProvince == "Alberta", ]
iNaturalist.data <- iNaturalist.data[iNaturalist.data$species %in% spp.lookup$Species, ]
iNaturalist.data <- iNaturalist.data[iNaturalist.data$year >= 2010, ] # We only have footprint information starting in 2010.
iNaturalist.data <- iNaturalist.data[!is.na(iNaturalist.data$decimalLatitude), ] # Remove missing coordinates
iNaturalist.data$coordinateUncertaintyInMeters[is.na(iNaturalist.data$coordinateUncertaintyInMeters)] <- 1 # If we don't have coordinate uncertainty, keep. We don't have this information for the other data sets anyways. 
iNaturalist.data <- iNaturalist.data[iNaturalist.data$coordinateUncertaintyInMeters <= 20, ] # As we are using a 800m buffer, we will accept uncertainty within 20m 

# There are some event dates with multiple species detected. However, these are unlikely to be complete surveys and should
# be treated as separate survey events.

# Create unique site IDs
iNaturalist.data$Project <- "iNaturalist"
iNaturalist.data$SiteID <- paste0(iNaturalist.data$decimalLatitude, 
                                 iNaturalist.data$decimalLongitude, 
                                 iNaturalist.data$year)

iNaturalist.site.lookup <- data.frame(SiteID = unique(paste0(iNaturalist.data$decimalLatitude, 
                                                            iNaturalist.data$decimalLongitude, 
                                                            iNaturalist.data$year)))
iNaturalist.site.lookup$Site <- 1:nrow(iNaturalist.site.lookup)
iNaturalist.data$SiteID <- iNaturalist.site.lookup$Site[match(iNaturalist.data$SiteID, iNaturalist.site.lookup$SiteID)]

# Standardize
iNaturalist.data <- iNaturalist.data[, c("Project", "SiteID", "species", "year", "decimalLatitude", "decimalLongitude")]
colnames(iNaturalist.data) <- c("Project", "SiteID", "Species", "Year","Latitude", "Longitude")

###################
# Strickland Data # Because this is observation data, there will be lots of locations. 
###################Just filter and use the coordinates stored in the raw data. 

# Load data
strickland.data <- read.csv("data/base/species/Strickland_observationdata.csv")

# Filter entries to the species of interest
strickland.data <- strickland.data[strickland.data$stateProvince == "Alberta", ]
strickland.data <- strickland.data[strickland.data$species %in% spp.lookup$Species, ]
strickland.data <- strickland.data[strickland.data$year >= 2010, ] # we only have footprint information starting in 2010. 
strickland.data <- strickland.data[!is.na(strickland.data$decimalLatitude), ] # Remove missing coordinates

# There are some event dates with multiple species detected. However, these are unlikely to be complete surveys and should
# be treated as separate survey events.

# Create unique site IDs
strickland.data$Project <- "Strickland"
strickland.data$SiteID <- paste0(strickland.data$decimalLatitude, 
                                        strickland.data$decimalLongitude, 
                                        strickland.data$year)

strickland.site.lookup <- data.frame(SiteID = unique(paste0(strickland.data$decimalLatitude, 
                                                     strickland.data$decimalLongitude, 
                                                     strickland.data$year)))
strickland.site.lookup$Site <- 1:nrow(strickland.site.lookup)
strickland.data$SiteID <- strickland.site.lookup$Site[match(strickland.data$SiteID, strickland.site.lookup$SiteID)]

# Standardize
strickland.data <- strickland.data[, c("Project", "SiteID", "species", "year","decimalLatitude", "decimalLongitude")]
colnames(strickland.data) <- c("Project", "SiteID", "Species","Year", "Latitude", "Longitude")

###########
# Combine #
###########

spp.data <- rbind.data.frame(anbc.data,
                             aep.data)

spp.data <- rbind.data.frame(spp.data,
                             aca.data)

spp.data <- rbind.data.frame(spp.data,
                             iNaturalist.data)

spp.data <- rbind.data.frame(spp.data,
                             strickland.data)

# Review specimens
table(spp.data$Species)

# Save the results
save(spp.data, file = "data/processed/species/species-long-form.Rdata")

rm(list=ls())

##############
# Range Maps # Note: All bee species are identified at the ANBC sites. AEP and ACA only identified bumble bees. Remaining are observation data
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
load("data/base/mapping/provincial-boundary.Rdata")
load("data/processed/species/species-long-form.Rdata")
spp.lookup <- read.csv("data/lookup/species-lookup.csv")
spp.lookup <- spp.lookup[spp.lookup$Reporting, ]

spp.lookup$Species[!(spp.lookup$Species %in% unique(spp.data$Species))]

##########################
# Field Survey Locations #
##########################

site.locations <- spp.data[!duplicated(spp.data[, c("Project", "SiteID", "Year", "Latitude", "Longitude")]), ]
write.csv(site.locations[, c("Project", "SiteID", "Year", "Latitude", "Longitude")], file = "data/lookup/site-list-GIS-extraction.csv", row.names = FALSE)

# Create a map of field surveys
data.projected <- st_as_sf(x = spp.data, 
                           coords = c("Longitude", "Latitude"),
                           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

occ.plot <- ggplot() + 
  geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
  scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
  scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
  new_scale_color() +
  new_scale_fill() +
  geom_sf(data = data.projected, aes(colour = Project, fill = Project, shape = Project, size = 1)) +
  scale_size(guide = "none") +
  scale_shape_manual(values = c(21:25)) +
  scale_color_manual(values = c("#DD5129", "#427083", "#299693", "#70B278", "#FAB255")) +
  scale_fill_manual(values = alpha(c("#DD5129", "#427083", "#299693", "#70B278", "#FAB255"), 
                                   c(0.9, 0.9, 0.9, 0.9, 0.9))) +
  theme_light() +
  guides(color = guide_legend(override.aes = list(size = 10))) +
  theme(axis.title = element_text(size=24),
        axis.text.x = element_text(size=24),
        axis.text.y = element_text(size=24),
        title = element_text(size=24),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.position = c(0.25, 0.15))

ggsave(filename = paste0("results/figures/landcover/survey-locations.png"),
       plot = occ.plot,
       height = 1200,
       width = 800,
       dpi = 72,
       units = "px")

######################
# Species occurrence #
######################

# Create the wide format data

# Presence/absence surveys
spp.pa <- spp.data[spp.data$Project %in% c("ACA", "AEP", "ANBC"), ]
spp.pa <- table(spp.pa$SiteID, spp.pa$Species, spp.pa$Project)

aep.pa <- spp.pa[, , "AEP"]
class(aep.pa) <- "matrix"
aep.pa <- as.data.frame(aep.pa)
aep.pa$SiteID <- rownames(aep.pa)
aep.pa <- merge.data.frame(site.locations[site.locations$Project == "AEP", ], aep.pa, by = "SiteID")

aca.pa <- spp.pa[, , "ACA"]
class(aca.pa) <- "matrix"
aca.pa <- as.data.frame(aca.pa)
aca.pa$SiteID <- rownames(aca.pa)
aca.pa <- merge.data.frame(site.locations[site.locations$Project == "ACA", ], aca.pa, by = "SiteID")

anbc.pa <- spp.pa[, , "ANBC"]
class(anbc.pa) <- "matrix"
anbc.pa <- as.data.frame(anbc.pa)
anbc.pa$SiteID <- rownames(anbc.pa)
anbc.pa <- merge.data.frame(site.locations[site.locations$Project == "ANBC", ], anbc.pa, by = "SiteID")

# Presence only
spp.presence <- spp.data[spp.data$Project %in% c("iNaturalist", "Strickland"), ]
spp.presence <- table(spp.presence$SiteID, spp.presence$Species, spp.presence$Project)

inaturalist.presence <- spp.presence[, , "iNaturalist"]
class(inaturalist.presence) <- "matrix"
inaturalist.presence <- as.data.frame(inaturalist.presence)
missing.columns <- colnames(anbc.pa)[-c(1:6)][!(colnames(anbc.pa)[-c(1:6)] %in% colnames(inaturalist.presence))]
missing.columns <- matrix(data = 0, nrow = nrow(inaturalist.presence), ncol = length(missing.columns), 
                          dimnames = list(rownames(inaturalist.presence), missing.columns))
inaturalist.presence <- cbind(inaturalist.presence, missing.columns)
inaturalist.presence <- inaturalist.presence[, colnames(anbc.pa)[-c(1:6)]]
inaturalist.presence$SiteID <- rownames(inaturalist.presence)
inaturalist.presence <- merge.data.frame(site.locations[site.locations$Project == "iNaturalist", ], inaturalist.presence, by = "SiteID")

strickland.presence <- spp.presence[, , "Strickland"]
class(strickland.presence) <- "matrix"
strickland.presence <- as.data.frame(strickland.presence)
missing.columns <- colnames(anbc.pa)[-c(1:6)][!(colnames(anbc.pa)[-c(1:6)] %in% colnames(strickland.presence))]
missing.columns <- matrix(data = 0, nrow = nrow(strickland.presence), ncol = length(missing.columns), 
                          dimnames = list(rownames(strickland.presence), missing.columns))
strickland.presence <- cbind(strickland.presence, missing.columns)
strickland.presence <- strickland.presence[, colnames(anbc.pa)[-c(1:6)]]
strickland.presence$SiteID <- rownames(strickland.presence)
strickland.presence <- merge.data.frame(site.locations[site.locations$Project == "Strickland", ], strickland.presence, by = "SiteID")

species.wide <- rbind.data.frame(aep.pa, aca.pa)
species.wide <- rbind.data.frame(species.wide, anbc.pa)
species.wide <- rbind.data.frame(species.wide, inaturalist.presence)
species.wide <- rbind.data.frame(species.wide, strickland.presence)

save(species.wide, file = "data/processed/species/species-wide-form.Rdata")

#################
# Visualization #
#################

# Loop through each unique species
for (species in spp.lookup$Species) {
  
  # Create directory to store the results
  if(!dir.exists(paste0("results/figures/species/", species))) {
    
    dir.create(paste0("results/figures/species/", species))
    
  }
  
  # Filter the presence only component of the data
  spp.map <- species.wide[!(species.wide$Project %in% c("iNaturalist", "Strickland") & species.wide[, species] == 0), ]

  # If the species is in the genus Bombus, we can use the Presence/Absence of ACA, AEP, ANBC plus presence only
  # Otherwise, we need to exclude the presence/absence of AEP as they only identified Bombus
  
  if(spp.lookup[spp.lookup$Species == species, "Genus"] != "Bombus") {
    
    spp.map <- spp.map[!(spp.map$Project %in% "AEP"), ]
    
  }
  
  # Create the spatial projection
  spp.map <- st_as_sf(x = spp.map, 
                             coords = c("Longitude", "Latitude"),
                             crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  # Modify the data frame to map only single species
  spp.map$Detections <- ifelse(as.numeric(as.data.frame(spp.map[, species])[,1]) >= 1, "Detected", "Undetected")

  occ.plot <- ggplot() + 
    geom_sf(data = province.shapefile, aes(color = NRNAME, fill = NRNAME), show.legend = FALSE) +
    scale_fill_manual(values =  alpha(province.shapefile$Color, 0.2)) +
    scale_color_manual(values =  alpha(province.shapefile$Color, 0.1)) +
    new_scale_color() +
    new_scale_fill() +
    geom_sf(data = spp.map, aes(colour = Detections, fill = Detections, shape = Detections, size = 1)) +
    scale_size(guide = "none") +
    scale_shape_manual(values = c(21,4)) +
    scale_color_manual(values = c("#122451", "#122451")) +
    scale_fill_manual(values = alpha(c("#122451", "#122451"), c(0.9, 0))) +
    theme_light() +
    guides(color = guide_legend(override.aes = list(size = 10))) +
    theme(axis.title = element_text(size=24),
          axis.text.x = element_text(size=24),
          axis.text.y = element_text(size=24),
          title = element_text(size=24),
          legend.text = element_text(size=24),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          legend.position = c(0.25, 0.15))

ggsave(filename = paste0("results/figures/species/", species, "/range-map.png"),
       plot = occ.plot,
       height = 1200,
       width = 800,
       dpi = 72,
       units = "px")
  
}


