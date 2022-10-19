#
# Title: Extraction of landcover information for the ANBC
# Created: April 14th, 2022
# Last Updated: October 18th, 2022
# Author: Brandon Allen
# Objectives: Extract and summarized the landcover information for all ANBC sites
# Keywords: Notes, Landscape extraction, Landcover summary
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
# 3) The output of this script is then run through Peter's veg-hf summary scripts
#
########################
# Landscape Extraction # 
########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(reticulate)
library(sf)

# Initialize arcpy
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') # Successful I think!

# Set geodatabase
arcpy$env$workspace <- paste0(getwd(), "/data/base/sites/ANBC_sites.gdb")

# Create buffer
arcpy$Buffer_analysis(in_features = "site_locations", 
                      out_feature_class = "site_buffer_800m", 
                      buffer_distance_or_field = "800 meters")

# Clip to the region
arcpy$PairwiseClip_analysis(in_features = "D:/backfill//veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos", 
                    clip_features = "site_buffer_800m", 
                    out_feature_class = "landcover_hfi_2018_800m")

# Intersect the two layers to maintain site information
arcpy$Intersect_analysis(in_features = c("site_buffer_800m",
                                         "landcover_hfi_2018_800m"),
                         out_feature_class = "landcover_hfi_2018_800m_intersect")

# Repair geometry
arcpy$RepairGeometry_management(in_features = "landcover_hfi_2018_800m_intersect", 
                                delete_null = "KEEP_NULL")

arcpy$CalculateGeometryAttributes_management(in_features = "landcover_hfi_2018_800m_intersect", 
                                             geometry_property = "AREA", 
                                             area_unit = "SQUARE_METERS")

# Recalculated the in ArcGIS (ShapeArea) as it wasn't working through ArcPy
# Convert to data frame
landcover.in <- read_sf(dsn = paste0(getwd(), "/data/base/sites/ANBC_sites.gdb"),
                        layer = "landcover_hfi_2018_800m_intersect")
landcover.in <- as.data.frame(landcover.in)
landcover.in$Shape_Area <- as.numeric(landcover.in$ShapeArea)
landcover.in <- landcover.in[, 1:38]
write.csv(landcover.in, file = "data/base/landcover/veg-hf_2018_800m_long.csv", row.names = FALSE)

rm(list=ls())
gc()

#####################
# Landscape Summary # 
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
load("data/processed/landcover/veg-hf_2018_800m_wide.Rdata")
veg.lookup <- read.csv("data/lookup/lookup-veg-hf-age-v2020.csv")

# Current vegetation 
veg.cur <- as.data.frame(as.matrix(d_wide$veg_current))
veg.cur <- veg.cur / rowSums(veg.cur)

unique.veg <- unique(veg.lookup$UseAvail_BEA)
unique.veg <- unique.veg[!(unique.veg %in% "EXCLUDE")]

veg.data <- data.frame(SiteID = rownames(d_wide$veg_current))

for(veg in unique.veg) {
  
  # Identify columns of interest
  veg.id <- veg.lookup[veg.lookup$UseAvail_BEA %in% veg, "ID"]
  
  # Check if row sum will fail
  if(length(veg.id) == 1) {
    
    veg.combined <- data.frame(Veg = veg.cur[, veg.id])
    
  } else {
    
    veg.combined <- data.frame(Veg = rowSums(veg.cur[, veg.id]))
    
  }
  
  colnames(veg.combined)[1] <- veg
  
  veg.data <- cbind.data.frame(veg.data,
                                     veg.combined)
  
  rm(veg.combined)
  
}

# Save the results
save(veg.data, file = "data/processed/landcover/veg-hf_2018_800m_wide_simplified.Rdata")

rm(list=ls())
gc()

