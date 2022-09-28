#
# Title: Extraction of landcover information for the ANBC
# Created: April 14th, 2022
# Last Updated: September 28th, 2022
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
library(foreign)
library(reticulate)
library(sf)

# Initialize arcpy
py_discover_config() # We need version 3.7
py_config() # Double check it is version 3.7

# Set python 
use_python(python = "C:/Users/ballen/miniconda3/envs/r-reticulate/python.exe")

# Load arcpy
arcpy <- import('arcpy') # Successful I think!

# Create geodatabase
arcpy$CreateFileGDB_management(out_folder_path = paste0(getwd(), "/data/processed/landcover/"), 
                               out_name = "ANBC_landcover.gdb")

# Define workspace
arcpy$env$workspace <- paste0(getwd(), "/data/processed/landcover/ANBC_landcover.gdb")

# Create buffer
arcpy$Buffer_analysis(in_features = paste0(getwd(), "/data/base/sites/ANBC_surveys_2018.shp"), 
                      out_feature_class = "ANBC_surveys_2018_800m", 
                      buffer_distance_or_field = "800 meters")

# Clip to the region
arcpy$PairwiseClip_analysis(in_features = "D:/backfill//veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos", 
                    clip_features = "ANBC_surveys_2018_800m", 
                    out_feature_class = "landcover_hfi_2018_800m")

# Intersect the two layers to maintain site information
arcpy$Intersect_analysis(in_features = c("ANBC_surveys_2018_800m",
                                         "landcover_hfi_2018_800m"),
                         out_feature_class = "landcover_hfi_2018_800m_intersect")

# Repair geometry
arcpy$RepairGeometry_management(in_features = "landcover_hfi_2018_800m_intersect", 
                                delete_null = "KEEP_NULL")


# Calculate proper areas, adjust column names
landcover.in <- read_sf(dsn = paste0(getwd(), "/data/processed/landcover/ANBC_landcover.gdb"),
                        layer = "landcover_hfi_2018_800m_intersect")
landcover.in$Shape_Area <- st_area(landcover.in) # ArcGIS function doesn't like reticulate, same areas though
colnames(landcover.in)[c(24,31,34)] <- c("Soil_Type_1", "Origin_Year", "Combined_ChgByCWCS") 

# Convert to data frame
landcover.in <- as.data.frame(landcover.in)
landcover.in <- landcover.in[, c(1:40)]
landcover.in$Shape_Area <- as.numeric(landcover.in$Shape_Area)
write.csv(landcover.in, file = "data/processed/landcover/veg-hf_2018_800m.csv", row.names = FALSE)

rm(list=ls())
gc()

#####################
# Landscape Summary # 
#####################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

