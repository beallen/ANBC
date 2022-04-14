#
# Title: Extraction of landcover information for the ANBC
# Created: April 14th, 2022
# Last Updated: April 14th, 2022
# Author: Brandon Allen
# Objectives: Extract the landcover information for the ANBC survey sites from the 2018 backfilll
# Keywords: Notes, Landscape extraction
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

# Define the scratch space otherwise functions without defined outputs will fail
scratch.space <- "C:/Users/ballen/Desktop/ANBC/scratch/"
arcpy$env$scratchWorkspace <- scratch.space

# Create buffer
arcpy$Buffer_analysis(in_features = "C:/Users/ballen/Desktop/ANBC/data/base/sites/ANBC_surveys_2018.shp", 
                      out_feature_class = "C:/Users/ballen/Desktop/ANBC/data/base/sites/ANBC_surveys_2018_800m.shp", 
                      buffer_distance_or_field = "800 meters")

# Clip to the region
arcpy$PairwiseClip_analysis(in_features = "D:/backfill//veg61hf2018_bdqt.gdb/veg61hf2018_BDQT_mtos", 
                    clip_features = "C:/Users/ballen/Desktop/ANBC/data/base/sites/ANBC_surveys_2018_800m.shp", 
                    out_feature_class = "C:/Users/ballen/Desktop/ANBC/data/processed/landcover/landcover_hfi_2018_800m.shp")

# Intersect the two layers to maintain site information
arcpy$Intersect_analysis(in_features = c("C:/Users/ballen/Desktop/ANBC/data/base/sites/ANBC_surveys_2018_800m.shp",
                                         "C:/Users/ballen/Desktop/ANBC/data/processed/landcover/landcover_hfi_2018_800m.shp"),
                         out_feature_class = "C:/Users/ballen/Desktop/ANBC/data/processed/landcover/landcover_hfi_2018_800m_intersect.shp")

# Calculate proper areas, adjust column names
landcover.in <- read_sf("C:/Users/ballen/Desktop/ANBC/data/processed/landcover/landcover_hfi_2018_800m_intersect.shp")
landcover.in$Shape_Area <- st_area(landcover.in) # ArcGIS function doesn't like reticulate, same areas though
colnames(landcover.in)[c(24,31,34)] <- c("Soil_Type_1", "Origin_Year", "Combined_ChgByCWCS") 

write_sf(landcover.in, dsn = "C:/Users/ballen/Desktop/ANBC/data/processed/landcover/landcover_hfi_2018_800m_intersect.shp")

# Convert to data frame
landcover.in <- as.data.frame(landcover.in)
landcover.in <- landcover.in[, c(1:40)]
landcover.in$Shape_Area <- as.numeric(landcover.in$Shape_Area)
write.csv(landcover.in, file = "data/processed/landcover/veg-hf_2018_800m.csv", row.names = FALSE)

rm(list=ls())
gc()


