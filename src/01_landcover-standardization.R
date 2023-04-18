#
# Title: Extraction of landcover information for the ANBC
# Created: April 14th, 2022
# Last Updated: March 27th, 2023
# Author: Brandon Allen
# Objectives: Clean and organize the climate and landcover summaries
# Keywords: Notes, Site Summaries, kgrid summaries
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) The landcover summaries were provided by Eric based on an 800m buffer around each site.
# 2) Each site uses the appropriately aged landcover layer with the closest Human Footprint Inventory stamped on top.
# 3) The initial file has already been pre-processed through the veg-hf-summary script.
#
##################
# Site Summaries # 
##################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load data
load("data/base/sites/anbc-landscape-summaries.Rdata")
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

site.info <- d_long[, c("UID", "NRNAME", "NSRNAME")]
site.info <- site.info[!duplicated(site.info), ]

# Save the results
save(veg.data, site.info, file = "data/processed/landcover/veg-hf_800m_wide_simplified.Rdata")

###################
# Kgrid Sumamries # 
###################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load lookup tables
veg.lookup <- read.csv("data/lookup/lookup-veg-hf-age-v2020.csv")
unique.veg <- unique(veg.lookup$UseAvail_BEA)
unique.veg <- unique.veg[!(unique.veg %in% "EXCLUDE")]

# Load landcover
load("data/base/landcover/veghf_w2w_2018_wide_water.RData")

# Current
veg.cur <- as.data.frame(as.matrix(dd_2018$veg_current))

# Convert to proportions
veg.cur <- veg.cur / rowSums(veg.cur)

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

veg.cur <- veg.data

# Reference
veg.ref <- as.data.frame(as.matrix(dd_2018$veg_reference))

# Convert to proportions
veg.ref <- veg.ref / rowSums(veg.ref)

veg.data <- data.frame(LinkID = rownames(veg.ref))

# Native only
for(veg in unique.veg[1:5]) {
  
  # Identify columns of interest
  veg.id <- veg.lookup[veg.lookup$UseAvail_BEA %in% veg, "ID"]
  
  # Check if row sum will fail
  if(length(veg.id) == 1) {
    
    veg.combined <- data.frame(Veg = veg.ref[, veg.id])
    
  } else {
    
    veg.combined <- data.frame(Veg = rowSums(veg.ref[, colnames(veg.ref) %in% veg.id]))
    
  }
  
  colnames(veg.combined)[1] <- veg
  
  veg.data <- cbind.data.frame(veg.data,
                               veg.combined)
  
  rm(veg.combined)
  
}

veg.ref <- veg.data

rm(dd_2018, lts, ltv)

# Load spatial data
load("data/base/landcover/kgrid_table_km.Rdata")

kgrid <- data.frame(LinkID = kgrid$Row_Col,
                    Latitude = kgrid$POINT_Y,
                    Longitude = kgrid$POINT_X,
                    NaturalRegion = kgrid$NRNAME,
                    MAP = kgrid$MAP,
                    MAT = kgrid$MAT,
                    FFP = kgrid$FFP)

# Merge with kgrid
kgrid.cur <- merge.data.frame(kgrid, veg.cur, by = "LinkID")
kgrid.ref <- merge.data.frame(kgrid, veg.ref, by = "LinkID")

save(kgrid.cur, kgrid.ref, file = "data/processed/landcover/kgrid-processed.Rdata")

rm(list=ls())
gc()
