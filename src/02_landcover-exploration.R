#
# Title: Exploration of the cleaned landcover information
# Created: April 14th, 2022
# Last Updated: April 14th, 2022
# Author: Brandon Allen
# Objectives: Explore the types of habitats surveyed
# Keywords: Notes, Landscape exploration
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
#
#########################
# Landscape exploration # 
#########################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(ggplot2)
library(MetBrewer)

# Load data
load("data/processed/landcover/veg-hf_2018_800m_processed.Rdata")
veg.lookup <- read.csv("data/lookup/lookup-veg-hf-age-v2020.csv")
soil.lookup <- read.csv("data/lookup/lookup-soil-hf-v2020.csv")

# Current vegetation 
veg.cur <- as.data.frame(as.matrix(d_wide$veg_current))
veg.cur <- veg.cur / rowSums(veg.cur)

veg.simplified <- data.frame(Vegetation = NA,
                             Area = NA)
unique.veg <- unique(veg.lookup$UseAvail_BEA)
unique.veg <- unique.veg[!(unique.veg %in% "EXCLUDE")]

for(veg in unique.veg) {
  
  # Identify columns of interest
  veg.id <- veg.lookup[veg.lookup$UseAvail_BEA %in% veg, "ID"]
  
  veg.simplified <- rbind.data.frame(veg.simplified,
                                     data.frame(Vegetation = veg,
                                                Area = sum(veg.cur[, veg.id])))
  
}

veg.simplified <- veg.simplified[-1, ]

# Standardize by site area
veg.simplified$Vegetation <- factor(veg.simplified$Vegetation, levels = veg.simplified$Vegetation)

# Visualization
png(file = paste0("results/figures/landcover-surveys.png"),
    width = 1500,
    height = 1500, 
    res = 300)

ggplot(data = veg.simplified, aes(Vegetation, y = Area, fill = Vegetation)) +
  geom_bar(stat="identity", fill = met.brewer("Cross", n = 14, type = "continuous")) +
  ylab("# Equivalent Sites") +
  theme_light() +
  theme(axis.title = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 90, hjust = 1),
        axis.text.y = element_text(size=12),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

dev.off()

