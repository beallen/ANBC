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
load("data/processed/landcover/veg-hf_2018_800m_wide_simplified.Rdata")

# Summarize across sites
veg.simplified <- colSums(veg.simplified[, -1])
veg.simplified <- data.frame(Vegetation = factor(names(veg.simplified), levels = names(veg.simplified)),
                             Area = as.numeric(veg.simplified))

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

