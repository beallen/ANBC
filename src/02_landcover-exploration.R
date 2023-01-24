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
veg.data <- colSums(veg.data[, -1])

# Correct names
names(veg.data)[c(5, 7, 9:11)] <- c("Treed Bog/Fen", "Grass/Shrub", "Urban Industrial", "Soft Linear", "Hard Linear")

veg.data <- data.frame(Vegetation = factor(names(veg.data), levels = names(veg.data)),
                             Area = as.numeric(veg.data))

# Visualization
landcover.plot <- ggplot(data = veg.data, aes(Vegetation, y = Area, fill = Vegetation)) +
  geom_bar(stat="identity", fill = met.brewer("Cross", n = 14, type = "continuous")) +
  ylab("# Equivalent Sites") +
  theme_light() +
  theme(axis.title = element_text(size=20),
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size=18),
        legend.title = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave(filename = paste0("results/figures/landcover-surveys.png"),
       plot = landcover.plot,
       height = 800,
       width = 1200,
       dpi = 72,
       units = "px")

