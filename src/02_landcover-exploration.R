#
# Title: Exploration of the cleaned landcover information
# Created: April 14th, 2022
# Last Updated: March 27th, 2023
# Author: Brandon Allen
# Objectives: Explore the types of landcover surveyed by each project
# Keywords: Notes, Landscape exploration
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
load("data/processed/landcover/veg-hf_800m_wide_simplified.Rdata")

# Correct names
names(veg.data)[c(6, 8, 10:12)] <- c("Treed Bog/Fen", "Grass/Shrub", "Urban Industrial", "Soft Linear", "Hard Linear")

# Summarize across sites and projects
veg.data$Project <- NA
veg.data$Project[grep("ACA", rownames(veg.data))] <- "ACA"
veg.data$Project[grep("AEP", rownames(veg.data))] <- "AEP"
veg.data$Project[grep("ANBC", rownames(veg.data))] <- "ANBC"
veg.data$Project[grep("iNaturalist", rownames(veg.data))] <- "iNaturalist"
veg.data$Project[grep("Strickland", rownames(veg.data))] <- "Strickland"

veg.data <- aggregate(veg.data[, !(colnames(veg.data) %in% c("SiteID", "Project"))], 
                      by = list(Project = veg.data$Project), 
                      FUN = function(x) sum(x))

rownames(veg.data) <- veg.data[, 1]
veg.data <- t(veg.data[, -1])

veg.data <- as.data.frame(veg.data)
veg.data$Vegetation <- factor(rownames(veg.data), levels = rownames(veg.data))

# Standardized based on number of sites
for(x in 1:5) {
  
  veg.data[, x] <- (veg.data[, x] / sum(veg.data[, x])) * 100
  
}

for (project.id in colnames(veg.data)[1:5]) {
  
  # Visualization
  landcover.plot <- ggplot(data = veg.data, aes_string(x = "Vegetation", y = project.id, fill = "Vegetation")) +
    geom_bar(stat="identity", fill = met.brewer("Cross", n = 14, type = "continuous")) +
    ylab("% Survey Effort") +
    theme_light() +
    theme(axis.title = element_text(size=20),
          axis.text.x = element_text(size=18, angle = 45, hjust = 1),
          axis.text.y = element_text(size=18),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA, size=1))
  
  ggsave(filename = paste0("results/figures/landcover/landcover-surveys_", project.id, ".png"),
         plot = landcover.plot,
         height = 800,
         width = 1200,
         dpi = 72,
         units = "px")
  
}

rm(list=ls())
gc()


