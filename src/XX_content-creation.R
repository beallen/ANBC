#
# Title: Creation of species pages
# Created: August 30th, 2022
# Last Updated: August 30th, 2022
# Author: Brandon Allen
# Objectives: Create pages for species
# Keywords: Notes, Pages
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All figures are going to be hosted on the local path since it is a small number of figures. May need to change
#
#########
# Pages #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(readxl)

# Load data
species.lookup <- read_excel("data/base/species/ANBC2018_monitoringdata.xlsx", sheet = "vlookup values")

# Filter the lookup table to include only genus/species IDs
species.lookup <- species.lookup[!is.na(species.lookup$Genus), ]

# Loop through page creation of each species
# Note, make sure to clear the navigation.yml upon each creation
# Append information to the navigation yml

# Add the header for the species profiles
new.text <- paste0("# ", "Species profiles", " sidebar \n",
                   "profiles", ":\n  - title: Species \n    children:")
write(new.text,
      file="_data/navigation.yml",
      append=TRUE)

for(spp in 1:length(species.lookup$Species)) {
  
  # Identify name
  name <- species.lookup$Species[spp]
  
  # Load appropriate template
  template  <- readLines("data/lookup/templates/species-template.md")
  
  # Identify page index
  fix.index <- 0
  if(spp == 1) {
    
    previous.index <- length(species.lookup$Species)
    next.index <- spp + 1
    fix.index <- 1
    
  }
  
  if(spp == length(species.lookup$Species)) {
    
    previous.index <- length(species.lookup$Species) - 1
    next.index <- 1
    fix.index <- 1
    
  }
  
  if(fix.index != 1) {
    
    previous.index <- spp - 1
    next.index <- spp + 1
    
  }
  
  # Identify link names
  name.next <- species.lookup$Species[next.index]
  name.previous <- species.lookup$Species[previous.index]
  
  # Update button links
  template <- gsub("PreviousURL", paste0("/profiles/species/", name.previous), template)
  template <- gsub("NextURL", paste0("/profiles/species/", name.next), template)
  
  # Update names
  template <- gsub("TaxonLow", "profiles", template)
  template <- gsub("SpeciesID", name, template)
  template <- gsub("SpeciesName", name, template)
  
  # Update IDS
  template <- gsub("DETECTION", name, template)
  
  # Write new lines
  writeLines(template, con = paste0("_pages/profiles/", "species/", name, ".md"))
  
  # Update navigation yml
  new.text <- paste0('      - title: "', name, '"\n',
                     "        url: /profiles/species/", name)
  write(new.text,
        file="_data/navigation.yml",
        append=TRUE)
  
  rm(template)
  
}


