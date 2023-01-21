#
# Title: Community analysis exploration
# Created: August 30th, 2022
# Last Updated: September 24th, 2022
# Author: Brandon Allen
# Objectives: Clean and create basic summaries for the species data
# Keywords: Notes, Species data, Range maps
#

#########
# Notes #
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# 1) All sites were surveyed in 2018
# 2) Need to identify the accuracy of GPS locations (e.g., location of traps versus fire towers)
# 3) Some accession values don't have IDs.
# 4) Some species with IDs don't have lat long coordinates or other site identifiers.
# 5) THIS ANALYSIS MIGHT BE A DUD AS IT ONLY IDENTIFIES ONE COMMUNITY
#
################
# Species data # 
################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# Load libraries
library(EcoCluster)

# Load data
load("data/processed/species/species-abundance.Rdata")

# Standardize to presence/absence
spp.data[, -c(1:4)][spp.data[, -c(1:4)] > 1] <- 1

#
# Community model
#

# Define parameters for mixture model
ngibbs <- 10000
n.group <- 10

# Mixture model for bombus
community.mix <- mixture.gibbs(dat = as.matrix(spp.data[, c(17:46)]),
                            ngroup = n.group,
                            nl = 1:nrow(spp.data),
                            ngibbs = ngibbs,
                            burnin = ngibbs/2,
                            a.prior = 1,
                            b.prior = 1)

# Check for convergence (want a fuzzy caterpillar)
plot(community.mix$logl, type='l',main='convergence assessment',xlab='Gibbs iterations',ylab='log-likelihood')

# Identify number of clusters
plot(colMeans(community.mix$theta),type='h',xlab='clusters',main='number of groups',ylab='theta',lwd=2)

# Calculate the prevalence of each species on the landscape
community.obs <- data.frame(Species = colnames(spp.data)[c(17:46)],
                         Prevalence = colSums(ifelse(spp.data[, c(17:46)] > 0, 1, 0)) / nrow(spp.data))

# Calculate species list
spp.name <- NULL
for(id in community.obs$Species) {
  
  # Replicating by the number of groups
  spp.name <- c(spp.name, rep(id, 10))
  
}

community.clusters <- data.frame(Species = spp.name,
                              Cluster = rep(seq(10), length(community.obs$Species)),
                              Phi = colMeans(community.mix$phi),
                              PhiScaled = colMeans(community.mix$phi),
                              Favorability = colMeans(community.mix$phi))

# Keep only the first two clusters based on the site assignment
community.clusters <- community.clusters[community.clusters$Cluster <= 2, ]

# Rescale
# Create the two versions of scaling (Phi and Favorability)
for(id in community.obs$Species) {
  
  # Phi scaling
  community.clusters[community.clusters$Species == id, "PhiScaled"] <- community.clusters[community.clusters$Species == id, "PhiScaled"] / sum(community.clusters[community.clusters$Species == id, "PhiScaled"])
  
  # Favorability
  community.clusters[community.clusters$Species == id, "Favorability"] <- plogis(qlogis(community.clusters[community.clusters$Species == id, "Favorability"]) - qlogis(community.obs[community.obs$Species == id, "Prevalence"]))
  
}

ggplot(data = community.clusters) +
        geom_tile(aes(x = Cluster, y = Species, fill = PhiScaled, col = PhiScaled)) +
        scale_fill_gradientn(colours = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous"))) +
        scale_color_gradientn(colours = rev(met.brewer(name = "Hiroshige", n = 100, type = "continuous"))) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

