# first part of experiment for 3-7 graphs
library(rjson)

source("src/generateGraphs.R")

latent <- 3
observed <- 7
onlyZUTA <- TRUE
graphsFile <- "results/3latent_7observed_graphs.json"

# generate all graphs with 3 latent and 7 observed nodes that fulfill ZUTA
print("generating graphs with 3 latent and 7 observed nodes...")
generateGraphs(latent, observed,onlyZUTA)
print("graphs generated.")
