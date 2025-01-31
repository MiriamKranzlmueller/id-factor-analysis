# check extended M-identifiability of graphs with 25 observed and 10 latent nodes
library(parallel)
library(rjson)

source("src/extendedMidentifiability.R")

checkExtMidentifiabilityOfLargeGraphs <- function(prob, latent, observed, maxCard = observed, cores = 2){
  
  x <- Sys.time()
  nameLoad =  paste("results/", latent, "latent_", observed, "observed_graphs_10_children_", prob,".json", sep="")
  graphs <- fromJSON(file = nameLoad)
  
  graphsResultsJson <- list()
  
  graphsResultsJson <- mclapply(graphs, testGraphForExtMid, maxCard, mc.cores = cores)
  
  name =  paste("results/", latent, "latent_", observed, "observed_graphs_", prob,"_results.json", sep="")
  jsonData = toJSON(graphsResultsJson)
  write(jsonData, name)
  y <- Sys.time()
  print(prob)
  print(y-x)
}

# check sign-identifiability for a graph using extended M-identifiability
testGraphForExtMid <- function(graph, maxCard){
  latentNodes <- c(1:10)
  observedNodes <- c(11:35)
  latent <- 10
  observed <- 25
  graphResults <- list()
  adjMatrix <- matrix(graph$adjMatrix, 35, 35)
  
  ExtMidResult <- checkExtendedMidentifiability(adjMatrix, latentNodes, observedNodes, FALSE, maxCard)
  
  graphResults <- list("edges" = graph$edges, "adjMatrix" = graph$adjMatrix, "ExtMid" = ExtMidResult$identifiable)
  return(graphResults)
}