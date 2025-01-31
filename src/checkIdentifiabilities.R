# check M-, ext. M- and AR-identfiability of graphs
library(parallel)
library(rjson)
library(igraph)
source("src/Midentifiability.R")
source("src/extendedMidentifiability.R")
source("src/ARidentifiability.R")
source("src/ZUTA.R")

# test graphs using parallelization 
# with or without previous generic sign-identifiability test
checkIdentifiabilityOfGraphs <- function(latent, observed, graphsFile, checkGenSignId = FALSE, onlyZUTA = FALSE, cores = 2){
  x <- Sys.time()
  graphs <- fromJSON(file = graphsFile)
  graphsResultsJson <- list()
  graphsResultsJson <- mclapply(graphs, runAllTestsForGraph, latent, observed, checkGenSignId, onlyZUTA, mc.cores = cores)
  
  name =  paste("results/", latent, "latent_", observed, "observed_graphs_results.json", sep="")
  jsonData = toJSON(graphsResultsJson)
  write(jsonData, name)
  y <- Sys.time()
  print(y-x)
  return("finished")
}

# check sign-identifiability for a graph M-, extended M- and AR-identifiability
runAllTestsForGraph <- function(graph, latent, observed, checkGenSignId, onlyZUTA){
  latentNodes <- c(1:latent)
  observedNodes <- c((latent+1):(latent+observed))
  graphResults <- list()
  adjMatrix <- matrix(graph$adjMatrix, latent+observed, latent+observed)
  
  if (onlyZUTA){
    ZUTAResult <- TRUE
  } else {
    ZUTAResult <- checkZUTA(adjMatrix, latentNodes, observedNodes)
  }
  
  MidResult <- checkMidentifiability(adjMatrix, latentNodes, observedNodes, FALSE)
  ExtMidResult <- checkExtendedMidentifiability(adjMatrix, latentNodes, observedNodes, FALSE)
  
  if(ZUTAResult){
    ARidentifiabilityResult <- checkARidentifiability(adjMatrix, latentNodes, observedNodes)
  } else {
    ARidentifiabilityResult <- FALSE
  }
  
  
  if(checkGenSignId){
    graphResults <- list("edges" = graph$edges, 
                         "adjMatrix" = graph$adjMatrix, 
                         "ZUTA" = ZUTAResult, 
                         "Mid" = MidResult$identifiable, 
                         "tupleListMid" = MidResult$tupleList, 
                         "ExtMid" = ExtMidResult$identifiable, 
                         "tupleListExtMid" = ExtMidResult$tupleList,
                         "ARidentifiability" = ARidentifiabilityResult, 
                         "genSignId" = graph$groebner)
  } else {
    graphResults <- list("edges" = graph$edges, 
                         "adjMatrix" = graph$adjMatrix, 
                         "ZUTA" = ZUTAResult, 
                         "Mid" = MidResult$identifiable, 
                         "tupleListMid" = MidResult$tupleList, 
                         "ExtMid" = ExtMidResult$identifiable, 
                         "tupleListExtMid" = ExtMidResult$tupleList,
                         "ARidentifiability" = ARidentifiabilityResult) 
  }
  return(graphResults)
}
