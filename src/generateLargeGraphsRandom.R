# generate graphs with 10 latent and 25 observed nodes
library(rjson)
library(igraph)

generateLargeGraphsRandom <- function(probForEdge,totalGraphsPerProb){
  set.seed(1)
  graphs <- lapply(c(1:totalGraphsPerProb), generateOneLargeGraph, probForEdge)
  graphsListJson <- c()
  
  for(allEdges in graphs){
    graphJson <- list(list("edges" = sum(allEdges), "adjMatrix" = allEdges))
    graphsListJson <- c(graphsListJson, graphJson)
  }
  
  # generate output json
  name =  paste("results/10latent_25observed_graphs_10_children_", probForEdge, ".json", sep="")
  jsonData = toJSON(graphsListJson)
  write(jsonData, name)
}

generateOneLargeGraph <- function(number,probForEdge){
  notFoundOne <- TRUE
  
  while(notFoundOne){
    edges <- sample(0:1,205, replace=T, prob = c(1-probForEdge,probForEdge))
    allEdges <- c(integer(350))
    for (i in 1:16){
      allEdges <- c(allEdges,edges[((i-1)*10+1):(i*10)],integer(25))
    }
    lastEdge <- 160
    zutaEdges <- c()
    for(i in 1:9){
      zutaEdges <- c(zutaEdges,edges[(lastEdge+1):(lastEdge+(10-i))],integer(25+i))
      lastEdge <- lastEdge+(10-i)
    }
    
    allEdges <- c(allEdges,zutaEdges)
    
    adjMatrix <- matrix(allEdges, 35, 35)
    if(!any(rowSums(adjMatrix[])>10)){
      notFoundOne <- FALSE
    }
  }
  return(allEdges)
}
