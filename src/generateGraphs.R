# generate graphs using parallelization
# only graphs are generated, that do not exceed the maximal number of edges for ZUTA 
# ZUTA still needs to be checked if onlyZUTA is false
library(parallel)
library(igraph)
library(rjson)
source("src/ZUTA.R")

generateGraphs <- function(latent, observed, onlyZUTA = FALSE, cores = 2){
  
  graphList <- list()
  edgeNumbers <- list()
  for(i in 1:latent){
    graphList <- append(graphList,list(c(rep(1,i),rep(0,latent-i))))
    edgeNumbers <- append(edgeNumbers,i)
  }
  
  # iterate over all observed nodes and compute all distinct graphs for each iteration
  for(iteration in 2:observed){
    x <- Sys.time()
    numberUniqueEdgeNumbers <- length(unique(edgeNumbers))
    
    # fork
    # calculate the new graph list for all distinct edge numbers (number of edges for each observed node) from previous iteration
    result <- mclapply(unique(edgeNumbers), getGraphsForNewNode, edgeNumbers, graphList, iteration, latent, observed, mc.cores = cores)
    edgeNumbers <- list()
    graphList <- list()
    
    # concatenate the graph list and edge number list
    for(i in 1:numberUniqueEdgeNumbers){
      graphList <- append(graphList, result[[i]][[1]]$graphs)
      edgeNumbers <- append(edgeNumbers, result[[i]][[1]]$edgeNumbers)
    }
    y <- Sys.time()
    print(length(graphList))
    print(y-x)
  }
  
  maxZUTAEdges <- ((latent-1)*latent)/2
  
  # generate the graphs to put in the json list
  graphsListJson <- list()
  for(graph in graphList){
    parts <- split(graph, cut(seq_along(graph), observed, labels=FALSE))
    allEdges <- c()
    for(part in parts){
      # add zeros for adjacency matrix
      addedZeros <- c(part,integer(observed))
      allEdges <- c(allEdges, addedZeros)
    }
    adjMatrixAsVector <- c(integer(latent*(observed+latent)), allEdges)
    adjMatrix <- matrix(adjMatrixAsVector,observed+latent,observed+latent)
    
    if(onlyZUTA){
      if(checkZUTA(adjMatrix, c(1:latent), c((latent+1):(latent+observed)))){
        newGraphJson <- list(list("edges" = sum(adjMatrixAsVector), "adjMatrix" = adjMatrixAsVector))
        graphsListJson <- c(graphsListJson, newGraphJson)
      }
    } else {
      newGraphJson <- list(list("edges" = sum(adjMatrixAsVector), "adjMatrix" = adjMatrixAsVector))
      graphsListJson <- c(graphsListJson, newGraphJson)
    }
    
    # calculate the remaining graphs, which are the complement of the previous graphs
    if(sum(adjMatrixAsVector)>=maxZUTAEdges & sum(adjMatrixAsVector)<(latent*observed)/2){
      ones <- rep(1, (latent*observed))
      complementGraph <- ones-graph
      
      partsComplement <- split(complementGraph, cut(seq_along(complementGraph), observed, labels=FALSE))
      allEdges <- c()
      for(part in partsComplement){
        # add zeros for adjacency matrix
        addedZeros <- c(part,integer(observed))
        allEdges <- c(allEdges, addedZeros)
      }
      adjMatrixAsVector <- c(integer(latent*(observed+latent)), allEdges)
      adjMatrix <- matrix(adjMatrixAsVector,observed+latent,observed+latent)
      
      if(onlyZUTA){
        if(checkZUTA(adjMatrix, c(1:latent), c((latent+1):(latent+observed)))){
          newGraphJson <- list(list("edges" = sum(adjMatrixAsVector), "adjMatrix" = adjMatrixAsVector))
          graphsListJson <- c(graphsListJson, newGraphJson)
        }
      } else {
        newGraphJson <- list(list("edges" = sum(adjMatrixAsVector), "adjMatrix" = adjMatrixAsVector))
        graphsListJson <- c(graphsListJson, newGraphJson)
      }
    }
  }

  # generate output json
  name =  paste("results/",latent, "latent_", observed, "observed_graphs.json", sep="")
  jsonData = toJSON(graphsListJson)
  write(jsonData, name)
  
  return(length(graphsListJson))
}

# calculate list of graphs for one edge number by adding all different possible edges for another observed node
getGraphsForNewNode <- function(edgeNumbers, listOfEdgeNumbers, graphList, iteration, latent, observed){
  listOfNewEdgeNumbers <- list()
  listOfNewGraphs <- list()
  binaryList <- list()
  
  # take all graphs from existing graph list, which have the same edge numbers
  if(any(edgeNumbers == listOfEdgeNumbers)){
    for(number in which(listOfEdgeNumbers == edgeNumbers)){
      graphInFront <- unlist(graphList[number])
      
      # add all possible edges
      for(j in 0:(2^latent)-1){
        binary <- as.integer(intToBits(j))[1:latent]
        newBinary <- c(graphInFront, binary)
        
        # check if the number of edges is smaller than half of the possible edges
        if(sum(newBinary) <= (latent*observed)/2){
          
          # calculate new edges list and new edge number
          newEdgesCount <- sum(binary)
          newEdgeNumbers <- as.integer(paste(edgeNumbers,newEdgesCount,sep=""))
          
          # split the new binary to generate adjacency matrix by adding zeros
          parts <- split(newBinary, cut(seq_along(newBinary), iteration, labels=FALSE))
          currentMax <- latent
          allEdges <- c()
          wrongOrder <- FALSE
          
          for(part in parts){
            # check that the number of children of latent nodes is ordered in descending order
            sum <- sum(unlist(part))
            if (sum > currentMax){
              wrongOrder <- TRUE
              break
            }
            currentMax <- sum
            
            # add zeros for adjacency matrix
            addedZeros <- c(part,integer(iteration))
            allEdges <- c(allEdges, addedZeros)
          }
          if(!wrongOrder){
            
            # generate adjacency matrix
            adjMatrixAsVector <- c(integer(latent*(latent+iteration)), allEdges)
            adjMatrix <-matrix(adjMatrixAsVector, latent+iteration, latent+iteration)
            newGraph <- graph_from_adjacency_matrix(adjMatrix)
            foundIsomorphic <- FALSE
            
            # check if there is already a graph with the same number of children for all latent nodes
            if(any(newEdgeNumbers == listOfNewEdgeNumbers)){
              for(numberOfCompareGraph in which(listOfNewEdgeNumbers == newEdgeNumbers)){
                graphToCompareVector <- unlist(listOfNewGraphs[numberOfCompareGraph])
                graphToCompareMatrix <- matrix(graphToCompareVector, latent+iteration, latent+iteration)
                graphToCompare <- graph_from_adjacency_matrix(graphToCompareMatrix)
                
                # compare the new graph to the old graph
                if(isomorphic(newGraph,graphToCompare)){
                  foundIsomorphic <- TRUE
                  break
                }
              }
            }
            
            # add graph to list of graphs
            if(!foundIsomorphic){
              listOfNewGraphs <- append(listOfNewGraphs, list(adjMatrixAsVector))
              binaryList <- append(binaryList, list(newBinary))
              listOfNewEdgeNumbers <- append(listOfNewEdgeNumbers, newEdgeNumbers)
            }
          }
        }
      }
    }
  } 
  return(list(list("edgeNumbers" = listOfNewEdgeNumbers, "graphs" = binaryList)))
}