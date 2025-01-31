# extended M-identifiability check
library(rjson)

source("src/matchingCriterion.R")
source("src/localBBCriterion.R")

# check sign-identifiability of a graph
checkExtendedMidentifiability <- function(adjMatrix, latentNodes, observedNodes, singleGraph = TRUE, maxCard = length(observedNodes)){
  S <- {}

  # all nodes without children are identifiable and added to S
  for(latent in latentNodes){
    hasChildren <- FALSE
    for(observed in observedNodes){
      if(adjMatrix[latent, observed]==1){
        hasChildren = TRUE
        break
      }
    }
    
    if(!hasChildren){
      S <- union(S, latent)
    }
  }
  
  adjMatrix[S,] = 0
  
  observedWithoutParents <- which(colSums(adjMatrix[])==0)
  observedNodes <- setdiff(observedNodes,observedWithoutParents)
  
  # first, try to find nodes which are identifiable using the local BB criterion, 
  # second, try to find nodes which are identifiable using the matching criterion
  
  latentNodes <- setdiff(latentNodes,S)
  notIdentifiedNodes <- latentNodes
  flowGraphAdjMatrix <- flowGraphMatrix(adjMatrix, latentNodes, observedNodes)
  tupleJson <- list()
  while (! length(latentNodes)==0) {
    foundIdentifiableNode <- FALSE
    
    tupleForSolvedNodes <- checkLocalBBCriterion(adjMatrix, latentNodes, observedNodes)
    if(tupleForSolvedNodes$found){
      foundIdentifiableNode <- TRUE
      
      tuple <- list(list("S"=S, "newNodesInS"=tupleForSolvedNodes$newNodesInS, "U"=tupleForSolvedNodes$U))
      tupleJson <- c(tupleJson,tuple)
      #print(tuple)
      latentNodes <- setdiff(latentNodes,tupleForSolvedNodes$newNodesInS)
      S <- union(S, tupleForSolvedNodes$newNodesInS)
      
      adjMatrix[tupleForSolvedNodes$newNodesInS,] = 0
      
      observedWithoutParents <- which(colSums(adjMatrix[])==0)
      observedNodes <- setdiff(observedNodes,observedWithoutParents)
    }
    
    if(!foundIdentifiableNode){
      for(h in notIdentifiedNodes){
        tupleForNode <- checkMatchingCriterion(flowGraphAdjMatrix, adjMatrix, h, latentNodes, observedNodes, maxCard)
        if(tupleForNode$found){
          foundIdentifiableNode <- TRUE
          latentNodes <- setdiff(latentNodes,h)
          S <- union(S,h)
          notIdentifiedNodes <- notIdentifiedNodes[! notIdentifiedNodes %in% c(h)]
          tuple <- list(list("h"=tupleForNode$h, "S"=S, "v"=tupleForNode$v, "W"=tupleForNode$W, "U"=tupleForNode$U))
          tupleJson <- c(tupleJson,tuple)
          
          adjMatrix[h,] = 0
          
          observedWithoutParents <- which(colSums(adjMatrix[])==0)
          observedNodes <- setdiff(observedNodes,observedWithoutParents)
          #print(tuple)
        }
      }
    }
    if(!foundIdentifiableNode){
      if(singleGraph){
        return("this graph is not sign-identifiable using both criteria!")
      } else {
        return(list("identifiable" = FALSE, "tupleList" = list()))
      }
    }
  }
  if(singleGraph){
    graphJson <- list("adjMatrix" = adjMatrix, "latentNodes" = latentNodes, "observedNodes" = observedNodes, "tupleList" = tupleJson)
    jsonData = toJSON(graphJson)
    write(jsonData, "resultsExtendedMid.json")
    return("this graph is sign-identifiable using both criteria!")
  } else {
    return(list("identifiable" = TRUE, "tupleList" = tupleJson))
  }
}