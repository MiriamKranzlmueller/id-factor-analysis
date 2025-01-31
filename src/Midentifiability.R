# check M-identifiability
library(rjson)

source("src/matchingCriterion.R")

# check sign-identifiability of a graph by iterating over all latent nodes
checkMidentifiability <- function(adjMatrix, latentNodes, observedNodes, singleGraph = TRUE, maxCard = length(observedNodes)){
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
  
  latentNodes <- setdiff(latentNodes, S)
  
  notIdentifiedNodes <- setdiff(latentNodes,S)
  flowGraphAdjMatrix <- flowGraphMatrix(adjMatrix, latentNodes, observedNodes)
  tupleJson <- list()
  while (!(length(latentNodes)==0)) {
    foundIdentifiableNode <- FALSE
    for(h in notIdentifiedNodes){
      tupleForNode <- checkMatchingCriterion(flowGraphAdjMatrix, adjMatrix, h, latentNodes, observedNodes, maxCard)
      if(tupleForNode$found){
        foundIdentifiableNode <- TRUE
        latentNodes <- setdiff(latentNodes,h)
        S <- union(S,h)
        notIdentifiedNodes <- notIdentifiedNodes[! notIdentifiedNodes %in% c(h)]
        tuple <- list(list("h"=tupleForNode$h, "S"=S, "v"=tupleForNode$v, "W"=tupleForNode$W, "U"=tupleForNode$U))
        tupleJson <- c(tupleJson,tuple)
      }
    }
    
    if(!foundIdentifiableNode){
      if(singleGraph){
        return("this graph is not sign-identifiable using the matching criterion!")
      } else {
        return(list("identifiable" = FALSE, "tupleList" = list()))
      }
    }
  }
  
  # save as json
  if(singleGraph){
    graphJson <- list("adjMatrix" = adjMatrix, "latentNodes" = latentNodes, "observedNodes" = observedNodes, "tupleList" = tupleJson)
    jsonData = toJSON(graphJson)
    write(jsonData, "resultsMatchingCriterion.json")
    
    return("this graph is sign-identifiable using the matching criterion!")
  } else {
    return(list("identifiable" = TRUE, "tupleList" = tupleJson))
  }
}
