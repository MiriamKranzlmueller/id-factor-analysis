# check AR-identifiability based on the paper by
# Darjus Hosszejni and Sylvia Frühwirth-Schnatter: 
# "Cover it up! bipartite graphs uncover identifiability in sparse factor analysis", 2022. arXiv:2211.00671
# and using sparvaride package: https://CRAN.R-project.org/package=sparvaride
library(sparvaride)

# counting_rule_holds from sparvaride package needs a matrix as input, this function can handle vectors as well
# if used on its own need to check ZUTA first
checkARidentifiability <- function(adjMatrix, latentNodes, observedNodes){
  
  # generate matrix in shape needed for sparvaride
  adjMatrixSparvaride <- changeMatrixForSparvaride(adjMatrix, latentNodes, observedNodes)
  
  # use sparvaride if it is a matrix
  if(is.matrix(adjMatrixSparvaride)){
    countingRuleResult <- counting_rule_holds(adjMatrixSparvaride)
  } else {
    m <- nrow(adjMatrix)
    flowGraphMatrix <- matrix(0, m+2, m+2)
    
    # get number of latent nodes that have children
    newMatrix <- adjMatrix[rowSums(adjMatrix[])>0,] 
    if(is.matrix(newMatrix)){
      r <- nrow(newMatrix)
    } else {
      r <- 1
    }
    
    
    s <- m+1
    t <- m+2
    
    # connect latent nodes to s and to children
    for (y in latentNodes){
      flowGraphMatrix[s, y] <- (2*r +1)
      for (x in observedNodes){
        if (adjMatrix[y, x] == 1 ){
          flowGraphMatrix[y,x] <- 4*r
        }
      }
    }
    
    # connect observed Nodes to t
    for (x in observedNodes){
      flowGraphMatrix[x, t] <- r
    }
    
    stFlowGraph <- graph_from_adjacency_matrix(flowGraphMatrix, mode = "directed")
    value <- max_flow(stFlowGraph, s, t)
    if(value$value < r*(2*r +1)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

# matrix for the counting_rule_holds function of sparvaride needs a specific shape, which is generated here
changeMatrixForSparvaride <- function(adjMatrix, latentNodes, observedNodes){
  
  numberOfNodes <- nrow(adjMatrix)
  allRows <- c(1:numberOfNodes)
  notLatentRows <- (setdiff(allRows,latentNodes)) * (-1)
  notObservedColumns <- (setdiff(allRows,observedNodes)) * (-1)
  newMatrix <- t(adjMatrix[notLatentRows, notObservedColumns])
  
  newMatrix <- newMatrix[rowSums(newMatrix[])>0,colSums(newMatrix[])>0] 
  
  return(newMatrix)
}