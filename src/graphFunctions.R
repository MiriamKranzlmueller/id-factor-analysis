# functions to find specific sets of nodes in a graph

# compute joint parents of a set
jointParents <- function(adjMatrix, set, latentNodes){
  jointParents <- c()
  remainingSet <- set
  remainingLatentNodes <- latentNodes
  for(u in set){
    remainingSet <- setdiff(remainingSet, u)
    for(v in remainingSet){
      for(parent in remainingLatentNodes){
        if(adjMatrix[parent, u]== 1 & adjMatrix[parent, v]== 1){
          jointParents <- append(jointParents, parent)
          remainingLatentNodes <- setdiff(remainingLatentNodes, parent)
        }
      }
    }
  }
  return(jointParents)
}

# compute set of children of a set of nodes
children <- function(adjMatrix, set, possibleChildren){
  setOfChildren <- c()
  for(node in set) {
    for (child in possibleChildren){
      if (adjMatrix[node, child] ==1 && !(child %in% setOfChildren)){
        setOfChildren <- append(setOfChildren, child)
      }
    }
  }
  return(setOfChildren)
}

# compute the parents of a set of nodes
parents <- function(adjMatrix, set, possibleParents){
  setOfParents <- c()
  for (node in set) {
    for (parent in possibleParents){
      if ((adjMatrix[parent, node] == 1) && !(parent %in% setOfParents)){
        setOfParents <- append(setOfParents, parent)
      }
    }
  }
  return(setOfParents)
}