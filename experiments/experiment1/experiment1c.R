# third part of experiment for 3-7 graphs
library(rjson)

source("src/checkIdentifiabilities.R")

latent <- 3
observed <- 7
onlyZUTA <- TRUE
checkGenSignId <- TRUE
graphsFile <- "results/3latent_7observed_graphs_groebner.json"

print("testing graphs for M-id, ext. M-id and AR-id...")
# test M-identifiability, extended M-identifiability and AR-identifiability
checkIdentifiabilityOfGraphs(latent, observed, graphsFile, checkGenSignId, onlyZUTA)
print("graphs tested.")

# generate table
graphs <- fromJSON(file = "results/3latent_7observed_graphs_results.json")
table1 <- matrix(0, 19, 5)

for(graph in graphs){
  if(graph$ZUTA){
    table1[graph$edges, 1] <- table1[graph$edges, 1]+1
  }
  if(graph$genSignId){
    table1[graph$edges, 2] <- table1[graph$edges, 2]+1
  }
  if(graph$ARidentifiability){
    table1[graph$edges, 3] <- table1[graph$edges, 3]+1
  }
  if(graph$Mid){
    table1[graph$edges, 4] <- table1[graph$edges, 4]+1
  }
  if(graph$ExtMid){
    table1[graph$edges, 5] <- table1[graph$edges, 5]+1
  }
}
table1[19,] <- colSums(table1)
colnames(table1)<-c("ZUTA", "gen. sign-id", "AR-id", "M-id", "Ext. M-id")
rownames(table1) <-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","total")

write.table(table1, file = "results/experiment1_results.txt", sep = "\t", row.names = TRUE, col.names = TRUE)

print(table1)