# experiment for 4-9 graphs
library(rjson)

source("src/generateGraphs.R")
source("src/checkIdentifiabilities.R")

latent <- 4
observed <- 9
checkGenSignId <- FALSE
onlyZUTA <- FALSE
graphsFile <- "results/4latent_9observed_graphs_results.json"
cores = 4

# generate all graphs with 4 latent and 9 observed nodes
print("generating graphs with 4 latent and 9 observed nodes...")
generateGraphs(latent, observed, onlyZUTA, cores)
print("graphs generated.")

# test M-identifiability, extended M-identifiability and AR-identifiability
print("testing graphs for M-id, ext. M-id and AR-id...")
checkIdentifiabilityOfGraphs(latent, observed, graphsFile, checkGenSignId, onlyZUTA, cores)
print("graphs tested.")

# generate table
graphs <- fromJSON(file = "results/4latent_9observed_graphs_results.json")
table2 <- matrix(0, 32, 5)

for(graph in graphs){
  table2[graph$edges, 1] <- table2[graph$edges, 1]+1
  if(graph$ZUTA){
    table2[graph$edges, 2] <- table2[graph$edges, 2]+1
  }
  if(graph$ARidentifiability){
    table2[graph$edges, 3] <- table2[graph$edges, 3]+1
  }
  if(graph$Mid){
    table2[graph$edges, 4] <- table2[graph$edges, 4]+1
  }
  if(graph$ExtMid){
    table2[graph$edges, 5] <- table2[graph$edges, 5]+1
  }
}

graphsMoreThan30 <- sum(table2[c(1:5),1])+1
table2[31,] <- c(graphsMoreThan30,0,0,0,0)
table2[32,] <- colSums(table2)
colnames(table2)<-c("total","ZUTA", "AR-id", "M-id", "Ext. M-id")
rownames(table2) <-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14",
                     "15","16","17","18","19","20","21","22","23","24","25","26",
                     "27","28","29","30",">30","total")

write.table(table2, file = "results/experiment2_results.txt", sep = "\t", row.names = TRUE, col.names = TRUE)

print(table2)