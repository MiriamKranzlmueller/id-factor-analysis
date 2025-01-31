# experiment on large graphs
library(rjson)

source("src/generateLargeGraphsRandom.R")
source("src/checkExtMidentifiabilityLargeGraphs.R")

probs <- c(0.2,0.25,0.3,0.35,0.4,0.45)
totalGraphsPerProb <- 5000
latent <- 10
observed <- 25
maxCard <- 4
cores <- 6

# generate ZUTA graphs with 10 latent and 25 observed nodes, 
# each edge has a specific probability, each latent node has at most 10 children
print(paste("generating ", totalGraphsPerProb, " ZUTA graphs with 10 latent and 25 observed nodes for each probability...", sep=""))
for(probForEdge in probs){
  generateLargeGraphsRandom(probForEdge,totalGraphsPerProb)
}
print("graphs generated.")

# check extended M-identifiability of these graphs
print("testing graphs for ext. M-id...")
for(probForEdge in probs){
  checkExtMidentifiabilityOfLargeGraphs(probForEdge, latent, observed, maxCard, cores)
}
print("graphs tested.")



# generate table
table3 <- matrix(0, 2, 6)
i = 0

for(probForEdge in probs){
  i = i+1
  nameLoad =  paste("results/10latent_25observed_graphs_", probForEdge,"_results.json", sep="")
  graphs <- fromJSON(file = nameLoad)
  
  for(graph in graphs){
    if(graph$ExtMid){
      table3[1, i] <- table3[1, i]+1
    }
  }
  table3[2,i] <- table3[1,i]/totalGraphsPerProb*100 
}
colnames(table3)<-c("0.2","0.25", "0.3", "0.35", "0.4","0.45")
rownames(table3) <-c("Ext. M-id","Percentage")

write.table(table3, file = "results/experiment3_results.txt", sep = "\t", row.names = TRUE, col.names = TRUE)

print(table3)