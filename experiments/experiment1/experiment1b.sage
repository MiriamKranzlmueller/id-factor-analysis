import numpy as np
import json

load("src/functions.sage")

print("testing graphs for sign-identifiability using Groebner bases...")
graphsResults = []
    
with open('results/3latent_7observed_graphs.json', 'r') as graphFile:
    graphs = json.load(graphFile)

for graph in graphs:
    adjMatrixAsVector = np.array(graph["adjMatrix"])

    adjMatrix = adjMatrixAsVector.reshape(10,10)
    adjMatrixClean = adjMatrix[3:10,0:3]

    result = check_sign_identifiability(adjMatrixClean)
    graphsResults.append({
        "edges": graph["edges"],
        "adjMatrix": graph["adjMatrix"],
        "groebner": result
        })

graphsResultsJson = json.dumps(graphsResults, indent=4, separators=(',',': '))
 
with open("results/3latent_7observed_graphs_groebner.json", "w") as graphResultsFile:
    graphResultsFile.write(graphsResultsJson)

print("graphs tested.")
