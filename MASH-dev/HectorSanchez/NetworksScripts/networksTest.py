import networkx as nx
import csv

# Generate a random network
G=nx.watts_strogatz_graph(100,3,.2)
# Calculate centrality
deg=nx.betweenness_centrality(G)
# Export adjacency list
nx.write_adjlist(G,"OUT_networkExportTest.adjlist")
# Export centralities list
with open('OUT_centralities.csv', 'w') as f:
    [f.write('{0},{1}\n'.format(key, value)) for key, value in deg.items()]
