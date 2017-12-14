import networkx as nx

G=nx.complete_graph(100)
deg=nx.degree_centrality(G)
print(deg)