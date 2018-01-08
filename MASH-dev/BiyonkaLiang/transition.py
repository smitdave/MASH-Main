import networkx as nx
import matplotlib.pyplot as plt
import os, sys

clArguments=sys.argv[1:]
#function takes in two arguments:
#numpy matrix as .txt representing transition matrix
matrix = np.loadtxt(clArguments[0])
#list of nodes in a community as .txt
c = open(clArguments[1]+'.txt','r')
community = c.read()
#A_{ij}: represents frequency of transition from node i to node j

#sums entries of square matrix that represents all transitions within the community and from community outward
#np.ix_ allows easier subsetting by creating n-d meshgrid for the matrix

ixgrid = np.ix_(community)
m = matrix[ixgrid, :]
comm_transitions = m.sum()

#sums entries of square matrix that represents all transitions within the community only
ixgrid_c = np.ix_(community, community)
m_c = matrix[ixgrid_c]
in_transition = m_c.sum()

#subtract total community transitions from within community transitions
out_transition = comm_transitions - in_transition

ratio = in_transition/(out_transition+in_transition)
transition_list = [in_transition, out_transition, ratio]

# Export transition frequencies and ratio
with open("OUT_"+clArguments[1]+"centralities.csv", 'w') as f:
    [f.write('{}\n'.format(item)) for item in transition_list]
