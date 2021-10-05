from Bio.Phylo.TreeConstruction import DistanceCalculator

from task1 import pah_paralogues, h2bfs_paralogues, pah_orthologues_30

calculator = DistanceCalculator('blosum62')

print("PAH_paralogues distance:")
print(calculator.get_distance(pah_paralogues))

print("H2BFS_paralogues distance:")
print(calculator.get_distance(h2bfs_paralogues))

print("PAH_orthologues_30 distance:")
print(calculator.get_distance(pah_orthologues_30))
