from Bio.Phylo.TreeConstruction import DistanceTreeConstructor

from task1 import pah_paralogues, h2bfs_paralogues, pah_orthologues_30
from task2 import calculator

dtc = DistanceTreeConstructor(calculator, method="upgma")

pah_paralogues_tree = dtc.build_tree(pah_paralogues)
h2bfs_paralogues_tree = dtc.build_tree(h2bfs_paralogues)
pah_orthologues_30_tree = dtc.build_tree(pah_orthologues_30)
