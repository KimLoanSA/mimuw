from Bio import Phylo

from task3 import pah_paralogues_tree, h2bfs_paralogues_tree, pah_orthologues_30_tree

print("PAH_paralogues tree:")
Phylo.draw_ascii(pah_paralogues_tree)
Phylo.draw(pah_paralogues_tree)

print("H2BFS_paralogues tree:")
Phylo.draw_ascii(h2bfs_paralogues_tree)
Phylo.draw(h2bfs_paralogues_tree)

print("PAH_orthologues_30 tree:")
Phylo.draw_ascii(pah_orthologues_30_tree)
Phylo.draw(pah_orthologues_30_tree)