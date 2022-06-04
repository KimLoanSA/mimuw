from Bio import Phylo

from task3 import pah_paralogues_tree, h2bfs_paralogues_tree, pah_orthologues_30_tree

Phylo.write([pah_paralogues_tree], open("../output/Human_PAH_paralogues.nck", "w"), format="newick")
Phylo.write([h2bfs_paralogues_tree], open("../output/Human_H2BFS_paralogues.nck", "w"), format="newick")
Phylo.write([pah_orthologues_30_tree], open("../output/Human_PAH_orthologues_30.nck", "w"), format="newick")

Phylo.write([pah_paralogues_tree], open("../output/Human_PAH_paralogues.phyloxml", "w"), format="phyloxml")
Phylo.write([h2bfs_paralogues_tree], open("../output/Human_H2BFS_paralogues.phyloxml", "w"), format="phyloxml")
Phylo.write([pah_orthologues_30_tree], open("../output/Human_PAH_orthologues_30.phyloxml", "w"), format="phyloxml")
