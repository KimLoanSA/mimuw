from Bio import AlignIO


pah_paralogues = AlignIO.read("../input/Human_PAH_paralogues.nex", "nexus")
h2bfs_paralogues = AlignIO.read("../input/Human_H2BFS_paralogues.nex", "nexus")
pah_orthologues_30 = AlignIO.read("../input/Human_PAH_orthologues_30.nex", "nexus")
