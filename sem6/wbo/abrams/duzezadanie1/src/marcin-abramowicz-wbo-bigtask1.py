import pylab

from Bio import AlignIO
from Bio import Entrez
from Bio import SeqIO
from Bio import Phylo
from Bio.Align.Applications import ClustalwCommandline
from Bio.Phylo.TreeConstruction import DistanceCalculator
from Bio.Phylo.TreeConstruction import DistanceTreeConstructor


Entrez.email = "ma406058@students.mimuw.edu.pl"
Entrez.tool = "testing"


def fetch_protein(protein_id):
    """
    Function fetches protein with given id from the Entrez protein database
    :param protein_id: protein id
    :return: fetched sequence
    """
    handle = Entrez.efetch(db="protein", id=protein_id, rettype="fasta")
    return SeqIO.read(handle, "fasta")


def align_proteins_and_draw_tree(proteins, file_name, matrix, draw_ascii=False):
    """
    Function aligns given proteins using `file_name` to store temporary alignment files.
    :param proteins: proteins to align
    :param file_name: temporary files name
    :param matrix: calculator matrix
    :param draw_ascii: if true, tree will be drawn with draw_ascii (useful while running on docker),
        otherwise tree will be drawn with default draw function
    """
    print("\n====================================================================")
    print("Alignment and phylogenetic tree for: {name}".format(name=file_name))
    save_proteins_to_file(proteins, file_name)
    alignment = aline_proteins(file_name)
    create_and_draw_tree(alignment, matrix, draw_ascii)


def save_proteins_to_file(proteins, file_name):
    with open(file_name + ".fasta", "w") as output_handle:
        SeqIO.write(proteins, output_handle, "fasta")


def aline_proteins(file_name):
    clustalw = ClustalwCommandline(infile=file_name + ".fasta")
    clustalw()
    return AlignIO.read(file_name + ".aln", "clustal")


def create_and_draw_tree(alignment, matrix, draw_ascii):
    calculator = DistanceCalculator(matrix)
    tree_constructor = DistanceTreeConstructor(calculator, method="nj")
    tree = tree_constructor.build_tree(alignment)

    if draw_ascii:
        Phylo.draw_ascii(tree)
    else:
        Phylo.draw(tree)
        pylab.show()


covid2019_s_protein_id = "QHR63290"
covid2019_s_protein = fetch_protein(covid2019_s_protein_id)

sars_s_protein_id = "AAR33050"
sars_s_protein = fetch_protein(sars_s_protein_id)

bat_coronavirus_s_protein_id = "QTJ30135"
bat_coronavirus_s_protein = fetch_protein(bat_coronavirus_s_protein_id)

mers_s_protein_id = "QGW51401"
mers_s_protein = fetch_protein(mers_s_protein_id)

polish_surface_glycoprotein_ids = ["QPF48056", "QJZ28251", "QPF49316", "QPF49172"]
polish_surface_glycoprotein = [fetch_protein(polish_id) for polish_id in polish_surface_glycoprotein_ids]


print("====================================================================")
print("Legend:")
print("Covid 2019 S protein id: '{id}'".format(id=covid2019_s_protein_id))
print("SARS 2002 S protein id: '{id}'".format(id=sars_s_protein_id))
print("bat coronavirus S protein id: '{id}'".format(id=bat_coronavirus_s_protein_id))
print("MERS S protein id: '{id}'".format(id=mers_s_protein_id))
print("Coronavirus surface glycoprotein from poland: {ids}".format(ids=polish_surface_glycoprotein_ids))
print("====================================================================")
print("====================================================================")


align_proteins_and_draw_tree(
    [covid2019_s_protein, sars_s_protein, bat_coronavirus_s_protein, mers_s_protein],
    "species",
    "blosum62"
)
align_proteins_and_draw_tree(
    polish_surface_glycoprotein,
    "covid",
    "blosum62"
)
align_proteins_and_draw_tree(
    [covid2019_s_protein, sars_s_protein, bat_coronavirus_s_protein, mers_s_protein] + polish_surface_glycoprotein,
    "all",
    "blosum62"
)
