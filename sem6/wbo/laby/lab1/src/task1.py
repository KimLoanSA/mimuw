from Bio import SeqIO

seq_generator = SeqIO.parse(open("../input/test_fasta.fa"), "fasta")

sequences = [seq.reverse_complement(id=seq.id + "_complement", description="") for seq in seq_generator]

with open("../output/task1.fasta", "w") as output_handle:
    SeqIO.write(sequences, output_handle, "fasta")