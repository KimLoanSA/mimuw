from Bio.SeqIO import parse


bzip_seqs = list(parse("../files/bzips.fa", "fasta"))
histone_seqs = list(parse("../files/histones.fa", "fasta"))

print(bzip_seqs)
print(histone_seqs)

print(bzip_seqs[0].seq.translate())
print(histone_seqs[0].seq.translate())
