from Bio import pairwise2
from Bio.SeqIO import parse
from statistics import mean


bzip_seqs = list(map(lambda seq: str(seq.seq), list(parse("../files/bzips.fa", "fasta"))))
histone_seqs = list(map(lambda seq: str(seq.seq), list(parse("../files/histones.fa", "fasta"))))


def calculate_simple_best_global_and_local_alignment_scores(seq1, seq2):
    match = 1
    mismatch = -1

    global_alignment = next(iter(pairwise2.align.globalmx(seq1, seq2, match, mismatch)), None)
    local_alignment = next(iter(pairwise2.align.localmx(seq1, seq2, match, mismatch)), None)

    global_score = None if global_alignment is None else global_alignment.score
    local_score = None if local_alignment is None else local_alignment.score

    return global_score, local_score


def calculate_affine_best_global_and_local_alignment_scores(seq1, seq2):
    match = 1
    mismatch = -1
    opening = -1.0
    extending = -0.5

    global_alignment = next(iter(pairwise2.align.globalms(seq1, seq2, match, mismatch, opening, extending)), None)
    local_alignment = next(iter(pairwise2.align.localms(seq1, seq2, match, mismatch, opening, extending)), None)

    global_score = 0 if global_alignment is None else global_alignment.score
    local_score = 0 if local_alignment is None else local_alignment.score

    return global_score, local_score


def calculate_avg_for_sequences(seqs1, seqs2):
    simple_scores = [calculate_simple_best_global_and_local_alignment_scores(seqs1[i], seqs2[j])
                     for i in range(0, len(seqs1))
                     for j in range(i+1, len(seqs2))]
    affine_scores = [calculate_affine_best_global_and_local_alignment_scores(seqs1[i], seqs2[j])
                     for i in range(0, len(seqs1))
                     for j in range(i+1, len(seqs2))]

    simple_global_scores, simple_local_scores = zip(*simple_scores)
    affine_global_scores, affine_local_scores = zip(*affine_scores)

    return mean(simple_global_scores), mean(simple_local_scores), mean(affine_global_scores), mean(affine_local_scores)


print("bzip & bzip", calculate_avg_for_sequences(bzip_seqs, bzip_seqs))
print("histone & histone", calculate_avg_for_sequences(histone_seqs, histone_seqs))
print("histone & bzip", calculate_avg_for_sequences(histone_seqs, bzip_seqs))
