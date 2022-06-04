from Bio import pairwise2, Seq
from Bio.SubsMat import MatrixInfo
from collections import namedtuple
import itertools


def get_sequences_without_stop(seq):
    """
    Splits sequence into multiple by STOP codons

    :param seq: sequence to split
    :return: list with sequences without STOP codons
    """

    result = []
    stop_codons = {"TAG", "TAA", "TGA"}
    actual_seq = Seq.MutableSeq("")

    for i in range(0, len(seq), 3):
        if i + 2 < len(seq):  # we want to skip last incomplete codon
            actual_codon = seq[i:(i + 3)]

            if actual_codon in stop_codons:
                if len(actual_seq) > 0:
                    result.append(actual_seq.toseq())

                actual_seq = Seq.MutableSeq("")
            else:
                actual_seq.append(actual_codon[0])
                actual_seq.append(actual_codon[1])
                actual_seq.append(actual_codon[2])

    if len(actual_seq) > 0:
        result.append(actual_seq.toseq())

    return result


def recover_seq_from_translation_alignment(seq, translation, start, end):
    """
    Recovers DNA sequence for given translated sequence

    :param seq: base DNA sequence
    :param translation: translated base DNA sequence
    :param start: starting (inclusive) index of the optimal alignment
    :param end: ending (exclusive) index of the optimal alignment
    :return: DNA sequence (with empty spaces) which directly translates to :param translation
    """

    number_of_spaces = translation[:start].count("-")
    number_of_used_amino = start - number_of_spaces
    number_of_used_nucleotides = number_of_used_amino * 3

    current_seq = seq[number_of_used_nucleotides:]
    current_seq_index = 0

    result_seq = Seq.MutableSeq("")

    for amino in translation[start:end]:
        if amino != "-":
            result_seq.append(current_seq[current_seq_index])
            result_seq.append(current_seq[current_seq_index + 1])
            result_seq.append(current_seq[current_seq_index + 2])

            current_seq_index += 3
        else:
            result_seq.append("-")
            result_seq.append("-")
            result_seq.append("-")

    # self check
    # assert result_seq.toseq().translate() == translation[start:end]

    return result_seq.toseq()


def align_locally(seq1, seq2, d, e):
    """
    Returns all optimal local alignments using given sequences translations into amino acids
    (wraps pairwise2.align.localds) using blosum60 matrix and d and e penalties

    :param seq1: first sequence
    :param seq2: second sequence
    :param d: penalty for opening a new gap
    :param e: penalty for extending an existing gap
    :return: list with optimal alignments
    """

    alignments = pairwise2.align.localds(
        seq1.translate(),
        seq2.translate(),
        MatrixInfo.blosum60,
        d, e)

    LocalAlignment = namedtuple("LocalAlignment", ["seq1", "seq2", "score"])

    return list(map(lambda a:
                    LocalAlignment(
                        recover_seq_from_translation_alignment(seq1, a.seqA, a.start, a.end),
                        recover_seq_from_translation_alignment(seq2, a.seqB, a.start, a.end),
                        a.score),
                    alignments))


def filter_best_alignments(alignments):
    """
    Returns only alignments with the highest score

    :param alignments: list with alignments to filter
    :return: list with alignments with the highest score
    """

    sorted_alignments = sorted(alignments, key=lambda a: a.score, reverse=True)
    best_alignment = next(iter(sorted_alignments), None)

    return list(filter(lambda a: a.score == best_alignment.score, sorted_alignments))


def optimal_local_alignment(seq1, seq2, d, e):
    """
    Calculates all optimal local alignments using given sequences translations into amino acids
    using only current reading frame and split sequences by STOP codons

    :param seq1: first sequence
    :param seq2: second sequence
    :param d: penalty for opening a new gap
    :param e: penalty for extending an existing gap
    :return: list with all optimal alignments
    """

    all_seq1_without_stop = get_sequences_without_stop(seq1)
    all_seq2_without_stop = get_sequences_without_stop(seq2)

    all_alignments = [align_locally(seq1_without_stop, seq2_without_stop, d, e)
                      for seq1_without_stop in all_seq1_without_stop
                      for seq2_without_stop in all_seq2_without_stop]

    return filter_best_alignments(list(itertools.chain(*all_alignments)))


def optimal_local_alignment_for_all_frames(seq1, seq2, d=-8., e=-8.):
    """
    Calculates all optimal local alignments using given sequences translations into amino acids including all possible
    reading frames and sequences split by STOP codons

    :param seq1: first sequence
    :param seq2: second sequence
    :param d: penalty for opening a new gap (default=-8)
    :param e: penalty for extending an existing gap (default=-8)
    :return: list with all optimal alignments
    """

    all_seq1_frames = [seq1, seq1[1:], seq1[2:]]
    all_seq2_frames = [seq2, seq2[1:], seq2[2:]]

    all_alignments = [optimal_local_alignment(seq1_without_stop, seq2_without_stop, d, e)
                      for seq1_without_stop in all_seq1_frames
                      for seq2_without_stop in all_seq2_frames]

    return filter_best_alignments(list(itertools.chain(*all_alignments)))
