from homework_3_marcin_abramowicz import get_sequences_without_stop, recover_seq_from_translation_alignment, optimal_local_alignment, optimal_local_alignment_for_all_frames
from Bio import Seq
from Bio.SeqIO import parse


# get_sequences_without_stop
assert get_sequences_without_stop(Seq.Seq("AAACCCACTTGATGATGA")) == [Seq.Seq("AAACCCACT")]
assert get_sequences_without_stop(Seq.Seq("AAACCCACT")) == [Seq.Seq("AAACCCACT")]
assert get_sequences_without_stop("AAACCCACTTAA") == [Seq.Seq("AAACCCACT")]
assert get_sequences_without_stop("AAACCCACTTAG") == ["AAACCCACT"]

assert get_sequences_without_stop("AAACCCACTTAGAGGCGCGGCC") == [Seq.Seq("AAACCCACT"), Seq.Seq("AGGCGCGGC")]
assert get_sequences_without_stop("AAACCCACTTAAAGGCGCGGCC") == ["AAACCCACT", "AGGCGCGGC"]
assert get_sequences_without_stop("AAACCCACTTGAAGGCGCGGCC") == ["AAACCCACT", "AGGCGCGGC"]

assert get_sequences_without_stop("AAACCCACTTAGAGGCGCGGCTAAACATAG") == ["AAACCCACT", "AGGCGCGGC", "ACA"]
assert get_sequences_without_stop("ATGTCCGAATATCAGCCAAGTTTATTTGCTTTAAATCCAATGGGT") == ["ATGTCCGAATATCAGCCAAGTTTATTTGCTTTAAATCCAATGGGT"]

print("get_sequences_without_stop tests passed!")


# recover_seq_from_translation_alignment
seq1 = Seq.Seq("ATGGCTCTACCTCTG")
assert recover_seq_from_translation_alignment(seq1, seq1.translate(), 0, len(seq1.translate())) == seq1

assert recover_seq_from_translation_alignment(
    "ATGGCTCTACCTCTGATAAAACCTAAGGAGTCTGAGGAAAGCCACCTCGCTCTTTTGTCCAAGATCCACGTGTCCAAA",
    "--------MALPLIK--PKESEES-HLALLSKIH-VSK", 10, 21) == "CTACCTCTGATAAAA------CCTAAGGAGTCT"

print("recover_seq_from_translation_alignment tests passed!")


# optimal_local_alignment
local_alignments = optimal_local_alignment(Seq.Seq("TTGGCGAGCACA"), Seq.Seq("GATAGATAGCTAGCTAGTA"), -8, -8)
assert len(local_alignments) == 1
assert local_alignments[0].seq1 == Seq.Seq("TTGGCGAGC")
assert local_alignments[0].seq2 == Seq.Seq("CTAGCTAGT")

print("optimal_local_alignment tests passed!")


# optimal_local_alignment_for_all_frames
local_alignment_frames = optimal_local_alignment_for_all_frames(Seq.Seq("ATTGGCGAGCACA"), Seq.Seq("GATAGATAGCTAGCTAGTA"), -8, -8)
assert len(local_alignment_frames) == 1
assert local_alignment_frames[0].seq1 == Seq.Seq("TTGGCGAGC")
assert local_alignment_frames[0].seq2 == Seq.Seq("CTAGCTAGT")

local_alignment_frames_2 = optimal_local_alignment_for_all_frames(Seq.Seq("AAAA"), Seq.Seq("AAAA"), -6, -6)
assert len(local_alignment_frames_2) == 4
for a in local_alignment_frames_2:
    assert a.seq1 == Seq.Seq("AAA")
    assert a.seq2 == Seq.Seq("AAA")
    assert a.score == 4

print("optimal_local_alignment_for_all_frames tests passed!")


print("Example:")
print(optimal_local_alignment_for_all_frames(Seq.Seq("ATTGGCGAGCACA"), Seq.Seq("GATAGATAGCTAGCTAGTA"), -8, -8))


print("\n\n bzips & histones")

bzip_seqs = list(parse("../../lab3/files/bzips.fa", "fasta"))
histone_seqs = list(parse("../../lab3/files/histones.fa", "fasta"))


def testcross(index1, index2, d=-8., e=-8.):
    print("=================================================================")
    print(f"bzips[{index1}] & histone_seqs[{index2}]:")

    print("files:")
    print("seq1:", bzip_seqs[index1].seq)
    print("seq2:", histone_seqs[index2].seq)
    print("d:", d)
    print("e:", e)
    print("-----------------------------------------------------------------")

    result = optimal_local_alignment_for_all_frames(bzip_seqs[index1].seq, histone_seqs[index2].seq, d, e)

    for res in result:
        print("seq1:", res.seq1)
        print("seq2:", res.seq2)
        print("score:", res.score)
        print("-----------------------------------------------------------------")

    print("=================================================================")


testcross(0, 0)
testcross(0, 0, -0.002, -0.0001)


print(optimal_local_alignment_for_all_frames(Seq.Seq("GATCTGTAACCGCTTAGTAACACGGTATAAGC"), Seq.Seq("TCCAGCTCTCTTGGTAGTTGCAGGACCCACGT"), -6, -6))
print(optimal_local_alignment_for_all_frames(Seq.Seq("AAACCCAAA"), Seq.Seq("AAAAAAAAA"), -6, -6))

