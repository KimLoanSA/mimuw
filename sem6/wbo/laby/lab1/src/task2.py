from Bio import Seq


def kmers_with_complementary(s, k):
    return kmers(s, k).union(kmers(s.reverse_complement(), k))


def kmers(seq, k):
    return set([seq[i:i + k] for i in range(len(seq) - k + 1)])


# == tests ==
example_seq = Seq.Seq("ATAGGCAAAGGCT")

# kmers()
assert kmers(example_seq, 3) == {
    Seq.Seq("ATA"),
    Seq.Seq("AGG"),
    Seq.Seq("AAA"),
    Seq.Seq("GCA"),
    Seq.Seq("GGC"),
    Seq.Seq("GCT"),
    Seq.Seq("AAG"),
    Seq.Seq("CAA"),
    Seq.Seq("TAG")
}

assert kmers(example_seq, 7) == {
    Seq.Seq("ATAGGCA"),
    Seq.Seq("TAGGCAA"),
    Seq.Seq("AGGCAAA"),
    Seq.Seq("GGCAAAG"),
    Seq.Seq("GCAAAGG"),
    Seq.Seq("CAAAGGC"),
    Seq.Seq("AAAGGCT")
}

# kmers_with_complementary()
assert kmers_with_complementary(example_seq, 3) == {
    Seq.Seq("ATA"),
    Seq.Seq("AGG"),
    Seq.Seq("AAA"),
    Seq.Seq("GCA"),
    Seq.Seq("GGC"),
    Seq.Seq("GCT"),
    Seq.Seq("AAG"),
    Seq.Seq("CAA"),
    Seq.Seq("TAG"),

    Seq.Seq("AGC"),
    Seq.Seq("GCC"),
    Seq.Seq("CCT"),
    Seq.Seq("CTT"),
    Seq.Seq("TTT"),
    Seq.Seq("TTG"),
    Seq.Seq("TGC"),
    Seq.Seq("GCC"),
    Seq.Seq("CCT"),
    Seq.Seq("CTA"),
    Seq.Seq("TAT")
}

assert kmers_with_complementary(example_seq, 7) == {
    Seq.Seq("ATAGGCA"),
    Seq.Seq("TAGGCAA"),
    Seq.Seq("AGGCAAA"),
    Seq.Seq("GGCAAAG"),
    Seq.Seq("GCAAAGG"),
    Seq.Seq("CAAAGGC"),
    Seq.Seq("AAAGGCT"),

    Seq.Seq("AGCCTTT"),
    Seq.Seq("GCCTTTG"),
    Seq.Seq("CCTTTGC"),
    Seq.Seq("CTTTGCC"),
    Seq.Seq("TTTGCCT"),
    Seq.Seq("TTGCCTA"),
    Seq.Seq("TGCCTAT")
}