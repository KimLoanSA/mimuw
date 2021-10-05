from Bio import Seq
import numpy as np
import math


def distanceJC69(seq1, seq2):
    seq_len = len(seq1)
    number_of_mutations = get_number_of_mutations(seq1, seq2)
    p = number_of_mutations / seq_len

    return -3 / 4 * np.log(1 - (p * 4 / 3))


def get_number_of_mutations(seq1, seq2):
    mutations_marked = map(lambda e1, e2: e1 != e2, seq1, seq2)
    return sum(mutations_marked)


# == tests ==
# get_number_of_mutations
assert get_number_of_mutations(Seq.Seq("ACCT"), Seq.Seq("ACCT")) == 0
assert get_number_of_mutations(Seq.Seq("ACGT"), Seq.Seq("ACCT")) == 1
assert get_number_of_mutations(Seq.Seq("ACGT"), Seq.Seq("CGAA")) == 4

# distanceJC69
expected_result = 0.304098831081123286483
result = distanceJC69(Seq.Seq("ACGT"), Seq.Seq("ACCT"))
assert math.isclose(result, expected_result, rel_tol=1e-15)
