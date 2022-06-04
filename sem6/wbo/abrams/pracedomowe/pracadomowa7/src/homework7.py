import numpy as np
from hmmlearn import hmm
from Bio import SeqIO
from collections import namedtuple


all_nucleotide_doubles = [a + b for a in "ACGT" for b in "ACGT"]


def encode(n):
    return all_nucleotide_doubles.index(n)


def encode_seq(seq):
    return np.array([encode(str(seq[i:i + 2])) for i in range(0, len(seq) - 1)]).reshape(-1, 1)


def predict(model, seq):
    encoded_seq = encode_seq(seq.seq)
    predicted = model.predict(encoded_seq)

    seq_len = len(predicted)
    state_0_count = np.sum(predicted == 0)
    state_1_count = np.sum(predicted == 1)

    prediction_result = namedtuple("prediction_result",
                                   ["seq_len",
                                    "state_0_count", "state_0_percentage",
                                    "state_1_count", "state_1_percentage"])
    return prediction_result(seq_len, state_0_count, state_0_count / seq_len, state_1_count, state_1_count / seq_len)


learning_seq = next(iter(SeqIO.parse(open("../input/cpg.fa"), "fasta")))
testing_seqs = list(SeqIO.parse(open("../input/cpg_test.fa"), "fasta"))

hmmModel = hmm.MultinomialHMM(n_components=2, n_iter=10)
hmmModel.n_features_ = 16
hmmModel.fit(encode_seq(learning_seq.seq))

print(all_nucleotide_doubles)
print(hmmModel.emissionprob_)
print(hmmModel.transmat_)

file = open("../output/homework7output.txt", "w")

for test_seq in testing_seqs:
    predicted = predict(hmmModel, test_seq)
    file.write("sequence: {seq}, len: {len}\n"
               "state 0 count: {state_0_count}, state 0 percentage: {state_0_percentage}\n"
               "state 1 count: {state_1_count}, state 1 percentage: {state_1_percentage}\n"
        .format(
            seq=test_seq.description,
            len=predicted.seq_len,
            state_0_count=predicted.state_0_count,
            state_0_percentage=predicted.state_0_percentage,
            state_1_count=predicted.state_1_count,
            state_1_percentage=predicted.state_1_percentage))

file.close()

# domniemuje na podstawie prawdopodobienstw, ze stan 1 jest odpowiedzialny za wyspy
