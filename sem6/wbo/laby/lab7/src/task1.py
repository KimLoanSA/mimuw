import numpy as np
import matplotlib.pyplot as plt
from hmmlearn import hmm
from Bio import SeqIO


nucleotide_encoder1 = {"A": 0, "C": 1, "G": 2, "T": 3}

seq = next(iter(SeqIO.parse(open("../input/cpg.fa"), "fasta")))
input_seq1 = np.vectorize(lambda s: nucleotide_encoder1[s])(np.array(seq).reshape(-1, 1))

model1 = hmm.MultinomialHMM(n_components=2)
model1.n_features_ = 4
model1.fit(input_seq1)

plt.plot(input_seq1, "r*", label="thrown values")
plt.yticks(range(4), ["1", "2", "3", "4"])
plt.plot(model1.predict(input_seq1), "g-", label="reconstructed states")
plt.legend(loc="upper left")
plt.show()


all_doubles = [a + b for a in "ACGT" for b in "ACGT"]
def nucleotide_encoder2(n):
    return all_doubles.index(n)

input_seq2 = np.array([nucleotide_encoder2(str(seq.seq[i:i+2])) for i in range(0, len(seq), 2)]).reshape(-1, 1)


model2 = hmm.MultinomialHMM(n_components=2)
model2.n_features_ = 4
model2.fit(input_seq2)

plt.plot(input_seq2, "r*", label="thrown values")
plt.yticks(range(4), ["1", "2", "3", "4"])
plt.plot(model2.predict(input_seq2), "g-", label="reconstructed states")
plt.legend(loc="upper left")
plt.show()

# print(monety.emissionprob_)
# print(monety.transmat_)
# unfair_casino = hmm.MultinomialHMM(n_components=2)
# unfair_casino.n_features_ = 4
# unfair_casino.startprob_ = np.array([0.5, 0.5])
# unfair_casino.transmat_ = np.array([[0.44786276, 0.55213724],
#                                     [0.45736353, 0.54263647]])
# unfair_casino.emissionprob_ = np.array(
#     [[0.2861786, 0.23038389, 0.29145469, 0.19198282],
#      [0.14738492, 0.34379027, 0.2834401, 0.22538471]])
#
# plt.plot(input_seq, "r*", label="thrown values")
# plt.yticks(range(4) ,["1","2",'3',"4"])
# plt.plot(unfair_casino.decode(input_seq)[1],"g-",label="reconstructed states")
# plt.legend(loc="upper left")
# plt.show()

