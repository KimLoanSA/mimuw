from Bio import Seq
from task2 import jukes_cantor
from task3 import distanceJC69

import matplotlib.pyplot as plt
import random


def random_seq(length):
    result = Seq.MutableSeq("")
    for _ in range(length):
        result.append(random.choice("AGCT"))

    return result.toseq()


def plot_distance(seq, mi):
    time = 1000
    print("plotting distance for mi:", mi)
    xs = range(time)
    ys = get_distances(seq, time, mi)

    plt.plot(xs, ys)
    plt.title("distance for mi: {}".format(mi))
    plt.xlabel("time")
    plt.ylabel("distance")
    plt.show()


def get_distances(seq, time, mi):
    ys = []
    act_seq = seq
    for _ in range(time):
        act_seq = jukes_cantor(act_seq, 1, mi)
        ys.append(distanceJC69(seq, act_seq))

    return ys


s_0 = random_seq(1000)
plot_distance(s_0, 10e-10)
plot_distance(s_0, 10e-5)
plot_distance(s_0, 10e-3)
plot_distance(s_0, 10e-2)
