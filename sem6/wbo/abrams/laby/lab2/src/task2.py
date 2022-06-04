from Bio import Seq

import random


# ----- jukes-cantor -----
def jukes_cantor(seq, time, mi):
    result = seq
    for _ in range(time):
        result = mutate_jukes_cantor(result, mi)

    return result


def mutate_jukes_cantor(seq, mi):
    result_seq = map(lambda e: mutate_jukes_cantor_elem(e, mi), seq)

    return Seq.MutableSeq(result_seq).toseq()


def mutate_jukes_cantor_elem(elem, mi):
    if random.random() < mi:
        return random.choice("ACGT")

    return elem


# ----- kimura -----
def kimura(seq, time, ratio, mi):
    result = seq
    for _ in range(time):
        result = mutate_kimura(result, ratio, mi)

    return result


def mutate_kimura(seq, ratio, mi):
    result_seq = map(lambda e: mutate_kimura_elem(e, ratio, mi), seq)

    return Seq.MutableSeq(result_seq).toseq()


def mutate_kimura_elem(elem, ratio, mi):
    if random.random() < mi:
        if random.randint(0, ratio) == 0:
            return mutate_kimura_elem_transition(elem)
        else:
            return mutate_kimura_elem_transversion(elem)

    return elem


def mutate_kimura_elem_transition(elem):
    if elem in {"A", "G"}:
        return random.choice("AG")
    else:
        return random.choice("CT")


def mutate_kimura_elem_transversion(elem):
    if elem in {"A", "G"}:
        return random.choice("CT" + elem)
    else:
        return random.choice("AG" + elem)


# ----- felsenstein -----
def felsenstein(seq, time, prob, mi):
    result = seq
    for _ in range(time):
        result = mutate_felsenstein(result, prob, mi)

    return result


def mutate_felsenstein(seq, prob, mi):
    result_seq = map(lambda e: mutate_felsenstein_elem(e, prob, mi), seq)

    return Seq.MutableSeq(result_seq).toseq()


def mutate_felsenstein_elem(elem, prob, mi):
    if random.random() < mi:
        index = distribution_random(prob)

        return "AGCT"[index]

    return elem


def distribution_random(prob):
    d = [sum(prob[:i]) for i in range(1, len(prob) + 1)]
    x = random.random()

    if x < sum(prob):
        for i, t in enumerate(d):
            if x < t:
                return i

    return None
