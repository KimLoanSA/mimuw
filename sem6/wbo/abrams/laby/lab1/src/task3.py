from Bio import Seq

import itertools

# heura
def euler(kmers):
    graph = create_euler_graph(kmers)
    number_of_edges = get_number_of_edges(graph)
    result = []

    for key, val in graph.items():
        for path in paths(key, graph, [], number_of_edges):
            result.append(path)

    return map(create_sequence_from_path, result)


def create_euler_graph(kmers):
    graph = {}

    for kmer in kmers:
        add_kmer_to_euler_graph(kmer, graph)

    return graph


def add_kmer_to_euler_graph(kmer, graph):
    starting_node = kmer[:-1]
    ending_node = kmer[1:]

    add_to_graph_if_doesnt_contain(starting_node, graph)
    add_to_graph_if_doesnt_contain(ending_node, graph)

    graph[starting_node].append(ending_node)


def get_number_of_edges(graph):
    result = 0
    for key, val in graph.items():
        result = result + len(val)

    return result


def paths(v, graph, path, number_of_edges):
    neighbors = list(graph[v])

    if len(neighbors) == 0 and len(path) == number_of_edges:
        yield path[:]

    for w in neighbors:
        graph[v].remove(w)
        path.append((v, w))
        yield from paths(w, graph, path, number_of_edges)

        path.pop()
        graph[v].append(w)


def create_sequence_from_path(path):
    seq_array = list(map(lambda edge: Seq.MutableSeq(str(edge[0] + edge[1][-1])).toseq(), path))

    return create_sequence_form_permutation(seq_array)


# ------------------------------------------------------------
def hamilton(kmers):
    graph = create_hamilton_graph(kmers)
    result = []

    for permutation in list(itertools.permutations(graph)):
        if check_permutation(graph, permutation):
            result.append(create_sequence_form_permutation(permutation))

    return result


def create_hamilton_graph(kmers):
    graph = {}

    for kmer1 in kmers:
        for kmer2 in kmers:
            add_kmers_to_hamilton_graph(kmer1, kmer2, graph)

    return graph


def add_kmers_to_hamilton_graph(kmer1, kmer2, graph):
    if kmer1 != kmer2:
        add_to_graph_if_doesnt_contain(kmer1, graph)
        add_to_graph_if_doesnt_contain(kmer2, graph)

        if kmer1[1:] == kmer2[:-1]:
            graph[kmer1].append(kmer2)


def add_to_graph_if_doesnt_contain(node, graph):
    if node not in graph:
        graph[node] = []


def check_permutation(graph, permutation):
    for i in range(0, len(permutation) - 1):
        if permutation[i + 1] not in graph[permutation[i]]:
            return False

    return True


def create_sequence_form_permutation(permutation):
    seq = Seq.MutableSeq(str(permutation[0][0:-1]))
    for node in permutation:
        seq.append(str(node[-1]))

    return seq.toseq()


# == tests ==
def assert_graph(graph, expected_graph):
    assert len(graph) == len(expected_graph)

    for key, created_list in graph.items():
        expected_list = expected_graph[key]

        assert len(expected_list) == len(created_list)
        assert set(expected_list) == set(created_list)


example_kmers = {
    Seq.Seq("ATG"),
    Seq.Seq("TGG"),
    Seq.Seq("TGC"),
    Seq.Seq("GTG"),
    Seq.Seq("GGC"),
    Seq.Seq("GCA"),
    Seq.Seq("GCG"),
    Seq.Seq("CGT")
}

# -- euler --
assert set(euler(example_kmers)) == {Seq.Seq("ATGCGTGGCA"), Seq.Seq("ATGGCGTGCA")}
# create_euler_graph()
expected_euler_graph = {
    Seq.Seq("GT"): [Seq.Seq("TG")],
    Seq.Seq("TG"): [Seq.Seq("GC"), Seq.Seq("GG")],
    Seq.Seq("GC"): [Seq.Seq("CA"), Seq.Seq("CG")],
    Seq.Seq("CA"): [],
    Seq.Seq("GG"): [Seq.Seq("GC")],
    Seq.Seq("AT"): [Seq.Seq("TG")],
    Seq.Seq("CG"): [Seq.Seq("GT")]
}
created_euler_graph = create_euler_graph(example_kmers)

assert_graph(created_euler_graph, expected_euler_graph)

# -- hamilton --
# create_hamilton_graph()
expected_hamilton_graph = {
    Seq.Seq("TGG"): [Seq.Seq("GGC")],
    Seq.Seq("ATG"): [Seq.Seq("TGG"), Seq.Seq("TGC")],
    Seq.Seq("GGC"): [Seq.Seq("GCG"), Seq.Seq("GCA")],
    Seq.Seq("CGT"): [Seq.Seq("GTG")],
    Seq.Seq("TGC"): [Seq.Seq("GCG"), Seq.Seq("GCA")],
    Seq.Seq("GCG"): [Seq.Seq("CGT")],
    Seq.Seq("GCA"): [],
    Seq.Seq("GTG"): [Seq.Seq("TGG"), Seq.Seq("TGC")]
}
created_hamilton_graph = create_hamilton_graph(example_kmers)
assert_graph(created_hamilton_graph, expected_hamilton_graph)

assert set(hamilton(example_kmers)) == {Seq.Seq("ATGCGTGGCA"), Seq.Seq("ATGGCGTGCA")}