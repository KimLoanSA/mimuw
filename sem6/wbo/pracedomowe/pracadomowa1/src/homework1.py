from Bio import SeqIO


file_path = "../input/yeast.fa"


def unique_probes(file_path, file_format="fasta"):
    complement_sequences = parse_file_and_get_complement_sequences(file_path, file_format)

    return find_minimal_probes_len(complement_sequences)


def parse_file_and_get_complement_sequences(file_path, file_format):
    seq_generator = SeqIO.parse(open(file_path), file_format)
    return [seq.reverse_complement().seq for seq in seq_generator]


# using bin search to find minimal k
def find_minimal_probes_len(sequences):
    left = 1
    # maximal size of the probe is length of the shortest sequence
    right = find_minimal_sequence_len(sequences)

    print("\n======= running binsearch =======")

    while left < right:
        mid = (left + right) // 2
        print("[binsearch step] left:", left, ", mid:", mid, ", right:", right)

        if do_unique_probes_of_len_k_exist(sequences, mid):
            right = mid
        else:
            left = mid + 1

    if do_unique_probes_of_len_k_exist(sequences, left):
        return left
    else:
        return -1


def find_minimal_sequence_len(sequences):
    return min(map(lambda seq: len(seq), sequences))


def do_unique_probes_of_len_k_exist(sequences, k):
    sequences_as_kmers = [kmers(seq, k) for seq in sequences]
    all_unique_kmers = set()
    all_nonunique_kmers = set()

    for seq_kmers in sequences_as_kmers:
        # deleting already known duplicates
        kmers_without_global_nonunique = seq_kmers - all_nonunique_kmers

        # finding duplication with foregoing unique
        duplications_with_unique = kmers_without_global_nonunique & all_unique_kmers

        # removing duplications from global unique
        all_unique_kmers = all_unique_kmers - duplications_with_unique

        # adding new duplication to global duplications
        all_nonunique_kmers = all_nonunique_kmers.union(duplications_with_unique)

        # removing duplications
        unique_kmers = kmers_without_global_nonunique - duplications_with_unique

        # upading global unique with new kmers
        all_unique_kmers = all_unique_kmers.union(unique_kmers)

    # checking that we can find a probe for every sequence (we know that the are globally unique)
    return all(map(lambda x: len(x & all_unique_kmers) > 0, sequences_as_kmers))


def kmers(seq, k):
    return set([seq[i:i + k] for i in range(len(seq) - k + 1)])


assert unique_probes("../input/test/test_fasta.fa") == 3
assert unique_probes("../input/test/test1.fa") == 1
assert unique_probes("../input/test/test2.fa") == -1

print("result:", unique_probes(file_path))