from Bio import motifs


background = {'A': 0.3, 'C': 0.2, 'G': 0.2, 'T': 0.3}
pseudocounts = {'A': 0.6, 'C': 0.4, 'G': 0.4, 'T': 0.6}


def consensus(sequence_list, motif_length):
    first_seq = sequence_list[0]
    best_hit_value, best_hit_motif = 0, None

    for idx in range(len(first_seq) - motif_length + 1):
        current_motif = motifs.create([first_seq[idx:idx+motif_length].seq])

        value, modif = _consensus_for_given_start(current_motif, sequence_list[1:], motif_length)

        if value > best_hit_value:
            best_hit_value = value
            best_hit_motif = modif

    return best_hit_value, best_hit_motif


def _consensus_for_given_start(input_motif, sequence_list, motif_length):
    best_hit_value, best_hit_motif, best_hit_index = 0, input_motif, 0

    for seq_idx, seq in enumerate(sequence_list):
        for idx in range(len(seq) - motif_length + 1):
            subseq = seq[idx:idx+motif_length]

            current_motif = motifs.create(input_motif.instances + [subseq.seq])
            pwm = current_motif.counts.normalize(pseudocounts=pseudocounts)
            value = pwm.log_odds(background).std(background)

            if value > best_hit_value:
                best_hit_value = value
                best_hit_motif = current_motif
                best_hit_index = seq_idx

    if len(sequence_list) == 1:

        return best_hit_value, best_hit_motif

    sequence_list_without_best = sequence_list[:best_hit_index] + sequence_list[best_hit_index+1:]
    return _consensus_for_given_start(best_hit_motif, sequence_list_without_best, motif_length)
