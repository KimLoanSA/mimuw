import re
import subprocess

from Bio import SeqIO, SearchIO
from Bio.Blast import NCBIXML
from Bio.UniProt.GOA import gafiterator
from goatools import obo_parser


print("PART 1.")
print("================\n\n")

print("Reading e cola genes...")
e_cola_genes_file = "../files/Escherichia_coli.HUSEC2011CHR1.cdna.all.fa"
e_cola_genes = list(SeqIO.parse(open(e_cola_genes_file), "fasta"))
print("Reading done!")
print("First 3 sequences:\n\n{}\n\n{}\n\n{}\n".format(e_cola_genes[0], e_cola_genes[1], e_cola_genes[2]))
print("--------\n")

print("Translating genes...")
e_cola_proteins_file = "../files/e_cola_proteins.fa"
e_cola_proteins = list(map(lambda seq: seq.translate(id=seq.id, description=seq.description), e_cola_genes))
print("Translating done!")
print("Saving to {}".format(e_cola_proteins_file))
with open(e_cola_proteins_file, "w") as output_handle:
    SeqIO.write(e_cola_proteins, output_handle, "fasta")
print("First 3 sequences: {}".format(e_cola_proteins[0:3]))
print("--------\n")

print("Creating BLAST database...")
subprocess.run(["makeblastdb",
                "-in", e_cola_proteins_file,
                "-dbtype", "prot"],
               check=True,
               text=True)
print("Creating BLAST database done!")
print("--------\n")

print("Querying BLAST...")
proteins_fragments_file = "../files/protein_fragments.fa"
proteins_fragments_result_file = "../files/protein_fragments_result.xml"
e_value = 1e-90
subprocess.run(["blastp",
                "-query", proteins_fragments_file,
                "-db", e_cola_proteins_file,
                "-evalue", str(e_value),
                "-num_threads", "4",
                "-outfmt", "5",
                "-out", proteins_fragments_result_file],
               check=True,
               text=True)
print("Querying BLAST done!")
print("--------\n")

print("Searching found proteins...")
result_handle = open(proteins_fragments_result_file)
blast_record = NCBIXML.parse(result_handle)
found_proteins_descriptions = []
for a in blast_record:
    for alignment in a.alignments:
        for hsp in alignment.hsps:
            found_proteins_descriptions.append(alignment.hit_def)

found_proteins = list(filter(lambda seq: seq.description in found_proteins_descriptions, e_cola_proteins))
print("Searching found proteins done!")
print("First 3 sequences:\n\n{}\n\n{}\n\n{}\n".format(found_proteins[0], found_proteins[1], found_proteins[2]))

found_proteins_file = "../files/e_cola_proteins_found_fragments.fa"
print("Saving to {}\n".format(found_proteins_file))
with open(found_proteins_file, "w") as output_handle:
    SeqIO.write(found_proteins, output_handle, "fasta")
print("--------\n")


print("\nPART 2.")
print("================\n\n")

print("Indexing hmm (hmmpress)...")
hmm_file = "../files/Pfam-A.hmm"
subprocess.run(["hmmpress", hmm_file],
               check=True,
               text=True)
print("Indexing hmm (hmmpress) done!")

print("Scanning hmm (hmmscan)...")
scan_result_file = "../files/pfam_scan_result"
subprocess.run(["hmmscan",
                "--tblout", scan_result_file,
                hmm_file, found_proteins_file],
               check=True,
               text=True)
print("Scanning hmm (hmmscan) done!")

print("Reading scan results...")
with open(scan_result_file) as handle:
    scan_result = list(SearchIO.parse(handle, 'hmmer3-tab'))
found_domains = set()
for result in scan_result:
    domains = list(map(lambda hit: hit.id, result.hits))
    found_domains.update(set(domains))
print("Reading scan results done!")

print("Found domains:\n")
for domain in sorted(found_domains):
    print("- {}".format(domain))
print("--------\n")


print("\nPART 3.")
print("================\n\n")

print("Reading gaf file...")
gaf_file = "../files/ecocyc.gaf"
with open(gaf_file) as handle:
    gaf = list(gafiterator(handle))
print("Reading gaf file done!")
print("First 3 records:\n\n{}\n\n{}\n\n{}".format(gaf[0], gaf[1], gaf[2]))

print("Reading GO file...")
go_file = "../files/go.obo"
godag = obo_parser.GODag(obo_file=go_file)
print("Reading GO file done!")


print("Finding GO terms...")
gene_symbol_re = re.compile("gene_symbol:([^ ]*)")

for protein in found_proteins:
    protein_gene_symbol_match = gene_symbol_re.search(protein.description)
    protein_gene_symbol = protein_gene_symbol_match.group(1) if protein_gene_symbol_match else ""

    gaf_entries = filter(lambda gaf_entry: gaf_entry['DB_Object_Symbol'] == protein_gene_symbol, gaf)
    go_ids = map(lambda gaf_entry: gaf_entry['GO_ID'], gaf_entries)

    print("* protein: {}".format(protein.id))

    for go_id in go_ids:
        all_paths = godag.paths_to_top(go_id)
        for path in all_paths:
            for term in path:
                print("- namespace: {} | GO id: {} | name: {}".format(term.namespace, term.id, term.name))
        print()
    print("---\n\n")

print("Finding GO terms done!")


print("\nPART 4.")
print("================\n\n")
print("Not done: (")
