
# bowtie index fastq_file write to screen
bowtie e_coli reads/e_coli_1000.fq

# bowtie: index fastq_file write to map file (-t timing stats)
bowtie -t e_coli reads/e_coli_1000.fq e_coli.map

# bowtie: -S generate sam file
bowtie -S e_coli reads/e_coli_10000snp.fq ec_snp.sam

# search existing index s_cerevisiae.ebwt (-c get read from command line instead of file)
bowtie -c s_cerevisiae ATTGTAGTTCGAGTAAGTAATGTGGGTTTG

# build new index: fasta file, index name
bowtie-build NC_002127.fna e_coli_O157_H7

# convert sam to bam
samtools view -bS -o ec_snp.bam ec_snp.sam

# sort bam file
samtools sort ec_snp.bam ec_snp.sorted

# call variants from sorted bam file
samtools pileup -cv -f genomes/NC_008253.fna ec_snp.sorted.bam

# change bowtie index
export BOWTIE_INDEXES="new_index_dir/"