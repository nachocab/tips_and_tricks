# Bowtie aligns reads to a genome
# Tophat aligns reads to a genome (using bowtie) and then identifies splice junctions between the exons found
# Cufflinks "assembles transcripts, estimates their abundances, and tests for differential expression and regulation in RNA-Seq samples."

# cuffdiff tries to answer the question if there is a difference in a transcript expression in between two samples (biological, not technical, replicates).
# edgeR and DESeq try to answer if the difference in total expression (total count of a gene including all its isoforms) is significant and not due to biological variability. (useful to see the effects of the treatment: drug, gene knockdown, mutation, different tissue type)

# awesome way to query gene information given a genbank_id (returns a JSON object)
http://mygene.info/doc
http://mygene.info/query?q=cdk2+AND+species:human

# query longitudinal_fc, but then you still have to add the header
diff_lines -u -t ~/gene_info/gene_sets/TNF_domain.txt -q virus/lassa/calculations/PBMC/longitudinal_fc_all_by_gene_name_lassa-PBMC-day_3_vs_day_0.csv -f1 -i"," -k0 | awk 'NR>1' > misc/TNF_genes.txt

# download hg19 905MB
wget http://hgdownload.cse.ucsc.edu/goldenPath/hg19/bigZips/chromFa.tar.gz
gunzip  chromFa.tar.gz
tar -xvf chromFa.tar
rm -f *random*
rm -f *Un*
rm -f *hap*
cat chr*.fa > hg19.fa

# download interpro names
wget -O ~/gene_info/misc/interpro_names.txt "ftp://ftp.ebi.ac.uk/pub/databases/interpro/names.dat"

# download uniprot raw info
wget -O raw_uniprot.txt "http://www.uniprot.org/uniprot/?query=organism%3a9606+keyword%3a1185&force=yes&format=tab&columns=id,reviewed,protein%20names,genes,organism,interpro,go"

# download uniprot gene symbols
wget -O uniprot_uniprot_id_to_gene_symbol.txt "http://www.uniprot.org/uniprot/?query=taxonomy%3a9606&force=yes&format=tab&columns=id,genes"
awk -v OFS="," '$1 != "" && $2 != "" && NR > 1{print $1, $2}' uniprot_uniprot_id_to_gene_symbol.txt > first_filter_uniprot_id_to_gene_symbol.txt

# download uniprot genebank_ids
wget -O uniprot_uniprot_id_to_genebank_id.txt "http://www.uniprot.org/uniprot/?query=taxonomy%3a9606&force=yes&format=tab&columns=id,database(GENEID)"
awk -v OFS="," '$1 != "" && $2 != "" && NR > 1{print $1, $2}' uniprot_uniprot_id_to_genebank_id.txt | awk -F";" '{print $1}' > first_filter_uniprot_id_to_genebank_id.txt

# make translator hsa_genbank_id_to_gene_symbol: Use Homo_sapiens.gene_info instead of hsa_gene_info because we want different genbank_ids to point to the same gene_symbol
awk -v OFS="," '{print $2,$3}' ../Homo_sapiens.gene_info > translator/hsa_genbank_id_to_gene_symbol.txt

# get all gene symbols with a specific domain
grep IPR006052 raw_uniprot.txt | cut -f1 | pbcopy

# download uniprot mappings
wget -O uniprot_mappings.gz ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/idmapping.dat.gz
awk -v OFS="," '$2=="GeneID"{print $1,$3}' ../raw_uniprot_mapping.txt > ../translator/uniprot_id_to_genbank_id.txt
cat ../raw_uniprot_mapping.txt | awk -v OFS="," '$2=="NCBI_TaxID" && $3=="9606"{print $1}' > hsa_uniprot_ids.txt

awk -F, -v translator="hsa_uniprot_ids.txt" '
    BEGIN{
        while (getline < translator) {
            translations[$1] = $2
        }
        close(translator)
    }
    {
        if ($1 in translations){
            print
        }
    }
' ../translator/uniprot_id_to_genbank_id.txt > ../translator/hsa_uniprot_id_to_genbank_id.txt

# interesting gene information
String DB
wikigenes
Gene Cards
BioGRID

# TRANSCRIPTION FACTOR binding sites
http://www.sabiosciences.com/chipqpcrsearch.php?species_id=0&factor=c-Fos&gene=&nfactor=n&ninfo=n&ngene=n&B2=Search
pbpaste | cut -f2 > temp
translate -q temp -r > temp2
gene_info -q temp2 -k gene_symbol
mv temp2 ../gene_sets/AP-1-binding-site_SabioSciences.txt
mv temp2 ../gene_sets/IRF2-binding-site_SabioSciences.txt
mv temp2 ../gene_sets/IRF7-binding-site_SabioSciences.txt

# tagger
cd connor
cd misc/d3
tagger -q rescued_gene_names.txt > rescued_genes_tags.csv

# download all viruses from genbank
wget ftp://ftp.ncbi.nih.gov/genomes/Viruses/all.fna.tar.gz

# download lassa
wget ftp://ftp.ncbi.nih.gov/genomes/Viruses/Lassa_virus_uid14864/NC_004296.faa
wget ftp://ftp.ncbi.nih.gov/genomes/Viruses/Lassa_virus_uid14864/NC_004297.faa

# download marburg
wget ftp://ftp.ncbi.nih.gov/genomes/Viruses/Lake_Victoria_marburgvirus_uid15199/NC_001608.faa

# download ebola
wget ftp://ftp.ncbi.nih.gov/genomes/Viruses/Bundibugyo_ebolavirus_uid51245/NC_014373.faa

# bowtie
bowtie -p $NSLOTS -a --best --strata -v 2 -t --trim3 1 --trim5 1 file_index file.fastq file.bow
file_index # looks in the current folder, the indexes subdirectory of the bowtie executable, then on BOWTIE_INDEXES
-a # report all valid alignments per read. If more than one exists, and using --best
-p $NSLOTS # number of parallel threads
-v 2 # report alignments with at most 2 mm, ignore -e, -l and -n (stratum is the number of mm in the entire alignment)
-n 2 # report alignments with at most 2 mm in their seed of length -l, ignore -v
-t   # print wall-clock time required in each phase
--trim3 1 # trim 1 base from low-quality (right) end of each read before aligning
--trim5 1 # trim 1 base from high-quality (left) end of each read before aligning
--best # without best, it reports the first valid alignment. Stratum (number of mm in the seed region) trumps quality: a 1mm alignment with high Phred score is preferred to a 2mm where both positions have low Phred score.
--strata # only show the best stratum (1mm)
-c AATCGTCCGGAT # query a read manually
--supress 1,5,6,7 # don't show specified columns
-k 3 # report up to 3 valid alignments

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

# grep microarrays
grep COX4NB ../../virus/lassa/calculations/PBMC/all_probes_lassa-PBMC-day_3_vs_day_0.txt ../../virus/marburg/calculations/PBMC/all_probes_marburg-PBMC-day_3_vs_day_0.txt

# translator pattern: query,target. Ex: EN_to_ES_translator.txt
# whale,ballena
# rhinocerous,rinoceronte
