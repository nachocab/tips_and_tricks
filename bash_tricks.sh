# to apply changes in bash profile to current terminal window
source ~/.bash_profile

# to rename extensions
  find /path/to/images -name '*.JPG' -exec bash -c 'mv "$1" "${1/%.JPG/.jpg}"' -- {} \;
  
# to use all results from find as a list of argumnts (use "+")  
  find ../../../Genomes -name "*[^s].fna" -exec ./genoming.pl {} + > output
  
# remove a char from a file (in this case, to join lines)
tr -d "\n" < my_file  # fastest?
perl -p -e 's/\n//' my_file
sed -e ':a' -e 'N' -e '$!ba' -e 's/\n//g' my_file # slowest
#ex:
    tail -n +2 my_file.fna | tr -d "\n"
# otro TR
echo "PACO" | tr "A-Z" "a-z"
    
  
# show longest repeating sequences in a genome (at least 6 chars long)
tail -n +2 ../../../Genomes/whole/Clostridium\ perfringens.fna | tr -d "\n" | grep -oE (.)\1{5,} | sort -r | head  
# even better, It's better to grep a long line than many short lines:
    merge_fasta costridium.fna | grep -oE [ACGT]{3,}

# count number of matches
echo aaaa | grep -o a | wc -l 

# get the username of every system file
ps aux | cut -d " " -f 1

# sort by column. -t delimiter -n numeric -k field number
cat output | sort -n -k2 -t: | pretty

# remove duplicated lines
sort -u
sort | uniq

# show duplicates
sort | uniq -d

# delete a line in a file containing a string
sed '/awk/d' filename.txt

# change file extension of a group of files
for f in *.fa; do mv "$f" "`basename $f .fa`.fna"; done;

# remove from basename of multiple files
for i in *.fna
    do j=`echo $i | sed 's/hs_alt_HuRef_//g'`
    mv "$i" "$j"
done

# rename spaces to underscores
for f in *.fna ; do mv "$f" $( echo $f | tr ' ' '_' ) ; done

# find in current directory
find . -iname "*.txt"
find . -type f -maxdepth 1 "*.txt"

# prepend to a file (with process substitution)
header="T1\tT2\tT3\tI1\tN1\tN2\tN4" 
exec 3<> $output_patient1 && awk -v TEXT="$header" 'BEGIN {print TEXT}{print}' $output_patient1 >&3
# or - not tested
cat header.txt <(cat my.txt)

# direct a single stream of input (ls) to multiple readers (grep & wc) without using temporary files
ls |tee >(grep xxx |wc >xxx.count) >(grep yyy |wc >yyy.count) |grep zzz |wc >zzz.count

#en vez de mv /a/long/path/foo /a/long/path/bar
mv /a/long/path/{foo,bar}

#copia seguridad
mv /a/long/path/foo{,.bak}

#listar por fecha modif: r es reverse, t by modif, h show Kb,Mb
ls -lhtr

# sed return whatever between two regexps (first inclusive)
sed -n '/Q927F2/,/>/p' uniref50.fasta | sed '$d'

# execute previous command
# Type ! followed by the starting few letters of the command that you would like to re-execute.
sed -n '/Q927F2/,/>/p' uniref50.fasta | sed '$d'
!s

# use all arguments of previous command
!*
# use last argument of previous command
!$
# insert previous argument
<ESC> . 

# columnate!!
column -t my_file

# join individual XML files
find rn/*.xml -exec tail +3 '{}' \; > rn.xml
# prepend <?xml version><!DOCTYPE...>
cat rn.xml | pbcopy && head -n 2 rn/rn00072.xml > rn.xml && pbpaste >> rn.xml

# hacer comparaciones numéricas
cut -f 1 ecoli_network | awk '$1<10'| wc -l

# call awk inside awk
awk 'BEGIN{ system("awk '\''BEGIN{ print 1}'\''") }'

# to number lines
nl myfile

# pseudo awk in ruby
echo "paco;luis" | ruby -F";" -nae 'puts $F[1]'

# use bang to call last command from history that starts with those letters. Ex: execute last find command
!f

# copy elements from previous commands
ls paco luis pedro
ls !:1-3

# use all arguments
ls !*

# find and delete
find . -name "temp*" -type f -delete

# find with regex. Finds: temp.a and temp.b
find -E . -regex ".*temp\.(a|b)"

# rename 091/EZCD2CUnit091Pane2280B.jpg into 091/D2C_Unit091_Pane2280_B.jpg
for filename in */*.jpg ; do 
	new_filename=$(echo $filename | sed 's/\(EZC\)\([C2D]*\)\(Unit[[:digit:]]*\)\(Pane[[:digit:]]*\)\(.*\)/\2_\3_\4_\5/g' | sed 's/_A/_Question/' | sed 's/_B/_ZAnswer/')
	mv $filename $new_filename 
done

# fix command and edit in editoraslfjalk
fc

# to download a nature article
ssh nacho@maxwell.cnb.csic.es "wget http://www.nature.com/nature/journal/v475/n7357/pdf/475462a.pdf"
ssh nacho@maxwell.cnb.csic.es "cat 475462a.pdf > 475462a.pdf"

# convert citable notes into citations
grep -E "^#[^#]" *.note | sed 's/\(.*\)\.note:# \(.*\)/\2\(\1\)/g' > "citable_notes.txt"

# random number in range
shuf -i 1-100 -n 1

# get a substring by range
cut -c 201-220 my_big_string

# global replace gnu sed (replaces spaces with underscores)
gsed -i 's/\(Mouse\|Merck\|Human\|Agilent\|Rat\) /\1_/g' gene_info_segex.txt

# rsync local to remote
rsync --verbose  --progress --stats --compress --rsh=/usr/bin/ssh \
      --recursive --times --perms --links --delete \
      /Users/nachocab/Code/CNB/mirna_scroll* nacho@maxwell.cnb.csic.es:/home/nacho/mirna_scroll

# find lines of query_file inside database_file 
# (watch out for partial matches!!! use translate.awk instead)
grep -f query_file database_file

# send a foreground process into background
CTRL+_Z
bg 1
# to check for stopped processes
jobs

# columnize and paginate (cpag)
column -s";" -t top_day3.csv | less -S
cpag top_day3.csv ";"

# open stdout in vim
cpag top_day3.csv ";" | vim -

# sort by column ignoring header in vim
:2,$!sort -k4

# scroll right/left in vim
zH/zL
140 <right arrow>/<left arrow>

# make a symbolic link
ln -s TARGET LINK_FILE

# remove a non-empty directory
rm -rf DIRECTORY

# split in awk: split(input_field, output_array, separator)
split($4,gene_name,"_")
print gene_name[1]

# print an array in awk
for (gene_name in read_counts)
  print gene_name, read_counts[gene_names]

# include a script
lib_path="/data/home/nacho/lib"
source $lib_path/aux.sh

# time and execute in background
time sleep 3 &

# free space
df
df -h

# used space
du -sh 
du -h folder | sort -h

# echo a tab tabs
echo -e "\t"

# cat show tabs
cat -T file_with_tabs

# for loop numbers, don't use quotes
for sample in 1 2 5 6 7 8; do ... done

# check if file or directory exists
if [ -f /tmp/foo.txt ]; then
if [ -d "$DIRECTORY" ]; then

# append to an array
a=(1 3 56)
a=(${a[@]} 66 77)
echo ${a[@]}

# kill a process by job number
kill -9 %1

# variable substitution
${var//Pattern/Replacement}

# apple system log
grep backupd /private/var/log/system.log

# skip header
awk 'NR > 1'

# get size of a folder
du -hs folder_path

# find recursive
find -f . -iname

# copy directory
cp -r lib/ /data/share/pulm/UCLA/Cancer_Progression/edgeR/

# change group (don't confuse with chmod -R +x my_folder/)
chgrp -R pulm /data/share/pulm/UCLA/Cancer_Progression/edgeR/

# match a field against a regular expression in awk
awk '$2 !~ /ENSG/ {print $2, $1}'
awk '$2 ~ /ENSG/ {print $2, $1}'

# convert ensembl to genbank for gsea
cat 4P-IvsN-edgeR.txt | translate ~/Code/Translators/hsa_ensembl_gene_id_to_hsa_genbank_id.csv ";" | awk -F";" -v OFS="\t" '$3 != "" && $2 != "NA"{print $3, $2}' > 4P-IvsN-edgeR-gbk.rnk

# compare two sorted files line by line. Outputs 3 columns by default: unique to file1, unique to file2, intersection file1 and file2
# use --nocheck-order to avoid warnings
# use --output-delimiter=";"
comm file1 file2
comm -23 file1 file2 # lines in file1 but not in file2 (unique to file1)
comm -13 file1 file2 # lines in file2 but not in file1 (unique to file2)
comm -12 file1 file2 # lines in both files (intersection file1 and file2)

# rename

# !$ !^ !*
echo foo bar baz
foo bar baz
$ echo bang-dollar: !$ bang-hat: !^ bang-star: !*
echo bang-dollar: baz bang-hat: foo bang-star: foo bar baz

$ echo foo bar baz
$ echo !:2
echo bar

# :h :t :r :e
echo Head: !$:h  Tail: !$:t
echo Head: /usr/bin Tail: id
Head: /usr/bin Tail: id

# insert last command
esc+.

# print last command before using It
!!:p

# find all places that have perl executables
type -a perl

# send email from terminal
echo "hola

---
Sent from my Unix shell" | mail  -s "prueba" "nacho@bu.edu" -f "nachocab@gmail.com" 

# to add a host in ~/.ssh/config (a folder named .ssh in your home folder). Add an entry for each computer you want to connect to, like this:
Host compy
    HostName 98.256.211.12
    Port 90
    User sidney

ssh compy

# stop writing password to connect
scp ~/.ssh/id_rsa.pub nacho@decima.bu.edu:~/.ssh/
ssh decima # add to hosts
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys
rm ~/.ssh/id_rsa.pub

# copy file to remote. scp gives you progress
scp ~/.bashrc nacho@decima.bu.edu:~
scp ~edalign/MergePlusUnusedVOPE39cl.fasta nacho@decima:/fs/spartans3/nacho
scp some_file nacho@decima:/fs/spartans3/nacho
scp ~/Marine\ Genomics/codecheck/EdParaNfilter1.fastq.gz ~/Marine\ Genomics/codecheck/EdParaNfilter2.fastq.gz  nacho@blueice.bumc.bu.edu:/data/home/nacho/finnerty/

rsync -ave ssh fileToCopy ssh.myhost.net:/some/nonExisting/dirToCopyTO
rsync -ave ssh ~/edalign/MergePlusUnusedVOPE39cl.fasta nacho@decima:/fs/spartans3/nacho/
rsync -ave ssh ~/Marine\ Genomics/codecheck/ nacho@decima:/fs/spartans3/nacho/raw/

# which bash
echo $SHELL

# which linux
lsb_release -a
uname -a

# keep a server job running after I log out (only for bash)
^Z
bg
disown

nohup command-with-options &

# screen
screen -S sessionname
^-a c # create
^-a d # detach
screen -r # reattach

# zip in terminal
gzip file2 # overwrites original
gzip -c file2 > file2.gz # keep original

# unzip over terminal
gzip -d file.gz # overwrite the original: -d for decompress
gzip -d -c file.gz > file # keep the original: -d for decompress
tar zxvf allNvT1vsHsCnidprotcsv.tar.gz 

# growl sticky, use terminal icon, set title, set message
GROWLDATE=`date +"%A, %h %d %Y %H:%m:%S %Z"`
growlnotify -s -a terminal -t "Transfer Complete" -m "$GROWLDATE"

# how many cpus/cores in server
cat /proc/cpuinfo | grep -E "proc|core"
sysctl hw.ncpu # in mac

# core usage
mpstat -P ALL 
mpstat -P ALL 1

# run in multithread mode in linga: inside my_script, use the $NSLOTS var. Ex bowtie -p $NSLOTS ...
qsub -pe single_node 1-8 lib/example.qsub.sh

# to start using an array, you can just define it and assign its elements
samples_in_patient[1]="1 2 5 6 7 8"
samples_in_patient[2]="5 6 7 8"
${samples_in_patient[@]} # to access all the values
${samples_in_patient[*]} # to access all the values

# combine files vertically by colum
paste a b c