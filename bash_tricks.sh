# moving directories: mv what_you_are_moving folder_inside_which_you_move_it
rm -rf prueba1 prueba2 # reset
mkdir prueba1 prueba2
touch prueba1/a prueba1/b prueba1/c prueba2/a prueba2/c

# split the path into its elements
echo $PATH | tr ":" "\n" | sort
# /Users/oliver/.cabal/bin
# /Users/oliver/.rvm/bin

mv prueba1 prueba2 # ls prueba2 => a  c  prueba1/
mv prueba1/* prueba2 # asks if you want to overwrite prueba2/a and prueba2/c

rm -rf prueba1 prueba2 # reset
mkdir prueba1/dir prueba2/dir # -p by alias (default)
touch prueba1/dir/a prueba1/dir/b prueba1/dir/c prueba2/dir/a prueba2/dir/c

mv prueba1 prueba2 # ls prueba2/prueba1/dir => a b c
mv prueba1/* prueba2 # asks if you want to overwrite prueba2/dir, it doesn't allow it because prueba2/dir is not empty.

# pairwise merge skimpdf
skimpdf merge a.pdf b.pdf ab.pdf

# available cores in cluster
qstat -g c

# what hw installed hardware
system_profiler
dmidecode # linux

# xls to txt
xls2csv -q0 -b"--new_sheet--" data_s1.xls > data_s1.txt # -q0 no quotes, watch -g5 number precision

# copy symlink
cp -R symlink
# update brew
brew update

# brew upgrade outdated formulas
brew upgrade

# download concurrently from same server wget (-j is number concurrent downloads)
aria2c -x8 URL
aria2c -s8 URL #also try

# download and unzip (save as script)
#!/bin/bash
TMPFILE=`tempfile`
PWD=`pwd`
wget "$1" -O $TMPFILE
unzip -d $PWD $TMPFILE
rm $TMPFILE

# see all characters in text
grep -oE . text.txt | sort -u | tr -d "\n" #  '()*+,-./0123456789:;>ABCDEFGHIJKLMNOPQRSTUVWXYZ[]_abcdefghijklmnopqrstuvwxyz

# show non-printing special characters
cat -v # show non-printing, invisible
cat -e # show line ends ($) and non printing
cat -t # show tabs (^I) and non-printing
cat -s # squeeze repeat blank lines into one blank line
cat -n # number lines
cat -b # number non-blank lines

# number lines in clipboard
pbpaste | cat -n | pbcopy

# to apply changes in bash profile to current terminal window
source ~/.bash_profile

# to rename extensions
  find /path/to/images -name '*.JPG' -exec bash -c 'mv "$1" "${1/%.JPG/.jpg}"' -- {} \;

# to use all results from find as a list of argumnts (use "+", otherwise use "\;")
  find ../../../Genomes -name "*[^s].fna" -exec ./genoming.pl {} + > output

# remove a char from a file (in this case, to join lines)
tr -d "\n" < my_file  # fastest?
perl -p -e 's/\n//' my_file
sed -e ':a' -e 'N' -e '$!ba' -e 's/\n//g' my_file # slowest
#ex:
    tail -n +2 my_file.fna | tr -d "\n"
# otro TR
echo "PACO" | tr "A-Z" "a-z"

# remove non-printable characters
tr -cd "[:print:]" < file1 # -c means: everything except string 1 (in this case, "[:print:]")

# Remove diacritical marks from all accented variants of the letter `e':
tr "[=e=]" "e"


# show longest repeating sequences in a genome (at least 6 chars long)
tail -n +2 ../../../Genomes/whole/Clostridium\ perfringens.fna | tr -d "\n" | grep -oE (.)\1{5,} | sort -r | head
# even better, It's better to grep a long line than many short lines:
    merge_fasta costridium.fna | grep -oE [ACGT]{3,}

# count number of matches
echo aaaa | grep -o a | wc -l

# show only matching part grep, makes sense with regexps
grep -E "IFN\w+" -o # IFNAR1, IFNB1...

# grep options
grep -h # show only match
grep -H # show match and filename (default)
grep -l # show all matching filenames
grep -L # show all non-matching filenames
grep -m NUM # stop grep after NUM matches

# get the username of every system file
ps aux | cut -d " " -f 1

# sort by column. -t delimiter -n numeric -k field number
cat output | sort -n -k2 -t: | pretty

# if sort is too slow, and you just want to calculate row/column frequencies
awk '{count[$1]++} END{for(c in count) print c, count[c]}' calculations/anjan/2hpi_strata.bow > frequencies.txt

# remove duplicated lines
sort -u
sort | uniq

# show duplicates
sort | uniq -d

# remove last line of a file (in place)
sed -i '$ d' fake_data/b_4hpi_QV.qual

# delete a line in a file containing a string
sed '/delete_this/d' filename.txt

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
find $directory -type f -iname "*.in" # recursive by default
find . -type f -maxdepth 1 -iname "*.txt"

find . -type f -name \*.map -exec ls -lhtr {} + # execute an action after find (ALWAYS USE echo after -exec so you know what's going to execute)

# print the 2nd second nth line of a long file efficiently
awk 'NR==2{print;exit}' SRR049672_F3.csfasta

# grep escape characters
grep "\-1"
grep "\""

# grep reverse file (tac reverses order of lines)
tac file | grep "last_ocurrence"

# find and rename file extension
find /path/ -name "accepted_hits.bam" -exec sh -c 'samtools sort "$1" "${1%.bam}.sorted"' _ {} \; # _ makes sh use absolute paths, $1 is {}

find . -type f -name "*.so" | while read FNAME; do
    mv "$FNAME" "${FNAME%.so}.dylib" # (replace so with dylib)
done

find calculations/09/ -name "accepted*" -exec sh -c 'echo mv "$1" "${1/accepted_hits/accepted_hits.sorted}"' _ {} \;

# find and -execdir so the command is run from the subdirectory containing the matched file
find . -name "accepted_hits.sorted.bam" -execdir sh -c 'samtools view $1 | head' _ {} \; # THEY MUST BE SINGLE QUOTES
find /protected/individuals/nacho/cmf/calculations -name "accepted_hits.sorted.bam" -execdir sh -c "samtools view {} | htseq-count --stranded=no -o counts.sam - /protected/individuals/nacho/cmf/raw_data/genome/gencode.v14.annotation.gtf > counts.txt" \;

# if you want to use $* or $@, you need to pass sh, so that takes up $0 and you don't loose any arguments $1-$n
find path/ -name "counts.txt" -exec sh -c 'head -n1 $@' sh {} +

# find -exec sh -c details
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec echo {} \;
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c 'echo {}' \;
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c "echo {}" \;
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c 'echo $1' sh {} \;
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c "echo $1" sh {} \; # DOESN'T WORK
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c 'echo $1' {} \; # DOESN'T WORK
find /protected/individuals/nacho/cmf/calculations/09 -maxdepth 2 -name "accepted_hits.bam" -exec sh -c "echo $1" {} \; # DOESN'T WORK

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

# reverse order lines file
tail -r myfile.txt

#listar
ls -lhtr
ls -lS
-h # human readable sizes
-t # sort by time modified
-r # reverse sort
-S # sort files by size
-F # show types of files
    # * executable file
    # / directory
    # @ symbolic link
    # = socket
    # % whiteout
    # | FIFO


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

# join lines of two fields on a common field, cartesian, combine, BOTH FILES MUST BE SORTED
join a b # default is first column of each file, separated by spaces. "a" is one column, "b" is wide info
join <(sort a) <(sort b) # if files are unsorted
join -1 3 -2 2 a b -t ":"

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

# linga see how many cores per node in linga
qhost

# clear print line
echo -e '\r\e[K'

# to download a nature article
ssh nacho@maxwell.cnb.csic.es "wget http://www.nature.com/nature/journal/v475/n7357/pdf/475462a.pdf"
ssh nacho@maxwell.cnb.csic.es "cat 475462a.pdf > 475462a.pdf"

# convert citable notes into citations
grep -E "^#[^#]" *.note | sed 's/\(.*\)\.note:# \(.*\)/\2\(\1\)/g' > "citable_notes.txt"

# random number in range
shuf -i 1-100 -n 1

# for loop range sequence

for i in $(seq 1 5)
do
  echo "Welcome $i times"
done

# get a substring by range
cut -c 201-220 my_big_string

# global replace gnu sed (replaces spaces with underscores) # -i edits file in-place
gsed 's/\(Mouse\|Merck\|Human\|Agilent\|Rat\) /\1_/g' gene_info_segex.txt
gsed -i 's/\(Mouse\|Merck\|Human\|Agilent\|Rat\) /\1_/g' gene_info_segex.txt

# use sed -E -e 's///g' whenever possible to avoid \( ... \)

# replace tabs using sed (use -i to replace file)
gsed 's/\t/@/g' contig_gene_names2.txt
# rsync local to remote
rsync --verbose  --progress --stats --compress --rsh=/usr/bin/ssh \
      --recursive --times --perms --links --delete \
      /Users/nachocab/Code/CNB/mirna_scroll* nacho@maxwell.cnb.csic.es:/home/nacho/mirna_scroll

# diff two files by line content
a_not_in_b file_a file_b
a_not_in_b file_a file_b -r
# DON'T USE THIS FOR GENE NAMES!! (watch out for partial matches!!! use translate.awk instead)
grep -f small_query_file_with_one_column long_database_file_with_match_column_and_others > in_query_and_in_database.txt
grep -vf small_query_file_with_one_column long_database_file_with_match_column_and_others > in_query_and_not_in_database.txt

# send a foreground process into background (you get a notification when it's done)
CTRL+Z
bg 1

# to check how background processes are doing
jobs

# columnize and paginate (cpag)
column -s";" -t top_day3.csv | less -S
cpag top_day3.csv ";"

# open stdout in vim
cpag top_day3.csv ";" | vim -

# sort by column ignoring header in vim
:2,$!sort -k4

# sort specify column in linux (you probably need the second comma)
sort my_file -k1,1

# scroll right/left in vim
zH/zL
140 <right arrow>/<left arrow>

# make a symbolic link (can't use ../.. paths)
ln -s ORIGINAL_TARGET SYMBOLIC_LINK_NAME

# remove a non-empty directory
rm -rf DIRECTORY

# awk split - split in awk: split(input_field, output_array, separator)
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

# free space (quota in linga)
df
df -h
df -h --direct /protected/individuals/* # show the file, not the mount point

# used space
du -sh # summarize all subfolders
du -h | sort -h # show all and sort by human-size
du -ah | sort -h # show all and sort by human-size


# echo a tab tabs, or new lines
echo -e "\t"
echo    "paco\nluis" | wc -l # 1
echo -e "paco\nluis" | wc -l # 2

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

# shell parameter expansion - variable substitution
${variable:-word} # If variable is unset or null, the expansion of word is substituted. Otherwise, the value of parameter is substituted.
${variable:=word} # If variable is unset or null, the expansion of word is assigned to parameter. The value of parameter is then substituted. Positional parameters and special parameters may not be assigned to in this way.
${variable:?word} # If variable is null or unset, the expansion of word is written to the standard error and the shell, if it is not interactive, exits. Otherwise, the value of parameter is substituted.
${variable:+word} # If variable is null or unset, nothing is substituted, otherwise the expansion of word is substituted.

${variable:offset}
${variable:offset:length} # Expands to up to length characters of parameter starting at the character specified by offset. If length is omitted, expands to the substring of parameter starting at the character specified by offset. length and offset are arithmetic expressions (see Shell Arithmetic). This is referred to as Substring Expansion.

${!prefix*}
${!prefix@} # Expands to the names of variables whose names begin with prefix, separated by the first character of the IFS special variable. When ‘@’ is used and the expansion appears within double quotes, each variable name expands to a separate word.

${!name[@]}
${!name[*]} # If name is an array variable, expands to the list of array indices (keys) assigned in name. If name is not an array, expands to 0 if name is set and null otherwise. When ‘@’ is used and the expansion appears within double quotes, each key expands to a separate word.
${#parameter} # The length in characters of the expanded value of parameter is substituted. If variable is ‘*’ or ‘@’, the value substituted is the number of positional parameters. If variable is an array name subscripted by ‘*’ or ‘@’, the value substituted is the number of elements in the array.


# substring removal
a="aacdff"
${variable#word} # remove from beginning, shortest
echo ${a#*a} # acdff
${variable##word} # remove from beginning, longest
echo ${a##*a} # cdff
${variable%word} # remove from end, shortest
echo ${a%f*} # aacdf
${variable%%word}  # remove from end, longest
echo ${a%%f*} # aacd

${variable/pattern/string} # Parameter is expanded and the longest match of pattern against its value is replaced with string. If pattern begins with ‘/’, all matches of pattern are replaced with string. Normally only the first match is replaced. If pattern begins with ‘#’, it must match at the beginning of the expanded value of parameter. If pattern begins with ‘%’, it must match at the end of the expanded value of parameter. If string is null, matches of pattern are deleted and the / following pattern may be omitted. If variable is ‘@’ or ‘*’, the substitution operation is applied to each positional parameter in turn, and the expansion is the resultant list. If variable is an array variable subscripted with ‘@’ or ‘*’, the substitution operation is applied to each member of the array in turn, and the expansion is the resultant list.

# case modification
${PARAMETER^}
${PARAMETER^^}
${PARAMETER,}
${PARAMETER,,}
${PARAMETER~}
${PARAMETER~~} # The ^ operator modifies the first character to uppercase, the , operator to lowercase. When using the double-form (^^ and ,,), all characters are converted.

# case modification in arrays
array=(This is some Text)
echo "${array[@],}" # this is some text
echo "${array[@],,}" # this is some text
echo "${array[@]^}" # This Is Some Text
echo "${array[@]^^}" # THIS IS SOME TEXT
echo "${array[2]^^}" # TEXT

# find -printf show filename
find path/ -printf "%p\n" # show full path
find path/ -printf "%P\n" # show relative path

# apple system log
grep backupd /private/var/log/system.log

# skip header
awk 'NR > 1'

# get size of a folder
du -hs folder_path

# copy directory
cp -r lib/ /data/share/pulm/UCLA/Cancer_Progression/edgeR/

# change group (don't confuse with chmod -R +x my_folder/)
chgrp -R pulm /data/share/pulm/UCLA/Cancer_Progression/edgeR/

# match a field against a regular expression in awk grep, awk regex
awk '$2 !~ /ENSG/ {print $2, $1}'
awk '$2 ~ /ENSG/ {print $2, $1}'

# convert ensembl to genbank for gsea
cat 4P-IvsN-edgeR.txt | translate ~/Code/Translators/hsa_ensembl_gene_id_to_hsa_genbank_id.csv ";" | awk -F";" -v OFS="\t" '$3 != "" && $2 != "NA"{print $3, $2}' > 4P-IvsN-edgeR-gbk.rnk

# count empty lines in a file
grep -c ^$ my_file

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

# ls colors linux
ls --color=always

# show hostname on terminal with color and git status
export PS1=$IBlack$Time12h$Color_Off'$(git branch &>/dev/null;\
if [ $? -eq 0 ]; then \
  echo "$(echo `git status` | grep "nothing to commit" > /dev/null 2>&1; \
  if [ "$?" -eq "0" ]; then \
    # @4 - Clean repository - nothing to commit
    echo "'$Green'"$(__git_ps1 " (%s)"); \
  else \
    # @5 - Changes to working tree
    echo "'$IRed'"$(__git_ps1 " {%s}"); \
fi) '$BRed'\h '$PathShort$Color_Off' \$ "; \
else \
  # @2 - Prompt when not in GIT repo
  echo " '$Red'\h '$PathShort$Color_Off' \$ "; \
fi)'

# stop writing password to connect
scp ~/.ssh/id_rsa.pub nacho@rubysky.bumc.bu.edu:~/.ssh/
ssh nacho@rubysky.bumc.bu.edu
cat ~/.ssh/id_rsa.pub >> ~/.ssh/authorized_keys # do this in the server
rm ~/.ssh/id_rsa.pub # erase your local public key, you can create a new one by default, just hit yes and no pwd

# see known servers hosts
cat ~/.ssh/known_hosts

# copy file to remote. scp gives you progress
scp ~/.bashrc nacho@decima.bu.edu:~
scp ~edalign/MergePlusUnusedVOPE39cl.fasta nacho@decima:/fs/spartans3/nacho
scp some_file nacho@decima:/fs/spartans3/nacho
scp ~/Marine\ Genomics/codecheck/EdParaNfilter1.fastq.gz ~/Marine\ Genomics/codecheck/EdParaNfilter2.fastq.gz  nacho@blueice.bumc.bu.edu:/home/nacho/storage/cmf/raw_data

rsync -ave ssh fileToCopy ssh.myhost.net:/some/nonExisting/dirToCopyTO
rsync -ave ssh ~/edalign/MergePlusUnusedVOPE39cl.fasta nacho@decima:/fs/spartans3/nacho/
rsync -ave ssh ~/Marine\ Genomics/codecheck/ nacho@decima:/fs/spartans3/nacho/raw/
rsync -ave ssh *  nacho@decima:/fs/userB2/nacho/storage/derek/raw/

# upload rubysky executable
chmod +x tophat
scp tophat nacho@rubysky.bumc.bu.edu:/protected/individuals/nacho/cmf/src/ # or use cmd+uf in sftp
BOWTIE_INDEXES="/protected/individuals/nacho/cmf/raw_data/genome/bowtie1/";LANE="b_4"; OUT_DIR="/unprotected/projects/lasvchal/moss/fake_calculations/${LANE}_tophat"; /protected/individuals/nacho/cmf/src/tophat --library-type fr-secondstrand --segment-length 25 --no-coverage-search --no-novel-juncs -G "/protected/individuals/nacho/cmf/raw_data/genome/gencode.v14.annotation.gtf" -o "$OUT_DIR" --color --bowtie1 --quals hg19 "/unprotected/projects/lasvchal/moss/fake_data/${LANE}hpi.csfasta" "/unprotected/projects/lasvchal/moss/fake_data/${LANE}hpi_QV.qual"


# which shell bash
ps -p $$

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
run_script
^-a d # detach
screen -r sessionname # reattach

^-a c # create additional

# unzip .zip file
unzip file # keep original
unzip -d file

# gunzip COMPRESS into zip in terminal (3 min for a 1GB text file, down to 400MB)
gzip file2 # overwrites original
gzip -c file2 > file2.gz # keep original
find . -type f -name "*.fastq" -exec sh -c "gzip < {} > {}.gz" \; & # convert multiple individually in the background
ls | parallel -j+0 --eta gzip # try parallel

# unzip DECOMPRESS over terminal
gzip -d file.gz # overwrite the original: -d for decompress
gzip -d -c file.gz > file # keep the original: -d for decompress
tar zxvf allNvT1vsHsCnidprotcsv.tar.gz # --overwrite by default, use -k to keep old files, -x extract files from archive

tar tvfz file.tar.gz # -t view the file contents without extracting
tar xvfz archive_file.tar.gz /path/to/file # extract a single file
tar xvfz archive_file.tar.gz /path/to/dir/ # extract a single dir
tar xvfz archive_file.tar.gz /path/to/dir1/ /path/to/dir2/ # extract multiple dirs
tar xvf archive_file.tar --wildcards '*.pl' # use regex to specify which files should be extracted

tar tvfz file.tar.gz -C output_dir # specify a different output directory

# determine size of gz file (doesn't work over 2GB)
gzip -l compressedfile.gz

# crappy way to make number human readable
echo 23375001203 | perl -lne 'printf("%.2f MB\n", $_/1024/1024)'
echo 23375001203 | perl -lne 'printf("%.2f GB\n", $_/1024/1024/1024)'

# zip in a tarball a number of files
tar -cvzf mystuff.tar.gz foo.tex fig1.eps fig2.eps # -c create a new archive, -v verbosely list processed files, -f FILE_NAME -z compress the file (through gzip)

# growl sticky, use terminal icon, set title, set message
GROWLDATE=`date +"%A, %h %d %Y %H:%m:%S %Z"`
growlnotify -s -a terminal -t "Transfer Complete" -m "$GROWLDATE"

# remove whitespace in awk
gsub(/^[ \t]+|[ \t]+$/,"")

# decima kaya1 folders
/fs/userB2/nacho # home
/fs/spartans3/nacho # storage

# Linga folders (blueice redstar)
/home/nacho # my home
/protected/individuals/nacho # data

# new login on blueice linga
newgrp lasvchal
qlogin -P lasvchal

# job management
qhost -j # see which jobs from other users are running on each node
qstat # which jobs are running
qstat -j PID # info about specific job, including scheduling info
qsub job.qsub # submit job
qdel PID # kill job
qdel -u nacho # kill all my jobs

# find out what groups a user belongs to
id -G -n nacho
projects -lp


# run in multithread mode in linga: inside my_script, use the $NSLOTS var. Ex bowtie -p $NSLOTS ...
newgrp lasvchal
qsub -P lasvchal -pe single_node 1-8 src/assemble.qsub
Your job 3170852 ("assemble.sh") has been submitted

# how many cpus/cores in server
cat /proc/cpuinfo | grep -E "proc|core"
sysctl hw.ncpu # in mac

# core usage
mpstat -P ALL
mpstat -P ALL 1

# associative arrays
declare -A array
array[hello]=world
array[goodbye]=lucas
${array[hello]} # to access

# to start using an array, you can just define it and assign its elements, or use declare -a
samples_in_patient[1]="1 2 5 6 7 8"
samples_in_patient[1]=(1 2 5 6 7 8) # same thing
samples_in_patient[2]="5 6 7 8"
${samples_in_patient[@]} # to access all the values
${samples_in_patient[*]} # to access all the values

declare -a life_stages=(parasite_1 parasite_2 parasite2planula_1 planula_1 planula2adult_1 adult_1)

# Aspera Connect
~/Applications/Aspera\ Connect.app/Contents/Resources/ascp
http://download.asperasoft.com/download/docs/connect/3.0.1/osx/en/html/index.html

# combine files vertically by colum
paste a b c

# convert a column into a row, multiline to single lien
cut -f1 paco.csv | xargs # echo by default
echo 1 2 3 4 | xargs -t -n2 mv {} # {} allows to explicitly call each element?
echo 1 2 3 4 | xargs -n2
# 1 2
# 3 4
xargs -t # shows the command
xargs -p # asks for confirmation



# curl
curl -o index.html http://projects.flowingdata.com/life-expectancy/ # explicitly name
curl -O http://projects.flowingdata.com/life-expectancy/index.html # name as remote

# wget
wget -O - http://www.example.com # send webpage to stdin
wget -P my_folder http://www.example.com # change folder, not filename
wget -O my_folder/my_file_name http://www.example.com # change both, it's like a shell redirect

# brace expansion
wget http://www.ploscompbiol.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pcbi.1000545.s00{2..6} # download supplementary material
# curl has it integrated
curl -O "http://www.ploscompbiol.org/article/fetchSingleRepresentation.action?uri=info:doi/10.1371/journal.pcbi.1000545.s00[2-6]"

# download all the URLs from a file
wget -i file_with_urls

# download entire folder wget
wget -P local_folder -m url_folder # -r -N -l inf --no-remove-listing (m is for mirror)
wget -r -np url_folder # (investigate a way TO AVOID REPLICATING FOLDER STRUCTURE) download all files recursively under the specified path (np: --no-parent), default maximum recursion is 5

# download and extract tarball
wget -qO - "http://www.tarball.com/tarball.gz" | tar zxvf -


# get all pdf and zips from a website
wget --reject html,htm --accept pdf,zip -rl1 url

# untar a tar.bz2 file to a specific folder
tar -xjf test.tbz -C /tmp/test

# return true if the variable is unset undefined
if [[ -z "$VAR" ]];

# declare function
function_name(){
  local local_var=$1
  echo "$local_var" # return first argument
}

# execute function
function_name "paco" # "paco"
my_var=$(function_name "paco"); echo $my_var # "paco"

# shift arguments, while saving the first one
input_file=$1; shift

# with arguments one two three
echo "$@" # 3
echo "$*" # 1
echo $@ # 3
echo $* # 3

# concat strings
foo="Hello"
foo=$foo" World"

# CHECK if first file in awk, two file pattern, you need to check for the filename, otherwise FNR == second file if the first file is empty
NR == FNR && FILENAME == ARGV[1] {
    query[#{fields}]++; next
}

# translator pattern
BEGIN{
    while (getline < translator) {
        translations[$1] = $2
    }
    close(translator)
}

{
    $(NF + 1) = translations[$field_to_translate]
    print
}

# output and count lines (use tee to save intermediate files)
cat ns | tee /dev/tty | wc -l
cat ns | tee intermediate_file | wc -l
cat ns | tee /dev/tty intermediate_file | wc -l

# generate random numbers and reshape output array
jot -r 100 | rs 10 10 # matrix 10x10

# diff
diff -y --suppress-common-lines filea fileb

# grep gene info
grep -i "hla-" ~/gene_info/Homo_sapiens.gene_info | cut -f2,3,9

# awk for loop
for (i=1; i<=10; i++) {
  sum += i; # sum = sum + i
}

# awk concat string
awk 'BEGIN{a=""; a= a "b" 3 "c"; print a}' # don't use a+= "b"

# mdfind
mdfind -onlyin dir QUERY
mdfind -name matchingFileName QUERY
mdfind -literal QUERY

# translate
translate(){ wget -qO- "http://ajax.googleapis.com/ajax/services/language/translate?v=1.0&q=$1&langpair=$2|${3:-en}" | sed 's/.*"translatedText":"\([^"]*\)".*}/\1\n/'; }

# vi vim shortcuts
## move
w - go to beginning of next word
e - go to end of next word
b - go back to the beginning of previous word
. - jump back to last edited line.
g; - jump back to last edited position.
f - find forward
F - find backward
; - repeat last f, F, t or T
, - reverse ;
## insert
I - insert to the start of current line
A - append at the end of line
o - open a line below
O - open a line above
C - change rest of current line (like ^K)
cc - change the whole line (like a reset)
## scroll
:43<enter> - go to line 43
^u - scrolls half a screen up
^b - scrolls half a screen bottom

u - undo
^r - redo

# time formats
%d  Day of the month as a decimal number [01,31].
%m  Month as a decimal number [01,12].

%y  Year without century as a decimal number [00,99].
%Y  Year with century as a decimal number.

%A  Locale’s full weekday name.
%a  Locale’s abbreviated weekday name.

%H  Hour (24-hour clock) as a decimal number [00,23].
%M  Minute as a decimal number [00,59].

%b  Locale’s abbreviated month name.
%B  Locale’s full month name.

%c  Locale’s appropriate date and time representation.

%f  Microsecond as a decimal number [0,999999], zero-padded on the left (1)
%I  Hour (12-hour clock) as a decimal number [01,12].
%j  Day of the year as a decimal number [001,366].
%p  Locale’s equivalent of either AM or PM. (2)
%S  Second as a decimal number [00,61]. (3)
%U  Week number of the year (Sunday as the first day of the week) as a decimal number [00,53]. All days in a new year preceding the first Sunday are considered to be in week 0.  (4)
%w  Weekday as a decimal number [0(Sunday),6].
%W  Week number of the year (Monday as the first day of the week) as a decimal number [00,53]. All days in a new year preceding the first Monday are considered to be in week 0.  (4)
%x  Locale’s appropriate date representation.
%X  Locale’s appropriate time representation.
%z  UTC offset in the form +HHMM or -HHMM (empty string if the the object is naive).  (5)
%Z  Time zone name (empty string if the object is naive).
%%  A literal '%' character.
