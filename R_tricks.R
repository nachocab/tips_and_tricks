# select or slice data frame by column names or row names 
df[, c('column_name_1','column_name_2','column_name_3')]
subset(df,select=-c(column_name_1,column_name_3)) # negative selection by name
subset(df,select=column_name_1:column_name_3) # select by ranges
subset(df,select=-column_name_0) 
subset(df,select=c(column_name_1,column_name_3))
subset(df,select=c(column_name_1,3)) # you can combine names and numbers
subset(df, select=grep("col", colnames(df))) # subset by grepping
gene_name_mapper$GeneName[which(gene_name_mapper$ProbeName %in% d$ProbeName)] # select using %in%

# for each element in a, it checks if it's also in b
a %in% b

# substitute
names <- sub("Perez","Pérez",names)

# read from clipboard
a <- read.table("clipboard")

# sort dataframe by column name
df[with(df, order(-z, b)), ]

# sort dataframe by row names or column names
dat[order(rownames(dat)),order(colnames(dat))]

# sort dataframe by column number
df[order(-df[[3]]),]
df[order(df[[3]],decreasing=desc),,drop=F]

# (sort) get the top result of matrix by column 5 decreasing
probabilities[probabilities[,5] == max(probabilities[,5]),]
probabilities[order(-probabilities[,5]),][1,]

# wide to long. id: non-changing columns, variable_name: name of indexing column (otherwise it uses "variable")
mdf <- melt(df, id=c("probe_id", "accession_number", "gene_name", "gene_description", "p.value"), variable_name="Experiment")

# melt with split by variable name, create a new dataframe
# names(res): "X" "Alpha" "Lambda" "Elastic.Net.Mean" "Elastic.Net.Std"  "LM.Mean"       "LM.Std"
res_m <- res[,c(1,4:7)]
res_m <- melt(res_m, id=c("Gene"))
res_m$variable <- sub("Elastic.Net","Elastic_Net",res_m$variable)
res_m <- with(res_m, data.frame(
        Gene = Gene,
        model = unlist(strsplit(variable,"\\."))[1],
        statistic = unlist(strsplit(variable,"\\."))[2], 
        value = value))

# even better
res_m <- data.frame(
        Gene = rep(res$X, times=2),
        model = c(rep("Elastic Net", times=nrow(res)), rep("Linear Model", times=nrow(res))),
        mean = c(res$Elastic.Net.Mean, res$LM.Mean),
        sd = c(res$Elastic.Net.Std, res$LM.Std)
)

# use this page to update R or you'll get problems with libiconv
http://r.research.att.com/

# read arguments
args <- commandArgs(T)
input_name <- args[1]
R --slave --vanilla < script.R
R CMD BATCH infile.R outfile

# Plot a bar chart when one of your variables gives the height (use stat identity) bar plot
ggplot(iron, aes(cluster,size)) + geom_bar(stat="identity") 
ggplot(rna, aes(life_stage,reads, fill=type)) + geom_bar(stat="identity") # stacked bars

# bar chart with error bars
stdev_coords <- aes(ymax = mean_error + sd_error, ymin=mean_error - sd_error)
p <- ggplot(res_m, aes(x=gene, y=mean_error, fill=model)) + 
    geom_bar(position="dodge", stat="identity") + 
    geom_errorbar(stdev_coords, width=0.1, position="dodge")

# add contour around geom_bar
geom_bar(color="black")

# add manual palette ggplot
+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

# Save a graph
ggplot(reads_by_life_stage, aes(life_stage,reads, fill=type)) + geom_bar(stat="identity")
cat("Saving..."); filename <- paste0("plots/reads_by_life_stage.png");ggsave(filename, width=8, height=6)
cat(paste("\rSaved:", filename, "\n"))

filename <- paste("cdon.png", sep="");png(filename,width=1024,height=500,units="px",bg = "transparent")
print(p)
dev.off(); print(paste("Saved:", filename))

# Add points with different aes to an existing graph
ggplot(clusters,aes(V1,V2)) + 
    geom_point(data=clusters, aes(V1,V2,col=V4,shape=V3+15,size=7)) + 
    geom_point(data=centroids, aes(V1,V2, col=V3),size=10, inherit.aes=F)

# Remove legend ggplot
opts(legend.position="none")

# Increase ggplot outer margins (x-right:1, x-left:1, y-top:0.5, y-bottom:0.5)
opts(plot.margin = unit(c(1,1,25,25), "lines"))

# rotate x axis 90 degrees vertical (ggplot)
opts(axis.text.x = theme_text(angle=90, hjust=0))
opts(axis.text.x = theme_text(angle=45, hjust=1, vjust=1))

# Group and facet
ggplot(sub_m_cu,aes(codon,rf, group=gene, color=gene)) + geom_line() + facet_wrap(~aa, scale="free_x")
ggplot(global_rf, aes(codon,rf)) + geom_line(aes(group=aa, color=aa, size=2)) + facet_grid(. ~ aa, scale="free_x", space="free") + p_global_opts + ylim(0,1)

# Limit axis
ggplot(maa_current,aes(codon,value, group=gene, color=cluster)) + ylim(0,1)

# transparent jitter and summary line
ggplot(myaa_cc, aes(reorder(codon,rf,sum),rf, group=gene)) + geom_line(alpha=1/15, position=jit) + stat_summary()

# annotate individual facets
annotation <- data.frame(experiment=c("1-Control Proestrus"), Sample=41, Average=3, label=c("34.4"))
p <- p + facet_grid(. ~ experiment, scales = "free") 
p + geom_text(aes(label=label), data=annotation)

# split string
unlist(strsplit("a.b.c",".", fixed=T)) # literally
unlist(strsplit("a.b.c","\\.", fixed=T)) # regexp

# average mean by groups
aggregate(value ~ group, dat, mean) # dat$value, dat$group

# write data frame to file
write.table(ratList, file = "ratList.csv", sep = ";", quote=F, row.names=F, col.names=F)

# super hack to show filled squares instead of lines in legend
GeomLine2 <- proto(GeomLine, {
    objname <- "line2"
    guide_geom <- function(.) "polygon"
    default_aes <- function(.) aes(colour = "black", size=0.5, linetype=1, alpha = 1, fill = "grey20")
     })
geom_line2 <- GeomLine2$build_accessor()

g_qpcr <- ggplot(m_qpcr, aes(experiment, value, group=probe_id, color=gene_name, fill=gene_name)) 
g_qpcr <- g_qpcr + geom_line2(aes(size=log(1/p_value)))

# don't convert strings to factor
data.frame(... stringsAsFactors=F)

# reorder the levels of a factor
f <- factor(c("a","b","c"), levels=c("c","a","b"))
df$factor <- factor(df$factor, levels=c("c","a","b"))
unordered_factor <- relevel(unordered_factor, "Ref")

# make a factor ordered
f <- factor(c("a","b","c"), levels=c("c","a","b"), ordered=T)
data$f <- ordered(data$f, levels=c("c","a","b"))



# rename axis title
scale_y_continuous(name="Fold Change")

# increase font size and maintain vertical
opts(axis.title.y=theme_text(size=20, angle=90, face="bold"))

# use decent sizes
g_qpcr <- g_qpcr + opts(plot.title = theme_text(size = 25), axis.text.x = theme_text(angle=-90, hjust=0, size=20), axis.title.y=theme_text(size=20, angle=90), axis.title.x=theme_text(size=20), axis.text.y=theme_text(size=20))

# To build a dataframe in R by reading from a shell pipe 

data <- read.table(pipe("cat /dev/stdin"))
> cat my_data | Rscript reader.R

# To use a shell command and "interpolate" an R variable
system(paste("echo",myLongString,"|pbcopy"),intern=T)

pattern_variable <- "big_file_*"
paths <- system(paste("ls",pattern_variable), intern=T) 
paths
> big_file_1       big_file_2       big_file_3

# insert a string inside another string: (use sub?)
path <- "dir/my_file"
randomized_path <- system(paste("echo ", path, " | sed 's/\\(.*my_\\)/\\1randomized_/'", sep=""), intern=T) 
randomized_path 
dir/my_randomized_file

# open in sublime
system("subl qpcr_pairwise_proestrus.csv")

# find out size of current device window
dev.size(units="px")

# new device
dev.new(width=5, height=4)

# you can't change axis position (top, right)

# reduce number of significant positions
signif(df,4)
format(df, scientific=F)
format(df, digits=3)

# install a bioconductor package
source("http://www.bioconductor.org/biocLite.R")
biocLite("DESeq")
biocLite("org.Hs.eg.db")

# get current version of package
sessionInfo()

# par good practice
old_par <- par()
    par(mfrow=c(1,2))
    plot(a)
    plot(b)
par(old_par)

# conditional color
plot(..., col = ifelse( fit$padj < p.value.cutoff, "red", "black" ))

## Frequency tables ##
# 1. (case=>table) If you start with a dataframe in case form, you can convert to table form by using table():
repeated.rows <- data.frame(smoker=c("Y","Y","Y","N","N"), diagnosis=c("cancer","cancer","nothing","nothing","cancer"))
# smoker diagnosis
# Y      cancer
# Y      cancer
# Y      nothing
# N      nothing
# N      cancer
with(repeated.rows, table(smoker,diagnosis))
#       diagnosis
# smoker cancer nothing
#      N      1       1
#      Y      2       1

# 2. (frequency=>table)If you start with a dataframe in frequency form, you can convert to table form by using xtabs():
melanoma
#   type site count
# 1    1    h    22
# 2    1    t     2
# 3    1    e    10
# 4    2    h    16
# 5    2    t    54
# 6    2    e   115
T_melanoma <- xtabs(count~type+site, melanoma)
#     site
# type   e   h   t
#    1  10  22   2
#    2 115  16  54
#    3  73  19  33
#    4  28  11  17

# 3. (frequency=>case) If you start with a dataframe in frequency form, you can convert it to case form. Just rename count with Freq and use expand.dft().
names(melanoma)[3]<- "Freq"
library(vcdExtra)
expand.dft(melanoma) # auxiliary function
#    type site
# 1     1    h
# 2     1    h
# 3     1    h
# 4     1    h
# ...

# 4. (case=>frequency)
xtabs(~A+B)

# 5. (table=>case)
library(vcdExtra)
expand.dft(X)

# 6. (table=>frequency)
as.data.frame(X)

## model formulas ##
# simple linear regression model of y on x
y ~ 1 + x   # explicit intercept (+ means inclusion, optional for the first term)
y ~ x       # implicit intercept 

# simple linear regression of y on x through the origin (without an intercept)
y ~ 0 + x   
y ~ -1 + x   # (- means exclusion)
y ~ x - 1

# saturated model
# using SAS proc genmod: y=x1 x2 x1*x2
y ~ x1 * x2 # x1 + x2 + x1:x2
y ~ x1:x2

# independence model
y ~ x1 + x2

# what is the p-value of a given chi-square statistic with corresponding degrees of freedom
# pchisq: distribution function (pdf, even though d is for density) for the chi-squared distribution with 'df' degrees of freedom.
# A low p-value means we reject the hypothesis of independence, it means there is a significant association.
# A low p-value indicates a bad fit of the model.
1-pchisq(chi.stat,df)


# use expand.grid to get a cartesian product of factors. order of factors goes from fastest changing to slowest (put response variable first)
GSS <- data.frame(expand.grid(sex=c("female", "male"),party=c("dem", "indep", "rep")),
           count=c(279,165,73,47,225,191))
# customized way of doing it
politics <- data.frame(
                       gender=rep(rep(c("male", "female"),each=2),3), 
                       race=rep(c("white", "black"),6), 
                       party=rep(c("Democrat","Republican","Independent"),each=4),
                       count=c(132,42,172,56, 176,6,129,4, 127,12,130,15))

# get the names of a table. dimnames is a list
dimnames(HairEyeColor)

# to create a table
JobSat <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4)
dimnames(JobSat) = list(income=c("< 15k", "15-25k", "25-40k", "> 40k"),
                 satisfaction=c("VeryD", "LittleD", "ModerateS", "VeryS"))
JobSat <- as.table(JobSat)   

# Use formulas to display tables
library(vcd)
structable(Hair+Sex ~ Eye, HairEyeColor)  

# sum across one variable of an n-way table
margin.table(HairEyeColor,1)

# test for association between two variables using log-likelihood ratio g2 test, chi square test, etc.
library(vcd)
assocstats(GSStab)

# loglm and glm require dataframes in frequency form

# remove delete all objects in environment
rm(list=ls())

# breaks in a histogram
hist(df$p.values, breaks=300)

# add gene names ensembl
ensg_to_gene_name_table <- read.table("ensembl_gene_id_to_gene_name.csv",sep=";", header=T)
ensg_to_gene_name <- as.vector(ensg_to_gene_name_table[,1])
names(ensg_to_gene_name) <- ensg_to_gene_name_table[,2]
save(ensg_to_gene_name, file="ensg_to_gene_name.RData")
add_gene_names <- function(ensg_ids) {
  ifelse(!is.na(ensg_to_gene_name[ensg_ids]), ensg_to_gene_name[ensg_ids],ensg_ids)
}

# specify which column holds the row names in a dataframe
read.table(..., row.names="GeneName")

# rotate text angle
text(...,srt=-45)

# text overflow plot frame
par(xpd=NA)
plot(...)

# create text file quickly
cat("wt1 mu1
mu1 wt1
wt2 mu2
wt2 mu2
", file="prueba2.txt")

# for loop
for(gene in gene_names) {
    
}

# install package from source
install.packages("RJSONIO", repos = "http://www.omegahat.org/R", type="source")
install.packages("/Users/nachocab/Code/Rprojects/Design_2.3-0.tar.gz", repos = NULL, type="source")

# return an object without missing values
na.omit(df)

# For every element in a, return either the position where it occurs in b or NA.
query <- c("g","a","d","B","A","f")
ref <- letters
match(query,ref) #  7  1  4 NA NA  6


# create a vector with names
a <- c("a"=3,"b"=4)

# multiply two arrays to make a matrix
outer(c(1,2,3),c(4,5))
#      [,1] [,2]
# [1,]    4    5
# [2,]    8   10
# [3,]   12   15

# to see the callback stack after an error
traceback()

# measure times
started <- proc.time()

print(proc.time() - started)

# substring
get_the_b <- substr("aaabaa", 4, 4)

# grep list regex
grep("P4",sample_names, invert=T)

# find index of value in an array (don't confuse which with match)
which(c(5,6,7)==6) #2
a$prop1[which(a$prop2 %in% b)] # without which you get TRUE FALSE TRUE ...

# use which.min instead of match(min(arr))
which.min(arr)

# make text bigger
plot(..., cex=4) # text size = cex*ps

# make title bigger
plot(..., cex.main=5, mar=c(5, 4, 4, 2) + c(.1,.1,7,.1))

# change font size, you can see al the fonts by doing X11Fonts()
plot(..., font=2)   #(1=plain, 2=bold, 3=italic, 4=bold-italic)

# config file /Library/Frameworks/R.framework/Resources/fontconfig/conf.avail/60-latin.conf
plot(..., family="Helvetica")
text(1:4, 1:4, paste("Font face", 1:4, "for this text"),family="Palatino")
filename <- paste("fonts.png", sep="")
png(filename, width=800, height=800); plot(c(1,100),c(1,100));text(seq(1,100,10), seq(1,100,10), paste("Font face", 1:4, "for this text"), family="Helvetica", font=2, cex=2);dev.off(); 

# pause between plots
par(ask=T)

# add a text label by clicking 
text(locator(1),"84") 

# black background or no background
par(bg="black", col="white", col.main="white")
par(bg=NA, col="white", col.main="white")

# no box around plot (box type)
par(bty="n")

# line type: c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
plot(..., lty="solid")

# remove the 4% extension of plot origin, it's "r" by default
par(xaxs = "i", yaxs = "i")

# remove axes
plot(..., axes=F)

# plot line width
plot(..., lwd=3)

# plot type: "l", "p","b", "n"
plot(..., type="l")

# increase external margins: 0 by default
par(oma=rep(2,4))

# increase internal margins (B, L, T, R)
par(mar=c(5, 4, 4, 2) + c(.1,.1,2,.1))

# plot fill
y <- c(7,5,6)
x <- 0:(length(y)-1) # or 1:length(y)
plot(x,y, type="n", xlim=c(0,max(x)), ylim=c(0,max(y)))
polygon(c(min(x),x,max(x)), c(0, y,0), col="blue", border=NA)

# decent graphics with palette
old_par <- par()
par(bg=palette[1], cex.main=3, mar=c(5, 4, 4, 2) + c(.1,.1,3,.1), xpd=NA)
plot(..., cex=4, axes=F, bty="n", font=2, xlab="", ylab="", font=2)
par(old_par)

# ggplot log scale
+ scale_y_log10() + coord_trans(y = "pow10")

# assign a string into a variable
assign("a",3) # > a == 3
x <- 42
eval(parse(text = "3 + 4")) # you can even call functions!
# 7
eval(parse(text = "x")) 
[1] 42
x <- 42
deparse(substitute(x))
[1] "x"

# no warnings when executing a command 
suppressWarnings(expr)

# lazy loading of default arguments
my_args <- list(a=1,b=10, c=5)
p <- function(args, a=args$a,b=args$b,c=args$c){
  cat(paste(a,b,c,"\n"))
}
p(my_args)
# 1 10 5
# Create a directory, or no warning if it exists 
dir.create(path, showWarnings=F))

# check if file exists
file.exists(filename)

# get n random elements from a vector without replacement
sample(my_array,n)

# add a legend to a plot (x-coord, y-coord, text, line type, line width, colors)
legend(10,40, rownames(rates), lty=1, lwd=5, col=c("steelblue", "red"))

# rename axis
plot(..., xaxt="n")
axis(1, at=1:8, labels=my_labels)

# read a file into a vector or list
scan(path, "character") # or "integer", etc.

# debugging in R set a breakpoint inside your function
browser()

# add code at a specified location on the fly
as.list(body(my_func)) # choose line where you want to add code
trace("my_func", quote(if(any(is.nan(lz))) { browser() }), at=4, print=F)

# check if any element is na
 any( !is.na( x ) )

# go up the call stack
recover()

# debug automatically after an error
options(error = recover)
options(error = NULL) # to disable

# debugging in R
debug(my_func) # set debugging flag, you can also do this while debugging
my_func(x,y,z)
# n: next
# c: continue
# Q: quit
# where: stack

# exit with error status code
stop("You did something wrong ", this) # Error: You did something wrong "paco"

# there is no elsif, use "else if(...)" or ifelse(..., ifelse(...))
if (x ==1){
    print('same')
} else if (x > 1){
    print('bigger')
} else {
    print('smaller')
}
ifelse(x == 1, 'same', ifelse(x > 1, 'bigger', 'smaller'))

# vector to string
paste(vector, collapse="|")

# merge dataframes by row names, keep all NAs
merge(a,b, by=row.names, all.x=T, all.y=T)

# set seed, use it before every random call
set.seed(1); runif(1)
set.seed(1); runif(1)

# sleep
Sys.sleep(1)

# saveRDS and readRDS
saveRDS(simulation_1e7, file="simulation_1e7.rds")
b <- readRDS("simulation_1e7.rds")

# append or prepend a string to the elements of a vector
paste("a", c(1,2)) # "a 1" "a 2"

# extract a substring with groups
library(stringr)
str_match("simulation_1e7.rds", "_(.+)\\.")[2]

# create color grid tiles
image(some_matrix)

# change default prompt: http://stackoverflow.com/questions/1448600/change-default-prompt-and-output-line-prefix-in-r
options(prompt=" ", continue=" ")

# hide an axis title name
opts(axis.title.x=theme_blank())

# cbind the elements of a list
a <- list(a1=data.frame(abc=c(1,2,3), ccc=c(3,3,4)), a2=data.frame(abc=c(3,5,7), d=c(1,3,2)))
do.call(cbind,a)
  a1.abc a1.ccc a2.abc a2.d
1      1      3      3    1
2      2      3      5    3
3      3      4      7    2

# rename single column
names(d)[3] <- "three"
names(d)[names(d)=="beta"] <- "two"

# change ggplot color scales
+ scale_fill_manual(values=c("steelblue","red")) # To use for fills
+ scale_colour_manual(values=c("steelblue","red")) # To use for line and point colors

# play a sound when a script is done
sleep(2); alarm()
sleep(2); cat("\a")