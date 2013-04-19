# write tests in inst/tests dev_tools
test("/Users/nacho/Documents/Code/Rprojects/clickme")
test("/Users/nacho/Documents/Code/Rprojects/df2json")
auto_test("/Users/nacho/Documents/Code/Rprojects/clickme/R","/Users/nacho/Documents/Code/Rprojects/clickme/inst/tests")

dev_mode() # toggles
install("/Users/nacho/Documents/Code/Rprojects/clickme")
library("clickme")

# convert a nested list of lists to into a data frame
my_list <- list(list(a = 3,b = 2),list(a = 5,b = 3))
a <- ldply(my_list, data.frame) # works
a <- data.frame(do.call(rbind, my_list)) # doesn't really work, class(a$b) == "list"
# a b
# 3 2
# 5 3

# not in dev mode
library("clickme")

# loads code, data, etc. (in non-global env)
load_all("/Users/nacho/Documents/Code/Rprojects/clickme")
load_all("/Users/nacho/Documents/Code/Rprojects/clickme", T) # to force a true reload

test("/Users/nacho/Documents/Code/Rprojects/clickme") # load_all + test_dir("inst/tests")
document("/Users/nacho/Documents/Code/Rprojects/clickme") # run roxygen

check("/Users/nacho/Documents/Code/Rprojects/clickme")
check("/Users/nacho/Documents/Code/Rprojects/df2json")

# R errors
# argument is of length zero
if(NULL)

# object of type 'closure' is not subsettable
you’re using a list object that doesn’t exist

# remove file extension
strsplit(filename, "\\.")[[1]][1]

# get the file extension
strsplit(filename, "\\.")[[1]][2]

# get user input
response <- readline()
if (tolower(response) == "c"){

}

# use rsq instead of correlation squared
# http://www.win-vector.com/blog/2013/02/dont-use-correlation-to-track-prediction-performance/?utm_source = rss&utm_medium = rss&utm_campaign = dont-use-correlation-to-track-prediction-performance
rsq = function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }
y = runif(10)
x = y + 0.5*runif(10)
cor(x,y) # 0.8893743
cor(y,x) # 0.8893743
cor(10*x,y) # 0.8893743
cor(x+10,y) # 0.8893743
rsq(x,y) # -0.4966555
rsq(y,x) # 0.09424879
rsq(10*x,y) # -9.197255
rsq(x+10,y) # -2250.407

# Rook http server
s$stop()
library(Rook)
load_all("/Users/nacho/Documents/Code/Rprojects/clickme")
s <- start_server("/Users/nacho/Documents/Code/Rprojects/clickme/inst/demo/data_nachocab_scatterplot.html")

# test that
expect_true(x)
expect_false(x)
expect_is(expected, x)
expect_equal(expected, x)
expect_equivalent(expected, x)
expect_identical(expected, x)
expect_matches(expected, x)
expect_output(expected, x)
expect_message(expected, x)
expect_warning(expected, x)
expect_error(expected, x)

# file utilities
file.create(..., showWarnings = TRUE)
file.exists(...)
file.remove(...)
unlink("file")
unlink("dir", recursive = TRUE)
file.rename(from, to)
file.append(file1, file2)
file.copy(from, to, overwrite = recursive, recursive = FALSE,
          copy.mode = TRUE)
file.symlink(from, to)
file.link(from, to)
basename(path)
dirname(path)
dir.create(path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
Sys.chmod(paths, mode = "0777", use_umask = TRUE)
Sys.umask(mode = NA)
system.file(..., package = "base", lib.loc = NULL, mustWork = FALSE)
tempfile(pattern = "file", tmpdir = tempdir(), fileext = "")
tempdir()
file.path(...) # construct the path in a platform-indep way
path.expand(path) # expand leading tilde

# hooks
hook_output = knit_hooks$get('output')
knit_hooks$set(output = function(x, options) { head(x, options$out.lines)} # all chunks call the output chunk
opts_chunk$set(out.lines = 4) # globally
```{r , out.lines = 4}          # or by chunk

knit_hooks$set(foo1 = function(before, options, envir) {
    if (before) {
        "_I appear before a chunk!_\n\n"
    } else {
        "\n\n_I am after a chunk..._"
    }
})

knit_hooks$set(chunk = function(x, options) {
  gsub('```\n+```', '', x)
})

knit_hooks$set(source = function(x, options) {
  paste0('\n\n```r\n', gsub('\\n', '\n', x, fixed = TRUE), '```\n\n')
})

# read r code and insert it into knitr
read_chunk(lines = demo.sub, labels = 'rgl-sub') # demo.sub has r code, we assign it the label 'rgl-sub'
```{r rgl-sub} # run the code
```

# get the column numbers of a dataframe
data.frame(colnames(df))

# pass by reference list
zz <- new.env()
zz$foo <- c(1,2,3,4,5)
make_six <- function(blah) {
  foo <- 6
}
make_six(zz)
zz$foo # 6


# run a chunk inside another one
```{r a}
a <- 3
run_chunk('b') # don't use this, yihui changed it to ref.label and <<label>>
a
```

```{r b}
a <- 4
```

# get an object with a given name
a <- 3
get("a") # 3
eval(parse(text="a")) # 3

# local creates a new environment (perfect for counters)
fn = local({
  i = 0
  function(x) {
    i <<- i + 1 # modify external variable
    paste('Figure ', i, ': ', x, sep = '')
  }
})

# barplot


# set names to an array
setNames( 1:3, c("foo", "bar", "baz")) # instead of tmp <- 1:3;names(tmp) <-  c("foo", "bar", "baz");tmp

# To load R without the .rprofile
R -- vanilla

# divide a matrix by a vector (each column by its vector element)
a <- matrix(c(10,20,30,40,50,60), byrow = T, nrow = 2)
t(t(a)/c(5,7,3))
#    2 2.85714   10
#    8 7.14286   20

# ensure arguments are valid
a <-function(x, y = c("A","B")){ y <- match.arg(y); y}
a() # "A"
a(3,"A") # A
a(3,"C") # Error in match.arg(y) : 'arg' should be one of “A”, “B”
formals(a) # $x $y c("A","B")

# prepare a file for leanback
write_expression_d3(df, name="whatever", type="expression")
write_expression_d3(df, name="whatever", type="volcano")
# http://kaya1.bu.edu/leanback/expression/?file=../data/rescued_genes_lassa_flu_expression.csv&clusters = 1&groups = 1
# http://kaya1.bu.edu/leanback/volcano/?file=../data/rescued_genes_lassa_flu_volcano.csv&clusters = 1&groups = 1


# convert character to date
as.Date("5-oct-12", "%d-%b-%y") #"2012-10-05"
# %d  Day of the month (decimal number)
# %m  Month (decimal number)
# %b  Month (abbreviated)
# %B  Month (full name)
# %y  Year (2 digit)
# %Y  Year (4 digit)

# change locale
Sys.setlocale("LC_TIME", "es_ES")

# sanitize a string, clean, strip
make.names("-.!#paco$2") # "X....paco.2"

# make names unique
make.names(c("a","a","b"), unique = T) # "a"   "a.1" "b"

# horizontal axis labels => 0 = parallel, 1 = all horizontal, 2 = all perpendicular to axis, 3 = all vertical
plot(...,las = 1)

# plot a regression line
plot(dge$norm_counts["ENSG00000075624",])
m_actb <- lm(y~x, data = data.frame(x = 1:4, y = dge$norm_counts["ENSG00000075624",]))
lines(m_actb$fit)

# remove the last two elements of a string by regex pattern
sub('(_[^_]+){2}$','','AA_BB_C-C_DD') # AA_BB
sub('(_[^_]+){2}$','',c('AA_BB_C-C_DD','AAA_BB_C') # AA_BB AAA

# color
palette() # "black"   "red"    "green3"  "blue"  "cyan"   "magenta" "yellow"  "gray"
colors() # white... yellowgreen (657)
palette( rev(rich.colors(32)) ) # rich rainbow

# cummulative sum:  1 ; 2 + 1 = 3; 3+3 = 6; 6+4 = 10
cumsum(1:4) #  1  3  6 10
cumsum(1:10) #  1  3  6 10 15 21 28 36 45 55

# lagged difference: substract the first and the lag+1
diff(c(0,10,4,17), lag = 1) # 10 -6 13; 10-0 = 10; 4-10=-6, basically, what is the change between two consecutive positions.
diff(c(0,10,4,17), lag = 2) # 4 7, 4-0 = 4; 17-10 = 7q

# set rownames and colnames at same time
dimnames(correlation) <- list(colors,colors)

# global variable constant
global_var <<- 3
assign("global_var", 3, envir = .GlobalEnv)

# scale: centers or scales
scale(x, center = TRUE, scale = TRUE) # default, substract column means and divide by the sd of each column, standardize the data
scale(x, center = c(3,4,5), scale = TRUE) # substract center array from every column
scale(x, center = TRUE, scale = c(3,4,5)) # divide the centered columns by scale

# plyr genius
library(plyr)
unrowname(df) # remove row names
arrange(df, column1) # sort by column(s)
arrange(df, column1, column2)
arrange(df, column1, desc(column2))
count(df, "column1") # count unique values
count(df, "column1", "column2") # count unique values
mutate(df, double = 2 * value) # add a column to a df that is a transformation of an existing column df$double <- 2 * df$value
summarise(df, double = 2 * value) # summarize a df

# interesting parts of a ggplot variable (p)
data
layers # geom_line, mapping, pointrange, position
scales # continuous, discrete
mapping
theme
coordinates # transformer
facet # facet_wrap(virus)
plot_env
labels # x,y,group, yintercept, ymin, ymax


# modify an existing column
# h(airquality)
#   Ozone Solar.R Wind Temp Month Day
# 1    41     190  7.4   67     5   1
# 2    36     118  8.0   72     5   2
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
# h(transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8))
#   Ozone Solar.R Wind    Temp Month Day new
# 1    41     190  7.4 19.4444     5   1 -41
# 2    36     118  8.0 22.2222     5   2 -36

# ddply: split-apply-combine
library(plyr)
ddply(input, split, fun_to_apply)
ddply(data, .(gene, timepoints, virus), function(x) data.frame(m_values = mean(x$m_values), sd = sd(x$m_values), se = se(x$m_values), ymin = mean(x$m_values)-sd(x$m_values), ymax = mean(x$m_values)+sd(x$m_values)) )

# add left line to plot before making horizontal y-labels
par(mar = par("mar")+c(0,1,0,0))
plot(...,las = 1)

# horizontal y-labels in ggplot (right-aligned)
ylab("log2\n\nfold\n\nincrease\n\nover\n\nuniversal\n\nreference") +
theme(..., axis.title.y = element_text(size = 15, angle = 0, hjust = 1, face="bold"))

# googleVis
```{r, results="asis"}
df <- data.frame(x = 1:10, y = 1:10)
suppressPackageStartupMessages(library(googleVis))
sc <- gvisScatterChart(data = df, options = list(width = 300, height = 300,  legend='none', hAxis="{title:'x'}", vAxis="{title:'y'}"))
print(sc, 'chart') ## same as cat(sc$html$chart)
```

# ggplot horizontal vertical line
geom_hline(yintercept = 0, linetype="dotted", color="grey80", size = 1)

# knitr Rmd
```{r chunk_label, OPTIONS}
    # R code
```
# create markdown table from R dataframe
```{r createtable, results='asis', echo = FALSE}
cat("x | y", "--- | ---", sep="\n")
cat(apply(df, 1, function(X) paste(X, collapse=" | ")), sep = "\n")
```

# global knitr options
opts_chunk$set(fig.width = 5, fig.height = 5)
opts_chunk$set(cache = TRUE, autodep = TRUE)

# CHUNK OPTIONS knitr
{..., eval = TRUE} # whether to evaluate the code chunk, it can also be numeric vector selecting which expression inside the chunk is executed c(1,3,4) or -(4:5)
{..., echo = FALSE} # hide code, it can also be numeric vector selecting which expression inside the chunk is executed c(1,3,4) or -(4:5)
{..., results='asis'} # output as text (instead of as console output), also "markup" (Sweave verbatim), "hide"
{..., warning = T} # preserve warnings? warning()
{..., error = T} # preserve errors? stop()
{..., message = T} # preserve messages? message()
{..., split = F} # split output into separate files?
{..., include = T} # execute it but include (or not) the chunk output in the final document (for example, generate the images)
{..., engine="bash"} # execute bash, python, awk, ruby, instead of R


# CHUNK OPTIONS code
{..., tidy = T} # tidy up R code
{..., comment=""} # remove any preceding text from console output, default =="##"
{..., prompt = F} # show prompt chars
{..., highlight = T} # highlight the source code?
{..., size='normalsize'} # default latex font size
{..., background='#F7F7F7'} # default latex background color

# CHUNK OPTIONS cache
{..., cache = TRUE} # only run the first time, delete the contents of the cache folder to rerun everything
{..., cache.path='cache/'} # cache dir
{..., dependson = NULL} # char vector of chunk labels (or numeric)

# CHUNK OPTIONS figure
{..., fig.path='figure/mcmc-'} # path prefix to image (avoid spaces), "figure/" by default
{..., fig.keep='high'} # keep high level plots (not abline, points, lines) (also, "none", "all", "first", "last")
{..., fig.show='asis'} # also, "hold" output them at the end of the code chunk, "animate" wrap all plots in the chunk into an animation
{..., dev='png'} # also, "pdf", you can create both dev = c("pdf", "png")
{..., dev.args = NULL} # dev.args = list(bg="yellow",pointsize = 10)
{..., dpi = 72} # dpi
{..., fig.width = 7} # width in inches
{..., fig.height = 7} # height in inches
{..., out.width="300px"} # final width (scaled), 3in, 8cm
{..., out.height="300px"} # final height (scaled)
{..., out.extra="angle = 90"} # rotate final image (doesn't work), out.extra = 'style="float:left;"'
{..., fig.align="default"} # don't align, or "left","center","right"
{..., fig.cap="caption"} # caption
{..., fig.scap="short.caption"} # short . ; :
{..., fig.lp="fig:"} # concatenate "fig:" and the chunk label
{..., fig.pos=""} # latex figure arrangement

# knitr inline
`r x`

#knitr read external file
 read_chunk("script.r")

# convert a matrix to a bioconductor ExpressionSet
ExpressionSet(assayData = my_mat)

# heatmap decent defaults => library(gplots)
heatmap.2(mat,key = TRUE, keysize = 1, density.info="none", trace="none",scale="none",cexRow = 1, dendrogram="both", col = colorpanel(25,"#278DD6","#ffffff","#d62728"), margins = c(10,10), symbreaks = T)

# heatmap.2 options
Rowv = TRUE, Colv= FALSE, # sorting of dendrogram
dendrogram = c("both","row","column","none"), # show or hide
symm = FALSE, # is the matrix square?

# data scaling
scale = c("none","row", "column"), # row by default
na.rm = TRUE,

col = colorpanel(25,"steelblue","white","red"), # number of gradations and colorsk

labCol = NA, rowCol = NA, # to hide col/row labels

colsep = c(2,4,6), rowsep = c(15,25), # separate these rows/cols
sepcolor="white",
sepwidth = c(0.05,0.05), # percentage of row height

# density() computes a kernel density estimate. parameters:
bw # smoothing bandwith. the sd of the smoothing kernel
adjust # what is actually used: adjust*bw, this makes it easy to say use half the bandwith => adjust=.5
from,to # the left- and rigth-most points that must be estimated

# Row/Column Labeling
margins = c(5, 5),
ColSideColors,
RowSideColors,
cexRow = .3, cexCol = .3,  # to make text smaller
labRow = NULL, labCol = NULL,

# example of colsidecolors rowsidecolors (single column, single row)
mat <- matrix(1:100, byrow = T, nrow = 10)
column_annotation <- sample(c("red", "blue", "green"), 10, replace = T)
column_annotation <- as.matrix(column_annotation)
colnames(column_annotation) <- c("Variable X")

row_annotation <- sample(c("red", "blue", "green"), 10, replace = T)
row_annotation <- as.matrix(t(row_annotation))
rownames(row_annotation) <- c("Variable Y")

heatmap.3(mat, RowSideColors = row_annotation, ColSideColors = column_annotation)

# multiple column and row
mat <- matrix(1:100, byrow = T, nrow = 10)
column_annotation <- matrix(sample(c("red", "blue", "green"), 20, replace = T), ncol = 2)
colnames(column_annotation) <- c("Variable X1", "Variable X2")

row_annotation <- matrix(sample(c("red", "blue", "green"), 20, replace = T), nrow = 2)
rownames(row_annotation) <- c("Variable Y1", "Variable Y2")

heatmap.3(mat, RowSideColors = row_annotation, ColSideColors = column_annotation)



key = TRUE, keysize = 1,
density.info="none",
denscol = tracecol,
symkey = min(x < 0, na.rm = TRUE) || symbreaks,
densadj = 0.25,

# plot layout
lmat = NULL, # position of color key, col dendrogram, row dendrogram and heatmap (also rowsidecolors and colsidecolors)
lhei = c(.2,1,.7), # column height of each of the prev. elements
lwid = NULL # column width of each of the prev. elements

# random number between x and y
runif(1,x,y)

# replace elements of a matrix or data frame based on some condition
# head(qpcr)
#   gene_symbol CX8E_day_2 CX8E_day_3 CD8J_day_3 CX8C_day_3
# 1        IL1B      -3.84     -12.82      -5.17      -6.19
# 2         IL6      -2.73      -2.10      -1.27       1.06
# 3        IL21      -2.73      -2.10      -1.27       1.06
replace(qpcr[,-1],qpcr[,-1]>50,50)

# display all brewer palettes: sequential (low:light, high:dark), qualitative (categorical), diverging (mid-range:light, low and high:dark)
display.brewer.all()

# brewer colors
library(RColorBrewer)
colorRampPalette(brewer.pal(7,"Set1"))(100)

# MDS plot
d <- dist(t(mat))
mds <- cmdscale(d)
cols <- as.factor(timepoints)
plot(mds, col = as.numeric(cols), pch = 20, cex = 1.5)
par(xpd=TRUE); legend("bottomright", levels(timepoints), lty = 1, lwd = 5, col = seq(along = levels(timepoints)))

# point size
plot(..., cex = 1.5)



# intersect arrays
union(a,b)
intersect(a,b)
setdiff(a,b) # in a but not in b
setequal(a,b)

# adjust p value BH
p.adj.value <- p.adjust(p.value,method="BH")

# deal with NAs in t-test
obj <- try(t.test(day_0, day_3), silent = TRUE)
if (is(obj, "try-error")) return(NA) else return(obj$p.value)

# exit without making noise
capture.output(return())

# deal with errors
tryCatch(source("http://www.bioconductor.org/biocLite.R"),error = function(e)e)

# png is in pixels and pdf in inches
png(filename, width = 800, height = 800)
pdf(filename, width = 8, height = 10)

# select or slice data frame by column names or row names
df[, c('column_name_1','column_name_2','column_name_3')] # only select, can't remove
subset(df,select=-c(column_name_1,column_name_3)) # negative selection by name
subset(df,select = column_name_1:column_name_3) # select by ranges
subset(df,select=-column_name_0)
subset(df,select=-GeneName) # no quotes!
subset(df,select = c(column_name_1,column_name_3))
subset(df,select = c(column_name_1,3)) # you can combine names and numbers
subset(df, select = grep("col", colnames(df))) # subset by grepping
gene_name_mapper$GeneName[which(gene_name_mapper$ProbeName %in% d$ProbeName)] # select using %in%

# subset by row remove rows by rowname
raw_qpcr[!rownames(raw_qpcr) %in% c("HGDC","RTC1","RTC2","PPC"),]
remove_rownames(df,rownames_to_remove)

# categorical x axis
every_other <- function(labs,side = "x",...){
    l <- labs[seq(1,length(labs),by = 2)]
    if (side == 'x'){
        return(scale_x_discrete(breaks = l,labels = l,...))
    }
    if (side == 'y'){
        return(scale_y_discrete(breaks = l,labels = l,...))
    }
}

x_angle <- theme(axis.text.x = element_text(size = 7,
                                        hjust = 0,
                                        vjust = 1,
                                        angle = 310))

p + x_angle + every_other(levels(dat$x))


# for each element in a, it checks if it's also in b
a %in% b

# substitute
names <- sub("Perez","Pérez",names)

# read from clipboard
a <- read.table("clipboard")

# sort dataframe by the same column of a different data frame
just sort them individually

# sort dataframe by column name
df[order(-df$z), ] # doesn't work on matrix ?
df[with(df, order(-z)), ] # doesn't work on matrix ?

df[with(df, order(-z, b)), ] # several columns

# with
with(pbmc_ce[[2]], fit[fit$genes$GeneName=="HSPA1B",]$coefficients)

# sort dataframe by row names or column names
dat[order(rownames(dat)),order(colnames(dat))]

# sort dataframe by column number
df[order(-df[[3]]),]
df[order(df[[3]],decreasing = TRUE),,drop = F]

# (sort) get the top result of matrix by column 5 decreasing
probabilities[probabilities[,5] == max(probabilities[,5]),]
probabilities[order(-probabilities[,5]),][1,]

# uniquify a vector
a <- c("a","a","b","c","c","c")
dups <- duplicated(a)
a[dups] <- paste0(a[dups], "_", 1:sum(dups)) # "a"   "a_1" "b"   "c"   "c_2" "c_3"

# remove levels drop levels after subsetting
df <- droplevels(df)
df$subsetted_lsit <- factor(df$subsetted_list) # pre r 2.12

# wide to long. id: non-changing columns, variable_name: name of indexing column (otherwise it uses "variable")
library(reshape)
mdf <- melt(df, id = c("probe_id", "accession_number", "gene_name", "gene_description", "p.value"), variable_name="Experiment")

# melt using rownames, adds rownames as a column
melt(as.matrix(df_with_rownames))

# shuffle a vector
sample(x,length(x))

# melt with split by variable name, create a new dataframe
# names(res): "X" "Alpha" "Lambda" "Elastic.Net.Mean" "Elastic.Net.Std"  "LM.Mean"       "LM.Std"
res_m <- res[,c(1,4:7)]
res_m <- melt(res_m, id = c("Gene"))
res_m$variable <- sub("Elastic.Net","Elastic_Net",res_m$variable)
res_m <- with(res_m, data.frame(
        Gene = Gene,
        model = unlist(strsplit(variable,"\\."))[1],
        statistic = unlist(strsplit(variable,"\\."))[2],
        value = value))

# even better
res_m <- data.frame(
        Gene = rep(res$X, times = 2),
        model = c(rep("Elastic Net", times = nrow(res)), rep("Linear Model", times = nrow(res))),
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
ggplot(rna, aes(life_stage,reads, fill = type)) + geom_bar(stat="identity") # stacked bars

# bar chart with error bars
stdev_coords <- aes(ymax = mean_error + sd_error, ymin = mean_error - sd_error)
p <- ggplot(res_m, aes(x = gene, y = mean_error, fill = model)) +
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(stdev_coords, width = 0.1, position="dodge")

# ggplot summary stats
stat_summary(fun.data="mean_cl_normal", aes(colour = gene), geom="line", size = 1.5, width=.1, mult = 1) # mean_cl_normal comes from Hmisc smean.cl.normal, use mult = 1 to avoid extra margins
stat_summary(fun.data="mean_cl_boot", aes(colour = gene), geom="pointrange")
stat_smooth(aes(colour = gene), method="loess", se = F, size = 1.5)

# add contour around geom_bar
geom_bar(color="black")

# standard deviation sd ggplot
data # x = timepoints, y = m_values, so std_devs needs the m_values column (that's where the point appears)
std_devs <- ddply(data, .(gene, timepoints, virus),function(x) data.frame(m_values = mean(x$m_values), sd = sd(x$m_values), se = se(x$m_values), ymin = mean(x$m_values)-sd(x$m_values), ymax = mean(x$m_values)+sd(x$m_values)) )
p + geom_pointrange(aes(colour = gene, ymin = ymin, ymax = ymax, group = gene), data = std_devs, position = position_dodge(.3))

# rename columns dataframe
x <- c("a" = 1, "b" = 2, d = 3, 4); rename(x, c("d" = "c"))
rename(df, c(old_name1 = "new_name1", old_name2 = "new_name1", old_name3 = "new_name2"))

# remove null entries from a list
compact(list(3,NULL))
# [[1]]
# [1] 3

# remove names
unname(vector)

# uninstall a package
remove.packages("package_name")

# To index by coords (row and col): rename MAwin$M, then generate those names for the new "top"
rownames(ce_lassa$MAwin$M) <- paste0(ce_lassa$MAwin$genes$GeneName, "_", ce_lassa$MAwin$genes$Row, "_", ce_lassa$MAwin$genes$Col)
ce_lassa$bad_top_coords_d3 <- na.omit(ce_lassa$all[ce_lassa$all$P.Value < .005, c("Row","Col", "GeneName", "ProbeName")])
ce_lassa$bad_top_coords_d3 <- paste0(ce_lassa$bad_top_coords_d3$GeneName, "_", ce_lassa$bad_top_coords_d3$Row, "_", ce_lassa$bad_top_coords_d3$Col)
ce_lassa$bad_top_d3_M <- ce_lassa$MAwin$M[ce_lassa$bad_top_coords_d3,]
# then, to extract the gene_names
library(stringr)
bad_genes <- sapply(rownames(ce_lassa$bad_top_d3_M), function(x) str_match(x,"(^[^_]+)")[1])

# limma duplicate correlation time series longitudinal (but you can't have weird designs...)
# When the time course is of a repeated measures nature, you can estimate the correlation between the repeated measures using the duplicateCorrelation() function in limma, with the block argument indicating each time course replicate. The correlation is then input to the lmFit() function and carried through all the analysis.

# install from github
install_github("knitr","yihui")

load_all("path_to_downloaded_package", reset = T)

# add manual palette ggplot
categorical_10 <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
+ scale_fill_manual(values = categorical_10)
+ scale_colour_manual(values = categorical_10)
categorical_8 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# string length string number of characters
nchar(str)

# Save a graph
ggplot(reads_by_life_stage, aes(life_stage,reads, fill = type)) + geom_bar(stat="identity")
cat("Saving..."); filename <- paste0("plots/reads_by_life_stage.png");ggsave(filename, width = 8, height = 6)
message("\rSaved: ", filename)

filename <- paste("cdon.png", sep="");png(filename,width = 1024,height = 500,units="px",bg = "transparent")
print(p)
dev.off(); print(paste("Saved:", filename))

# Add points with different aes to an existing graph
ggplot(clusters,aes(V1,V2)) +
    geom_point(data = clusters, aes(V1,V2,col = V4,shape = V3+15,size = 7)) +
    geom_point(data = centroids, aes(V1,V2, col = V3),size = 10, inherit.aes = F)

# Remove legend ggplot
theme(legend.position="none")

# legend ggplot
theme(..., legend.position="bottom", legend.direction="horizontal", legend.box = "vertical")

# Increase ggplot outer margins (x-right:1, x-left:1, y-top:0.5, y-bottom:0.5)
library(grid); theme(plot.margin = unit(c(1,1,25,25), "lines")) #TRBL

theme(...axis.title.y = element_text(vjust=-.5), plot.margin = unit(c(1,1,1,2), "lines")) # great to add margin to y label y axis label

# plot two ggplots
plot1 <- plot_jitter( cfg(gfc(plot_probes), ce_lassa2), list(ce_lassa2), show_points = F, show_sd = F)
plot2 <- plot_jitter_qpcr(plot_probes,qpcr)
grid.arrange(plot1, plot2, ncol = 2)

# opts => theme
# rotate x axis 90 degrees vertical (ggplot)
theme(axis.text.x = element_text(angle = 90, hjust = 1)) #hjust = 0 is left align, hjust = 1 is right aligned
theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# rotate axis labels
plot(..., las = 1)
axis(1, las = 2)

# vjust: negative values the text away from the axis

# transparent opacity
rgb(255,0,0,75, maxColorValue = 255) # transparent red or just use "#FF00004B"
rgb(0,0,0,75, maxColorValue = 255) # transparent black or just use "#000004B"

# arrows
arrows(start_x, start_y, end_x, end_y, length=.1, angle=20, lwd=3, col = transparent("red", alpha=.3))

## ggplot opacity
alpha = 1/4

# Group and facet
ggplot(sub_m_cu,aes(codon,rf, group = gene, color = gene)) + geom_line() + facet_wrap(~aa, scale="free_x")
ggplot(global_rf, aes(codon,rf)) + geom_line(aes(group = aa, color = aa), size = 2) + facet_grid(. ~ aa, scale="free_x", space="free") + p_global_opts + ylim(0,1)

# Limit axis
ggplot(maa_current,aes(codon,value, group = gene, color = cluster)) + ylim(0,1)

# custom labels ggplot
scale_y_continuous(labels = function(x) format(10^x, digits = 2)) +

# transparent jitter and summary line
ggplot(myaa_cc, aes(reorder(codon,rf,sum),rf, group = gene)) + geom_line(alpha = 1/15, position = jit) + stat_summary()

# Hide all the gridlines
bp + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

# annotate individual facets
annotation <- data.frame(experiment = c("1-Control Proestrus"), Sample = 41, Average = 3, label = c("34.4"))
p <- p + facet_grid(. ~ experiment, scales = "free")
p + geom_text(aes(label = label), data = annotation)

starts <- c(1.5,3.5,2.5)
reference <- data.frame(x1 = starts, x2 = starts+1, virus = c("lassa","ebola2","marburg"))
reference$virus <- factor(reference$virus, levels = c("lassa","ebola2","marburg"))

# decent venn diagrams
library(bvenn)
bvenn(get_combined_variable(all,"top_genes"), colors = brewer.pal(7,"Set3"))

# uneven time series annotations, color gradient jitter
p + geom_jitter(aes(x = dpi, y = log2_ratio, col = log2_ratio), position = position_jitter(width=.1), size = 4) + # make sure that aes() isn't set in ggplot(), but in the geometry
scale_colour_gradient(high="#BB232F",low="steelblue") +
facet_grid(. ~ virus, scale="free_x", space="free") +
geom_rect(data = reference,aes(xmin = x1, xmax = x2, virus = virus), ymin=-Inf, ymax = Inf, fill="grey", alpha=.4)

# change facet text
theme(strip.text.x = element_text(...))

# remove facet strips
theme(strip.text.x = element_blank(), strip.background = element_blank())

# theme text
element_text(family = "", face = "plain", colour = "black", size = 10, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 1.1)

# split string
unlist(strsplit("a.b.c",".", fixed = T)) # literally
unlist(strsplit("a.b.c","\\.", fixed = F)) # regexp

# array contains includes element?
v <- c('a','b','c','e')
'b' %in% v ## returns TRUE
any(v=="b")
match('b',v) ## returns the first location of 'b', in this case: 2

# sort strings with embedded numbers
library(gtools)
mixedsort(levels(m$dpi))
# [1] "day_0"  "day_1"  "day_3"  "day_6"  "day_8"  "day_10" "day_12"

# average mean by groups
aggregate(value ~ group, molten_df, mean) # molten_df$value, molten_df$group

# if you want to aggregate by some of the columns, but not all
a <- data.frame(probe = c("probe1","probe2","probe3","probe4"), gene = c("gene1","gene1","gene2","gene1"), value = c(.001,.1,.05,.001))
#   probe  gene  value
# 1 probe1 gene1 0.001
# 2 probe2 gene1 0.100
# 3 probe3 gene2 0.050
# 4 probe4 gene1 0.001
aggregated <- aggregate(value~gene, data = a, FUN = min)
#    gene value
# 1 gene1 0.001
# 2 gene2 0.050
b <- merge(aggregated, a) # YOU MIGHT STILL GET DUPLICATE GENES AT THIS STAGE becase probe1 and probe4 had the same value
#    gene value  probe
# 1 gene1 0.001 probe1 ***
# 2 gene1 0.001 probe4 ***
# 3 gene2 0.050 probe3
b <- merge(aggregate(value~gene, data = a, FUN = min), a)
aggregate(b, by = list(b$gene), function(x) x[1])[,-1]
#    gene value  probe
# 1 gene1 0.001 probe1
# 2 gene2 0.050 probe3
ddply(a, .(gene), function(x) x[which.min(x$value),]) # more elegant, but too slow with a big dataframe

# ddply is slow
# 11:23:01 > h(mulatta$mnc)
#        gene_symbol   sample   counts lane monkey timepoint
# 14925    5_8S_rRNA l1_m1_d0  529.241   l1     m1        d0
# 32255    5_8S_rRNA l1_m1_d3 3633.144   l1     m1        d3
a <- ddply(mulatta$mnc, .(gene_symbol,timepoint), summarize, median = median(counts)) # too slow
a <- aggregate(counts~gene_symbol+timepoint, data = mulatta$mnc, median) # do this
dcast(a, gene_symbol ~ timepoint, value.var="counts") # to unmelt

# write data frame to file
write.table(ratList, file = "ratList.csv", sep = ";", quote = F, row.names = F, col.names = F)

# write text to a file
writeLines(text,file)

# write data frame to clipboard
pbcopy(data)

# to read data with gene descriptions, disable quotes, using @ is also advised because sublime text merges trailing tabs (not a problem if you don't have empty fields)
ensembl_info <- read.table("/Users/nacho/Documents/BU/Connor/projects/cmf/raw_data/genome/ensembl_69.txt",sep="@", quote="")

# read data from clipboard
a <- read.table(pipe("pbpaste"))

# don't add X to column names
read.table("path.txt", check.names = FALSE)

# histogram vs density plot
plot((density(na.omit(ce_lassa$all$P.Val)))) # doesn't allow NAs, you can change the bw of density(). Ex: bw=.1
hist(ce_lassa$all$P.Val, breaks = 50)

# numerical differentiation derivative
x <- -4:4
y <- x^2
f <- splinefun(x,y)
plot(x,y, type="b")
lines(x,f(x,deriv = 1))

# super hack to show filled squares instead of lines in legend
GeomLine2 <- proto(GeomLine, {
    objname <- "line2"
    guide_geom <- function(.) "polygon"
    default_aes <- function(.) aes(colour = "black", size = 0.5, linetype = 1, alpha = 1, fill = "grey20")
     })
geom_line2 <- GeomLine2$build_accessor()

g_qpcr <- ggplot(m_qpcr, aes(experiment, value, group = probe_id, color = gene_name, fill = gene_name))
g_qpcr <- g_qpcr + geom_line2(aes(size = log(1/p_value)))

# don't convert strings to factor
data.frame(... stringsAsFactors = F)

# reorder the levels of a factor
f <- factor(c("a","b","c"), levels = c("c","a","b"))
df$factor <- factor(df$factor, levels = c("c","a","b"))
unordered_factor <- relevel(unordered_factor, "Ref") # relevel with a new reference value (puts the reference value first)
reorder(f, c("c","a","b")) # you need to specify every item in f in the right order,
raw$dpi <- factor(raw$dpi, levels = mixedsort(levels(raw$dpi))) # using day_0, day_3 from gtools

# rename levels
# Rename by name: change "beta" to "two"
levels(x)[levels(x)=="beta"] <- "two"
# Rename by index in levels list: change third item, "gamma", to "three"
levels(x)[3] <- "three"
# Rename all levels
levels(x) <- c("one","two","three")
# Rename all levels, by name
x <- factor(c("alpha","beta","gamma","alpha","beta"))
levels(x) <- list(A="alpha", B="beta", C="gamma")
# "A" "B" "C"
# This only works if ALL levels are set in the list; if any are not in the list,
# they will be replaced with NA

# make a factor ordered
f <- factor(c("a","b","c"), levels = c("c","a","b"), ordered = T)
data$f <- ordered(data$f, levels = c("c","a","b"))

# cool ggplot docs
http://had.co.nz/ggplot2/docs/


# rename axis title
scale_y_continuous(name="Fold Change")

# increase font size and maintain vertical
theme(axis.title.y = element_text(size = 20, angle = 90, face="bold"))

# use decent sizes
g_qpcr <- g_qpcr + theme(plot.title = element_text(size = 25), axis.text.x = element_text(angle=-90, hjust = 0, size = 20), axis.title.y = element_text(size = 20, angle = 90), axis.title.x = element_text(size = 20), axis.text.y = element_text(size = 20))

# To build a dataframe in R by reading from a shell pipe

data <- read.table(pipe("cat /dev/stdin"))
> cat my_data | Rscript reader.R

# To use a shell command and "interpolate" an R variable
system(paste("echo",myLongString,"|pbcopy"),intern = T)

pattern_variable <- "big_file_*"
paths <- system(paste("ls",pattern_variable), intern = T)
paths
> big_file_1       big_file_2       big_file_3

# insert a string inside another string: (use sub?)
path <- "dir/my_file"
randomized_path <- system(paste("echo ", path, " | sed 's/\\(.*my_\\)/\\1randomized_/'", sep=""), intern = T)
randomized_path
dir/my_randomized_file

# open in sublime
system("subl qpcr_pairwise_proestrus.csv")

# find out size of current device window
dev.size(units="px")

# new device
dev.new(width = 5, height = 4)

# you can't change axis position (top, right)

# reduce number of significant positions decimals, round
signif(df,4)
format(df, scientific = F)
format(df, digits = 3)
format(.323,digits = 1) # 0.3
format(.001,digits = 1) # 0.001
options(digits = 2)

# install a bioconductor package
source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
biocLite("DESeq")
biocLite("org.Hs.eg.db")

# get current version of package
packageVersion('knitr')
sessionInfo()
installed.packages()
packageDescription("ggplot2")["Version"]

# list of loaded packages
(.packages())

# reset par()
dev.off()

# return multiple arguments, load multiple arguments
lapply(list("a","b"), function_that_returns_two_arguments)

# par good practice
old_par <- par()
    par(mfrow = c(1,2)) # multiple plots
    plot(a)
    plot(b)
par(old_par)

# conditional color
plot(..., col = ifelse( fit$padj < p.value.cutoff, "red", "black" ))

## Frequency tables ##
# 1. (case=>table) If you start with a dataframe in case form, you can convert to table form by using table():
repeated.rows <- data.frame(smoker = c("Y","Y","Y","N","N"), diagnosis = c("cancer","cancer","nothing","nothing","cancer"))
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

# 7. wide to long using rownames, add rownames as a column
df$genes <- rownames(df)
melt(df)

# long to wide
library(reshape2)
data.wide <- dcast(origdata.long, row ~ column, value.var="measurement") # this is useful to quickly convert from molten/long to wide
dcast(data_ces, gene ~ timepoint, value.var="M_norm", fun.aggregate = mean) # many probes per gene
list(.(a + b), (c = round(c))) # instead of formula

# colSums sum a molten dataframe
dcast(molten_df, vars_to_group_by ~ ., value.var="var_to_aggregate_by", fun.aggregate = sum)
dcast(mulatta$mrc, lane + monkey + timepoint ~ ., value.var="counts", fun.aggregate = sum)

## model formulas ##
# simple linear regression model of y on x
y ~ 1 + x   # explicit intercept (+ means inclusion, optional for the first term)
y ~ x       # implicit intercept

# simple linear regression of y on x through the origin (without an intercept)
y ~ 0 + x
y ~ -1 + x   # (- means exclusion)
y ~ x - 1

# saturated model
# using SAS proc genmod: y = x1 x2 x1*x2
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
GSS <- data.frame(expand.grid(sex = c("female", "male"),party = c("dem", "indep", "rep")),
           count = c(279,165,73,47,225,191))
# customized way of doing it
politics <- data.frame(
                       gender = rep(rep(c("male", "female"),each = 2),3),
                       race = rep(c("white", "black"),6),
                       party = rep(c("Democrat","Republican","Independent"),each = 4),
                       count = c(132,42,172,56, 176,6,129,4, 127,12,130,15))

# repeat a dataframe a given number of times specified in one of its columns
mydf <- data.frame(col1 = c(1, 2, 3, 4),
                   col2 = c(5, 6, 7, 8),
                   col3 = c("a", "b", "c", "d"), stringsAsFactors  = FALSE)
index <- rep(seq_len(nrow(mydf)), each = 3)
mydf[index, ]

# reverse order of columns in a dataframe
rev(df)

# repeat the rows of a dataframe (use rownames(mt))
a <- c("a","b","c")
m <- c("m1","m2","m3","m1","m2","m2","m3")
t <- c(11,11,11,22,22,33,33)
mt <- data.frame(m,t)
mt[rep(rownames(mt),each = length(a)),]
mt[rep(seq_len(nrow(mt)),each = length(a)),] # more efficient?

# repeat two vectors into a dataframe
a <- c("a","b","c")
b <- c("A","B","C")
c <- c(1,2,3)
d <- c(11,22)
nrow_cd <- length(c)*length(d)
sapply(list(a,b), rep, times = nrow_cd)

# get the names of a table. dimnames is a list
dimnames(HairEyeColor)

# to create a table
JobSat <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4)
dimnames(JobSat) = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
                 satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS"))
JobSat <- as.table(JobSat)

# modify an existing list
a <- list(a = 3, b = list(c = 3,d = 4))
modifyList(a, list(a = 4, b = list(c = 4), e = 3)) # changes a and c, keeps d, and adds e

# Use formulas to display tables
library(vcd)
structable(Hair+Sex ~ Eye, HairEyeColor)

# gradient color ggplot
+ scale_colour_gradient(high="#BB232F",low="steelblue")

# sum across one variable of an n-way table
margin.table(HairEyeColor,1)

# test for association between two variables using log-likelihood ratio g2 test, chi square test, etc.
library(vcd)
assocstats(GSStab)

# loglm and glm require dataframes in frequency form

# remove delete all objects in environment
rm(list = ls())
rm(list = setdiff(ls(),c("rf","source_dir")))
rm(list = setdiff(ls(),c("current_object_a","current_object_b")), pos = globalenv()) # do it inside a function

# add gene names ensembl
ensg_to_gene_name_table <- read.table("ensembl_gene_id_to_gene_name.csv",sep=";", header = T)
ensg_to_gene_name <- as.vector(ensg_to_gene_name_table[,1])
names(ensg_to_gene_name) <- ensg_to_gene_name_table[,2]
save(ensg_to_gene_name, file="ensg_to_gene_name.RData")
add_gene_names <- function(ensg_ids) {
    ifelse(!is.na(ensg_to_gene_name[ensg_ids]), ensg_to_gene_name[ensg_ids],ensg_ids)
}

# specify which column holds the row names in a dataframe
read.table(..., row.names="GeneName")

# grep gene_info o microarray
grep -i "kappa" ~/gene_info/Homo_sapiens.gene_info | cut -f2,3,9
grep -i "FCGR3"  ~/Documents/BU/Connor/projects/hfv/raw_array_data/marburg/PBMC/252665214252_201109091420_S01_GE2_1010_Sep10_1_1.txt | cut -f13,14,16

# rotate text angle
text(...,srt=-45)
boxplot(t(raw_qpcr), ylab="Ct", xaxt="n", xlab="") # xaxt hide x labels
text(1:nrow(raw_qpcr), par("usr")[3] - 0.25, srt = 90, adj = 1, labels = rownames(raw_qpcr), xpd = T, font = 1, cex=.7)

# text overflow plot frame
par(xpd = NA)
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
install.packages("/Users/nacho/Documents/Code/Rprojects/Design_2.3-0.tar.gz", repos = NULL, type="source")
install.packages("/Users/nacho/Documents/Code/Rprojects/pheatmap 3", repos = NULL, type="source")
install.packages("/Users/nacho/Documents/Code/Rprojects/limma2", repos = NULL, type="source") # change the name of package in DESCRIPTION file
detach("package:limma2", unload = T); install.packages("/Users/nacho/Documents/Code/Rprojects/limma2", repos = NULL, type="source"); library(limma2)
detach("package:knitr", unload = T); install.packages("/Users/nacho/Documents/Code/Rprojects/knitr", repos = NULL, type="source"); library(knitr)


# return an object without missing values
na.omit(df) # better than x[!is.na(x)]

# create a package (requires devtools, roxygen2, testthat)
create("/Users/nacho/Documents/Code/Rprojects/clickme")
build("/Users/nacho/Documents/Code/Rprojects/clickme")
wput df2json_0.0.1.tar.gz ftp://anonymous:nachocab@gmail.com@CRAN.R-project.org/incoming/
# email CRAN@R-project.org
# Subject: CRAN submission df2json 0.0.1
# Body: I have read and agree to the CRAN policies.

# roxygen commands http://roxygen.org/roxygen2-manual.pdf
#' @export # make the function visible
#' @import package1 package2 # to require existing packages
#' @param name description # document a parameter
#' @examples R code
#' @return # Used to document the object returned by the function. For lists, use the \item{name a}{description a} describe each component of the list
#' @note contents
#' @seealso Text with \code{\link{function}} Pointers to related R objects

# make a hidden variable in a package visible
MASS:::vcov.polr

# For every element in a, return either the position where it occurs in b or NA.
untranslated <- c("B","@","@","A","@","C","C")
from <- c("A","B","C")
to <- c("a","b","c")
if (nrow(to) != nrow(from)){
    stop("Repeated probe names in agilent translator file ", agilent_translator_file, "'from' and 'to' have different number of rows")
}
translatable_positions <- which(untranslated %in% from) # 1 4 6 7 : positions in "untranslated" which are in "from"
translated <- to[match(untranslated[translatable_positions],from)]
untranslated[translatable_positions] <- translated

# match and which and match don't confuse
target <- letters
query <- c("g","a","d","A","B","f")
match(query,target) #  7  1  4 NA NA  6
which(target %in% query) # 1 4 6 7

match(c("a","a","c","b"), target) # 1 1 3 2
which(c("a","b","c") %in% c("a","a","c","b")) # 1 2 3
which(c("a","a","c","b") %in% c("a","b","c")) # 1 2 3 4

x <- x[!is.na(x)] #  7  1  4 6
match("a", c("a","b","c")) # 1

# create a vector with names
a <- c("a"=3,"b"=4)

# invert reverse an array
rev(c("c","a","b")) # "b" "a" "c"

# invert rows in mat
mat <- mat[ nrow(mat):1, ]


# multiply two arrays to make a matrix
outer(c(1,2,3),c(4,5))
#      [,1] [,2]
# [1,]    4    5
# [2,]    8   10
# [3,]   12   15

# to see the callback stack after an error
traceback()

# manual shapes ggplot
p <- p + geom_pointrange(aes(colour = gene, ymin = ymin, ymax = ymax, group = gene, shape = gene), size = 1, data = std_devs, position = position_dodge(.3)) +
        scale_shape_manual("Gene",values = as.numeric(factor(u(std_devs$gene)))+15) # solid from 15 onward

# measure times
started <- proc.time()

print(proc.time() - started)

# substring
get_the_b <- substr("aaabaa", 4, 4)

# grep list regex. Expression can use ^ $ etc
sample_names <- c("P1N3","P2N3", "P4N5")
grep("P4",sample_names) # select those that match the expression
grep("P4",sample_names, invert = T) # select those that don't match the expression

# find index of value in an array (don't confuse which with match)
which(c(5,6,7)==6) #2
a$prop1[which(a$prop2 %in% b)] # without which you get TRUE FALSE TRUE ...
which(matrix==6, arr.ind = TRUE)
#        row col
# [1,] 27487   2

# use which.min instead of match(min(arr))
which.min(arr)

# see which elements match a string
grep("day_0",colnames(qpcr$two_neg_delta_ct)) # which, returns index
grepl("day_0",colnames(qpcr$two_neg_delta_ct)) # returns logical

# make text bigger
plot(..., cex = 4) # text size = cex*ps

# make title bigger
plot(..., cex.main = 5, mar = c(5, 4, 4, 2) + c(.1,.1,7,.1)) # BLTR

# change font weight, you can see al the fonts by doing X11Fonts()
plot(..., font = 2)   #(1 = plain, 2 = bold, 3 = italic, 4 = bold-italic)

# config file /Library/Frameworks/R.framework/Resources/fontconfig/conf.avail/60-latin.conf
plot(..., family="Helvetica")
text(1:4, 1:4, paste("Font face", 1:4, "for this text"),family="Palatino")
filename <- paste("fonts.png", sep="")
png(filename, width = 800, height = 800); plot(c(1,100),c(1,100));text(seq(1,100,10), seq(1,100,10), paste("Font face", 1:4, "for this text"), family="Helvetica", font = 2, cex = 2);dev.off();

# show labels
df_text <- df[df$gene_symbol %in% manual_genes,]
text(text_x,text_y, labels = chaperone_genes, col="red", pos = 3, font = 2, xpd = NA)
# pos 1: right-aligned, font 2: bold
#     2: bottom
#     3: top
#     4: left-aligned


# don't confuse rank and order
rank(-c(10,30,40,20,10), ties.method="first") #  4 2 1 3 5
order(-c(10,30,40,20,10)) # 3 2 4 1 5

# pause between plots
par(ask = T)

# rank
rank(c(10,20,3,1,50)) # 3 4 2 1 5  [1   3 10 20 50]
rank(-c(10,20,3,1,50)) # 3 2 4 5 1 [50 20 10  3  1]

# add a text label by clicking
text(locator(1),"84")

# log scale in plot (no ggplot)
plot(raw_s2_8$PA_glucose, raw_s2_8$frac_glucose, pch = 20, log="xy", xlim = c(10,10^4), ylim = c(10^-4,1))
plot(density(log10(counts),adjust = adjust)) # to get the same as scale_x_log10

# black background or no background
par(bg="black", col="white", col.main="white") # to reverse: par(bg="white", col="black", col.main="black")
par(bg = NA, col="white", col.main="white")

# no box around plot (box type)
par(bty="n")
par(bty="o") # default: all around

# line type: c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
plot(..., lty="solid")

# remove the 4% extension of plot origin, it's "r" by default
par(xaxs = "i", yaxs = "i")

# remove axes
plot(..., axes = F)

# plot line width
plot(..., lwd = 3)

# plot type: "l", "p","b", "n"
plot(..., type="l")

# increase external margins: 0 by default
par(oma = c(2,2,2,2)) # (B, L, T, R)

# increase internal margins
par(mar = c(5, 4, 4, 2) + c(.1,.1,2,.1)) # (B, L, T, R)

# plot fill
y <- c(7,5,6)
x <- 0:(length(y)-1) # or 1:length(y)
plot(x,y, type="n", xlim = c(0,max(x)), ylim = c(0,max(y)))
polygon(c(min(x),x,max(x)), c(0, y,0), col="blue", border = NA)

# decent graphics with palette
old_par <- par()
par(bg = palette[1], cex.main = 3, mar = c(5, 4, 4, 2) + c(.1,.1,3,.1), xpd = NA)
plot(..., cex = 4, axes = F, bty="n", font = 2, xlab="", ylab="", font = 2)
par(old_par)

# ggplot log scale
library(MASS)
library(scales)
scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))  + annotation_logticks(sides="l")  +

## ggplot bw
theme_bw()

# limits ymax xmin
+ scale_y_continuous(limits = c(min_log_ratio,max_log_ratio))


# rename axis title ggplot
+ xlab("paco") + ylab("luis")

# assign a string into a variable
assign("a",3) # > a == 3
x <- 42
eval(parse(text = "3 + 4")) # you can even call functions!
# 7
get("x") # eval(parse(text = "x"))
[1] 42
x <- 42
deparse(substitute(x)) # get name of a variable, variable name, this is a problem if you want a filename and instead of x you write blah()
sapply(substitute(list(...))[-1], deparse) # to do it for a list, for example to get dots ...
# "x"
# (s.e <- substitute(expression(a + b), list(a = 1)))  #> expression(1 + b)

# function input variable name
a <- function(x) deparse(substitute(x))
a(b) # "b"

# no warnings when executing a command
suppressWarnings(expr)

# suppress output (silent)
capture.output(cat("Hi\n"))
invisible(expr())

## difference PCA (principal component analysis) and MDS (aka PCO, principal coordinate analysis, or classical MDS)
Multidimensional scaling operates on a matrix of similarities between rows (so the axes cannot be interpreted),
whereas PCA operates on a matrix of similarities between columns (so the axes can be interpreted).

# uppercase upcase
toupper("paco") # PACO
toupper(c("paco","luis")) # "PACO" "LUIS"

# aggregate (returns a data.frame)
cdata.means <- aggregate(data[c("before","after","change")],
                         by = data[c("sex","condition")],
                         FUN = mean, na.rm = TRUE)

aggregate(mat_with_probe_names, by = list(gene_names_for_each_probe_name), median) # the first column of output contains unique gene_names

# lazy loading of default arguments
p <- function(args, a = args$a,b = args$b,c = args$c){
    message(a,b,c,"\n")
}
my_args <- list(a = 1,b = 10, c = 5)
p(my_args) # 1105
p(my_args,b = 3) # 135

# lazy loading of default arguments
deviation <- function(a,b = mean(a)){
    b-a
}
deviation(c(1,43,2,23)) # 16.25 -25.75  15.25  -5.75
deviation(c(1,43,2,23),b = 0)

# lazy evaluation of arguments
p <- function(a,b = my_var){
    my_var <- 3*a
    a*b
}
p(2) # 12

# remember you can return a function

my_plot <- function(color){
    p <- function(values) plot(values, col = color, pch = 16)
    p
}
blue_plots <- my_plot("blue")
blue_plots(c(1,2,3))
blue_plots(c(4,5,6))

# expand a vector into the arguments of a function
fun <- function(a,b,c){a+b^2*c}
do.call(fun, as.list(c(1,2,3))) # (do.call expects a list)
do.call("paste", c(tmp, sep="")) # pass arguments to FUN, tmp is already a list or a data.frame (quotes around function are optional)
do.call(rbind,lapply(ces, get_molten_data)) # don't confuse with do.call(get_molten_data, ces), ces is a list of ce objects

# 1 10 5
# Create a directory, or no warning if it exists
dir.create(path, showWarnings = F))

# build path
file.path("path","paco") # "path/paco"

# check if file exists
file.exists(filename)

# check if a variable is defined
exists("my_var_name")

# get n random elements from a vector without replacement randomize scramble
sample(my_array, length(my_array))

# add a legend to a plot (x-coord, y-coord, text, line type, line width, colors), you can also use fill=
# or bottomleft, topleft, topright, top, bottom, left, right, center
par(xpd=TRUE); legend("topright", levels(categories), lty = 1, lwd = 5, col = seq(along = levels(categories)))
par(xpd=TRUE); legend(10,40, rownames(rates), lty = 1, lwd = 5, col = c("steelblue", "red"))
par(xpd=TRUE); legend(10,40, rownames(rates), pch = 16, col = c("steelblue", "red"))

# don't use matplot, use plot_lines for decent colors
plot_lines(mat) # matplot(t(mat))

# rename x axis y axis matplot
plot(..., xaxt="n")
axis(1, at = 1:8, labels = my_labels)

# read a file into a vector or list, alternative to read.table
scan(path, "character") # or "integer", etc.

# debugging in R set a breakpoint inside your function
browser()
# enter - go to next statement
# c - continue without single stepping
# n - execute next statement
# where - show call stack
# Q - halt execution

# add code at a specified location on the fly debug browser
as.list(body(gvisFormat)) # choose LINE where you want to add code
LINE <- 3
trace("gvisFormat", quote(if(any(is.nan(lz))) { browser() }), at = LINE, print = F)

# check if any element is na
 any( !is.na( x ) )

# go up the call stack
recover()

# debug automatically after an error
options(error = recover)
options(error = NULL) # to disable

# debugging in R
sb("path_to_my_fun.r", line_num)
my_fun()
usb(my_fun)

debug(my_func) # set debugging flag, you can also do this while debugging
my_func(x,y,z)
# n: next
# c: continue
# Q: quit
# where: stack
    ls()
    # [1] "dat"
    options(error=dump.frames)
    mi(dat)
    ls()
    # [1] "dat"       "last.dump"  # Apparently there WAS an error

    # INVESTIGATE WITH debugger()
    debugger(dump=last.dump)

# exit with error status code
stop("You did something wrong ", my_variable) # Error: You did something wrong "paco"

# there is no elsif, use "else if(...)" or ifelse(..., ifelse(...))
if (x ==1){
    print('same')
} else if (x > 1){
    print('bigger')
} else {
    print('smaller')
}
ifelse(x == 1, 'same', ifelse(x > 1, 'bigger', 'smaller')) # ternary ?

# jitter
geom_jitter(position = position_jitter(width=.15)

# vector to string
paste(vector, collapse="|")

# merge dataframes by row names, keep all NAs. "row.names" creates a Row.names column
m <- merge(a,b, by="row.names", all.x = T, all.y = T)
m <- merge(a,b, by.x="row.names", by.y="GeneName", all.x = T, all.y = T)
rownames(muscle_cluster_ids_fc) <- m$Row.names
m <- subset(m, select=-c(Row.names))

# you can also merge by multiple columns
merge(b,a, by.x = c("row","col"), by.y = c("a","b"))

# set seed, use it before every random call
set.seed(1); runif(1)
set.seed(1); runif(1)

# merge several dataframes
library(reshape)
merge_recurse(list(df1,df2,df3))
merge_all(list(df1,df2,df3)) # only those with common columns

# serialize an object
dput(my_df) # df <- structure(list(hpi_0 = c(1L, 1L, 3L), hpi_0_5 = c(0L, 0L, 1L),
    hpi_2 = c(7L, 4L, 3L), hpi_6 = c(24L, NA, NA), hpi_18 = c(8L,
    # NA, NA), hpi_1 = c(NA, 1L, 2L), hpi_4 = c(NA, 193L, 31L)), .Names = c("hpi_0",
    # "hpi_0_5", "hpi_2", "hpi_6", "hpi_18", "hpi_1", "hpi_4"), row.names = c(NA,
    # -3L), class = "data.frame")


# sleep
Sys.sleep(1)

# sort character string embedded numbers numeric
mixedsort(colnames(anjan_9$counts)) # hpi_0 hpi_0_5 hpi_2 hpi_6 hpi_18


# set env variable
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/Users/nacho/bin", sep=":"))

# saveRDS and readRDS
saveRDS(simulation_1e7, file="simulation_1e7.rds")
b <- readRDS("simulation_1e7.rds")

# append or prepend a string to the elements of a vector
paste("a", c(1,2)) # "a 1" "a 2"

# extract a substring with groups
library(stringr)
str_match("simulation_1e7.rds", "_(.+)\\.")[2]

# str_replace
library(stringr)
fruits <- c("one apple", "two pears", "three bananas")
str_replace(fruits, "[aeiou]", "-") # "-ne apple"     "tw- pears"     "thr-e bananas"
str_replace_all(fruits, "[aeiou]", "-") # "-n- -ppl-"     "tw- p--rs"     "thr-- b-n-n-s"
str_replace(fruits, "([aeiou])", "") # "ne apple"     "tw pears"     "thre bananas"
str_replace(fruits, "([aeiou])", "\\1\\1") # "oone apple"     "twoo pears"     "threee bananas"
str_replace(fruits, "[aeiou]", c("1", "2", "3")) # "1ne apple"     "tw2 pears"     "thr3e bananas"
str_replace(fruits, c("a", "e", "i"), "-") # "one -pple"     "two p-ars"     "three bananas"

# str_detect
library(stringr)
fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "a") # TRUE TRUE TRUE TRUE
str_detect(fruit, "^a") #  TRUE FALSE FALSE FALSE
str_detect(fruit, "a$") # FALSE  TRUE FALSE FALSE
str_detect(fruit, "b") # FALSE  TRUE FALSE FALSE
str_detect(fruit, "[aeiou]") # TRUE TRUE TRUE TRUE

# str_locate
library(stringr)

# str_extract
library(stringr)
shopping_list <- c("apples x4", "flour", "sugar", "milk x2")
str_extract(shopping_list, "\\d") # "4" NA  NA  "2"
str_extract(shopping_list, "[a-z]+") # "apples" "flour"  "sugar"  "milk"
str_extract(shopping_list, "[a-z]{1,4}") # "appl" "flou" "suga" "milk" # truncate
str_extract(shopping_list, "\\b[a-z]{1,4}\\b") # NA     NA     NA     "milk"

# str_split
library(stringr)
str_split_fixed(colnames(counts),"_", number_of_pieces_to_return) # returns a dataframe
str_split(colnames(counts),"_") # returns a list of vectors

# str_sub
library(stringr)
hw <- "Hadley Wickham"
str_sub(hw, 1, 6) # "Hadley"
str_sub(hw, end = 6) # "Hadley"
str_sub(hw, 8, 14) # "Wickham"

# check conditions
stopifnot(1 == 1, all.equal(pi, 3.14159265), 1 < 2)
stopifnot(is.data.frame(df))

# create color grid tiles
image(some_matrix)

# change default prompt: http://stackoverflow.com/questions/1448600/change-default-prompt-and-output-line-prefix-in-r
options(prompt=" ", continue=" ")

# hide an axis title name
labs=(x="")
labs=(y="") # used to be theme(axis.title.y = element_blank())

# ggplot main title
labs(title="This plot is blah blah", x="this is x axis", y="this is y axis", color="Gene symbol") # also accepts a list, use color fill for legend title
ggtitle("this is title")
ylab("this is y axis")
xlab("this is x axis")

# specify ncol nrow in facet wrap - facet grid doesn't have nrow ncol, use .~var for 1 col, or var~. for one row
facet_wrap(~virus, nrow = 1)

# geom_histogram
my_ggplot(mnc9, aes(x = normalized_counts)) + facet_wrap(~ timepoint, ncol = 1) + geom_histogram(binwidth=.15) +
        scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))


# separation between facet_grid panels
theme(panel.margin = unit(2, "lines"))

# cbind/rbind the elements of a list (do.call expects a list)
a <- list(a1 = data.frame(abc = c(1,2,3), ccc = c(3,3,4)), a2 = data.frame(abc = c(3,5,7), d = c(1,3,2)))
do.call(cbind,a)
    a1.abc a1.ccc a2.abc a2.d
1      1      3      3    1
2      2      3      5    3
3      3      4      7    2

# rename single column
names(d)[3] <- "three"
names(d)[names(d)=="beta"] <- "two"

# change ggplot color scales
+ scale_fill_manual(values = c("steelblue","red")) # To use for fills
+ scale_colour_manual(values = c("steelblue","red")) # To use for line and point colors

# play a sound when a script is done beep
sleep(2); alarm();alarm();alarm();alarm();alarm();alarm()
sleep(2); cat("\a")
# overwrite
cat("Saving..."); sleep(2); cat("\rSaved\033[K\n")

# sort character by numeric
molten$Timepoint <- c("day_1", "day_2", "day_10")
key <- as.numeric(sub("^.+_(.+)", "\\1", molten$Timepoint))
molten$Timepoint <- factor(molten$Timepoint, levels = unique(molten$Timepoint[order(key)]))

# convert factor to integer
as.numeric(levels(f))[f] # better
as.numeric(as.character(fac))[fac]
as.integer(factor(groups, levels = unique(groups))) # to get seq_along the levels starting on 1

# hierarchical clustering of rows
clustering <- hclust(dist(data), method="ward")
data[clustering$order,] # to reorder
plot(clustering, hang=-1, main="Clustering Editors by Common Features", sub="", xlab="")

# cut the tree into k clusters
object$cluster <- cutree(clustering, k = k)

# r palettes
rainbow(n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, alpha = 1)
heat.colors(n, alpha = 1)
terrain.colors(n, alpha = 1)
topo.colors(n, alpha = 1)
cm.colors(n, alpha = 1)

# parallel maximum minimum
pmax(c(10,2,40,3),c(2,4,30,20)) # 10  4 40 20

# facet by cluster using k-medoids with a trend similarity metric. The problem with k-medoids is that the clusters tend to be noisy (try hclust+cutree)
distances <- get_distances(unnormalized) # scale and diff
cl <- clara(distances, num_clusters)
distances$cluster <- factor(cl$cluster)
distances$GeneName <- rownames(distances)
molten <- melt(distances, id = c("GeneName", "cluster"))
colnames(molten) <- c("GeneName","cluster","comparison", "distance")
colnames(molten_fit_values)[colnames(molten_fit_values)=="variable"] <- "timepoints"
colnames(molten_fit_values)[colnames(molten_fit_values)=="value"] <- "m_values"


pcp_cl <- ggplot(molten, aes(comparison, distance, group = GeneName, color = cluster))
pcp_cl + geom_line(alpha = .75) + facet_wrap(~ cluster)

# cpm
cpm <- cpm(df$dge)
cpm <- data.frame(t(t(1e6*df$rc)/(df$dge$samples$lib.size*df$dge$samples$norm.factors))) # divide each column by its library size

# color leaves hclust dendrogram
http://stackoverflow.com/questions/8402216/highlight-and-color-a-specific-node-on-a-dendrogram-plotted-on-a-heatmap

# ggplot heatmap
p <- ggplot(data.melt, aes(x = Feature, y = Editor, fill = Present, color="black", group = Price))
p + geom_tile() + scale_fill_manual(values = my.cols) + scale_x_discrete(labels = feature.labels) +
    theme(axis.text.x = element_text(hjust = 1, angle = 90),
        axis.text.y = element_text(hjust = 1)) + scale_colour_discrete(guide="none") + theme(legend.position = "top")


# make footnote

makeFootnote <- function(footnoteText = format(Sys.time(), "%d %b %Y"), size= .7, color= grey(.5)) {
    require(grid)
    pushViewport(viewport())
    grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just = c("right", "bottom"),
             gp = gpar(cex= size, col = color))
    popViewport()
}

# to take the mean of a one row data frame
mean(as.numeric(data[1,]))
rowMeans(data[1,])

# combination operator
combn(c(1,2,3), 2) # pairwise combinations of 1, 2 and 3

# get the matrix index column row of the minimum maximum element
arrayInd(which.max(mat), dim(mat))
which(F == max(F), arr.ind = TRUE)



# remove margin padding in a ggplot: expand = c(multiplicative_factor, additive_factor), use waiver() if you want to leave it the same
scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))
scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0))
scale_x_discrete(expand = c(0.05, 0)) + scale_y_discrete(expand = c(0.05, 0)) # or just reduce it

# don't draw outlines in boxplot
boxplot(x, outline = F)

# find out where packages are installed
.Library
# /Library/Frameworks/R.framework/Resources/library

# Find out where a package stores its files
system.file('resources/markdown.html',package='markdown') # /Users/nacho/Library/R/2.15/library/markdown/resources/markdown.html

# see loaded libraries
search()

# unload a library package
detach("package:edgeR", unload = T)
detach("package:pheatmap", unload = T); install.packages("/Users/nacho/Documents/Code/Rprojects/pheatmap 3", repos = NULL, type="source"); library(pheatmap)
detach("package:limma2", unload = T); install.packages("/Users/nacho/Documents/Code/Rprojects/limma2", repos = NULL, type="source"); library(limma2)
detach("package:Boruta", unload = T); install.packages("/Users/nacho/Documents/Code/Rprojects/Boruta", repos = NULL, type="source"); library(Boruta)

# load library string
library("df2json", character.only = TRUE)

# edit the source code for a currently loaded function/object in the fly (edit it, add browser() for example, close the file, and call the function)
fix(my_func)

# go into error mode
options(error = recover)
options(error = NULL) # to unset

# omit every line with an NA
na.omit(df)

# kohonen som self organizing
library("kohonen")
data("wines")
wines.sc <- scale(wines)
wine.som <- som(data = wines.sc, grid = somgrid(5, 4, "hexagonal"))
plot(wine.som, main = "Wine data")

# append a list to a list
ranges <- list()
ranges[[length(ranges) + 1]] <- list(lower_bound, upper_bound)

# break up a vector into pieces/slices/chunks, intervals
cut(data$age, breaks = 3)
split(vec, ceiling(seq_along(vec)/size_chunks)) # returns list

# split(x, f, drop = F,...) - divide de data in x into groups in f, you can also unsplit
# drop indicates if unused levels should be dropped
data <- data.frame(city = c("A", "A", "B", "B"), street = c("a", "b", "a", "b"), tenant = c("Smith","Jones","Smith","Jones"), income = c(100,200,300,400))
split_by_city <- split(data, f = data$city) # data gets split row-wise by each level in city
# $A
#   city street tenant income
# 1    A      a  Smith    100
# 2    A      b  Jones    200

# $B
#   city street tenant income
# 3    B      a  Smith    300
# 4    B      b  Jones    400
split_by_street <- split(data, f = data$street) # data gets split row-wise by each level in street
# $a
#   city street tenant income
# 1    A      a  Smith    100
# 3    B      a  Smith    300

# $b
#   city street tenant income
# 2    A      b  Jones    200
# 4    B      b  Jones    400

# rapply(obj, f, how = c("unlist","replace","list")) - apply functions to lists in different ways - recursive lapply?
l <- list(a = 1:10, b = 11:20)
rapply(l, log2, how="list")
# $a
#  [1] 0.00000 1.00000 1.58496 2.00000 2.32193 2.58496 2.80735 3.00000 3.16993 3.32193
# $b
#  [1] 3.45943 3.58496 3.70044 3.80735 3.90689 4.00000 4.08746 4.16993 4.24793 4.32193
rapply(l, log2, how="unlist")
#      a1      a2      a3      a4      a5      a6      a7      a8      a9     a10      b1      b2      b3      b4      b5      b6
# 0.00000 1.00000 1.58496 2.00000 2.32193 2.58496 2.80735 3.00000 3.16993 3.32193 3.45943 3.58496 3.70044 3.80735 3.90689 4.00000
#      b7      b8      b9     b10
# 4.08746 4.16993 4.24793 4.32193
rapply(l, mean, how="unlist")
#   a    b
# 5.5 15.5
rapply(l, mean, how = "list")
# $a
# [1] 5.5

# $b
# [1] 15.5

# seq_along
seq_along(c(20,3,34)) # 1 2 3

# subset using all any
distances[apply(distances,1, function(x) all(is.na(x))),] # the whole row is NA

# get duplicate elements in an array (frequency)
x[duplicated(x, fromLast = T)]

# keep track of index at sapply
sapply(seq_along(y), function(x){sum(y[x:length(y)])})

# z-value z-score.
# Given a sample mean of x, we want to find the probability of getting a sample mean with a greater/lower z-score.
# we obtain a z-score by assuming H_0 is true: the population mean == sample mean
# a z-score of +1.5 is interpreted as being +1.5 standard deviations away from the mean
z <- (x-mean(x))/sd(x) # unscaled
z <- (x-mean(x))/sd(x)/sqrt(length(x)) # scaled
# to get the p-value
2*pnorm(-abs(z)) # two-tailed

# convert a data frame to numeric vector
as.numeric(as.matrix(df))

# reshape splitting columns by name (melt first, then split)
trial <- data.frame(id = factor(1:4), A1 = c(1, 2, 1, 2), A2 = c(2, 1, 2, 1), B1 = c(3, 3, 3, 3))
trialm <- melt(trial)
trialm <- cbind(trialm, colsplit(colnames(trial), pattern="", names = c("treatment", "time"))) # don't do colsplit(trialm$variable, ) because it's very slow, R just repeats the smaller df on cbind
# id variable value treatment time
# 1 1 A1 1 A 1
# 2 2 A1 2 A 1
# 3 3 A1 1 A 1
# 4 4 A1 2 A 1
# 5 1 A2 2 A 2
# 6 2 A2 1 A 2

# Convert a numeric vector to a factor
cut(rnorm(100),3,c("Low","Med","High"))

# get last value (useful after a slow call when you forgot to save output)
.Last.value

# Compute the lengths and values of runs of equal values in a vector
x <- rev(rep(6:10, 1:5)) # 10 10 10 10 10  9  9  9  9  8  8  8  7  7  6
rle(x)
## lengths [1:5]  5 4 3 2 1
## values  [1:5] 10 9 8 7 6

# reverse order rows reverse rows
m[nrow(m):1, ] # use rev_rows(m)

# winsorize data (replace any value above the 95th percentile with the 95th percentile, and same with values below the 5th percentile)
pmax(pmin(x, quantile(x, .95)), quantile(x, .05))

# open a new plot window
quartz()
X11()

# instead of having two related objects, use attributes
a <- c(4,20,40)
attr(a, "max") <- 100
attr(a, "min") <- 0 # or use attributes:
attributes(a) <- list(min = 0,max = 100) # you can create this object directly with: structure(c(4, 20, 40), max = 100, min = 0)

attr(a, "min") # 0
attributes(a) #
# $max
#  [1] 100
# $min
#  [1] 0

# update packages
update.packages() # asks which ones to install
update.packages(ask = FALSE) # doesn't ask
update.packages("knitr") # DOESN'T WORK

# call a function in sapply passing additional arguments
sapply(thresholds, roc.utils.perfs, controls = controls, cases = cases, direction = direction) # same as sapply(thresholds, function(x) roc.utils.perfs(x, controls = controls, cases = cases, direction = direction))

# ROCR, but use pROC because it's cooler, calculate area under the curve auc
library(pracma)
trapz(dens$x, dens$y) # 1.00058
library(ROCR)
clustering <- hclust(dist(t(m_b_30)), method="ward")
cluster <- cutree(clustering, k = 2)
pred <- prediction(cluster,labels);
perf <- performance(pred, "sens", "spec")
plot(perf, col = 2, add = T, lwd = 3)

# multiple levels roc
library(pROC)
multiclass.roc(c(1,3,3,2),c(1,2,3,2))

# split a dataset 80/20
index <- sample(2, nrow(iris), replace = TRUE, prob = c(0.8, 0.2)) # iris[ind==1,] and iris[ind==2,]

# compare predicted vs observed
table(observed = test_labels, predicted = res)
#         predicted
# observed  1  2  3
#        1 12  3  0
#        2  7 12  7
#        3  0  5  2

# create an empty list of size n
my.list <- vector('list', n)

# linear model
lin.mod<-lm(z~i,data = data.frame(z = log10(z),i = index))
z.pred<-predict(lin.mod,data.frame(i = index))
R.coeff<-cor(log10(z),z.pred,method='pearson')

# loess
y.loess <- loess(y ~ x, data = data.frame(y = d$length, x = 1:nrow(d)))
y.pred <- predict(y.loess, 1:nrow(d))
plot(1:nrow(d),y.pred)

# do something with a given probability
if (runif(1)<p1){...} # using the uniform distribution
if (sample(c(1,0), 1, prob = c(p1,1-p1)) == 1) {...} # choose 1 with probability p1


# print with no quotes
write(df,"")
print(df, quote = FALSE)



DT = data.table(a = LETTERS[c(1,1:3)],b = 4:7,key="a")
DT[,c:=8] # add a numeric column, 8 for all rows
DT[,d:=9L] # add an integer column, 9L for all rows
DT[,c:=NULL] # remove column c
DT[2,d:=10L] # subassign by reference to column d

DT[b>4,b:=d*2L] # subassign to b using d, where b>4
DT["A",b:=0L] # binary search for group "A" and set column b
DT[,e:=mean(d),by = a] # add new column by group by reference
DT["B",f:=mean(d)] # subassign to new column, NA initialized

# data.table
dt <- data.table(y = c(1,3,6), v = 1:9)

# see created data.tables
tables()

DT[2] # 2nd row

DT[,v] # v column (as vector)

DT[,J(v)] # v column (as data.table)
DT[,list(v)] # v column (as data.table)

DT[2:3,sum(v)] # sum(v) over rows 2 and 3
DT[2:5,cat(v,"\n")] # just for j’s side effect ??

DT[c(FALSE,TRUE)] # even rows (usual recycling)

DT[,2,with = FALSE] # 2nd column

setkey(DT,x) # set a 1-column key. No quotes, for convenience.
setkeyv(DT,"x") # same (v in setkeyv stands for vector)
v="x"
setkeyv(DT,v) # same
# key(DT)<-"x" # copies whole table, please use set* functions instead
DT["a"] # binary search (fast)
DT[x=="a"] # vector scan (slow)
DT[,sum(v),by = x] # keyed by
DT[,sum(v),by = key(DT)] # same
DT[,sum(v),by = y] # ad hoc by
DT["a",sum(v)] # j for one group
DT[c("a","b"),sum(v)] # j for two groups
X = data.table(c("b","c"),foo = c(4,2))
X
DT[X] # join
DT[X,sum(v)] # join and eval j for each row in i
DT[X,mult="first"] # first row of each group
DT[X,mult="last"] # last row of each group
DT[X,sum(v)*foo] # join inherited scope
setkey(DT,x,y) # 2-column key
setkeyv(DT,c("x","y")) # same
DT["a"] # join to 1st column of key
DT[J("a")] # same. J() stands for Join, an alias for list()
DT[list("a")] # same
DT[.("a")] # same. In the style of package plyr.
DT[J("a",3)] # join to 2 columns
DT[.("a",3)] # same
DT[J("a",3:6)] # join 4 rows (2 missing)
DT[J("a",3:6),nomatch = 0] # remove missing
DT[J("a",3:6),roll = TRUE] # rolling join (locf)
DT[,sum(v),by = list(y%%2)] # by expression
DT[,.SD[2],by = x] # 2nd row of each group
DT[,tail(.SD,2),by = x] # last 2 rows of each group
DT[,lapply(.SD,sum),by = x] # apply through columns by group
DT[,list(MySum = sum(v),
MyMin = min(v),
MyMax = max(v)),
by = list(x,y%%2)] # by 2 expressions
DT[,sum(v),x][V1<20] # compound query
DT[,sum(v),x][order(-V1)] # ordering results
DT[,z:=42L] # add new column by reference
DT[,z:=NULL] # remove column by reference
DT["a",v:=42L] # subassign v by reference
DT[,m:=mean(v),by = x] # add new column by reference by group
DT[,.SD[which.min(v)],by = x] # nested query by group


# Binomial mass function (not a density because x is a discrete variable)
dbinom(x = num_sucesses, size = num_trials, prob= prob_success_on_each_trial)
# If I flip a coin 3 times, the possible number of head I can get is 0,1,2,3. What are the probabilities for each?
dbinom(0,3,.5) # 0.125
dbinom(1,3,.5) # 0.375
dbinom(2,3,.5) # 0.375
dbinom(3,3,.5) # 0.125

# what are the cumulative probabilities (the probabilities of getting at least 0,1,2,3 heads after 3 trials, distribution function)?
pbinom(0,3,.5) # 0.125
pbinom(1,3,.5) # 0.5
pbinom(2,3,.5) # 0.875
pbinom(3,3,.5) # 1

# apply rows of a data frame without coercing to character vector
library(plyr)
ddply(df, 1, class(a$my_col)) # input can be a matrix, array or data frame


# generate the input data
input_data <- data.frame(muscle_strength = c("lower", "middle", "upper"), num_deaths = c(214, 143, 146), group_size = c(2920, 2919, 2923))
input_data$mortality_rate <- input_data$num_deaths / input_data$group_size # this is a ratio, the sample proportion, not the population probability
input_data$standard_error <- sqrt(input_data$mortality_rate*(1-input_data$mortality_rate)/input_data$group_size)

# visualize it
library(plyr)
viz_data <- ddply(input_data, 1, function(group){
  range_num_deaths <- 0:300
  df <- data.frame(muscle_strength = group$muscle_strength,
             num_deaths = range_num_deaths,
             mortality_rate = range_num_deaths/group$group_size ,
             probability = sapply(range_num_deaths, function(x) dbinom(x, group$group_size, group$mortality_rate)))
  df
})

ggsave("/Users/nacho/Documents/Presentations/2013-04-06 lab meeting/muscle_pvalue.png")

my_ggplot(viz_data, aes(mortality_rate, probability)) +
  geom_area(aes(fill = muscle_strength), alpha=.5) +
  geom_vline(data = input_data, aes(xintercept = mortality_rate, color = muscle_strength), linetype = 2) + xlim(.025,.1) +
  geom_text(data = input_data, aes(c(.079, .045, .054), 2e-3, label = paste0(signif(mortality_rate,2)*100, "%"), color = muscle_strength), size = 8)

# Fit a logistic regression model
model_data <- data.frame(muscle_strength = rep(levels(input_data$muscle_strength), times = 2), outcome = rep(c("survived","died"), each = 3), counts = c(input_data$group_size - input_data$num_deaths, input_data$num_deaths))
model_data$muscle_strength <- relevel(model_data$muscle_strength, ref = "upper")
model <- glm(outcome == "died" ~ muscle_strength, weights = model_data$counts, family = binomial(link = "logit"), data = model_data)

summary(model)


