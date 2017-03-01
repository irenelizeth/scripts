#!/Library/Frameworks/R.framework/Resources/bin/Rscript 

#Irene Manotas
#2017-03-01
#Procesing SEEDS Log files for single-change solutions

suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(stringr))


# usage: ./analyze_seeds_ss_results.r -d 'clean_bb_seeds17_single_best_JCF_10R_cl/' -n barbecue -t 10 -f '../barbecue-1.0.0mappingAlt_jcf.csv' -o 'solutions_bb_jcf.csv'


option_list= list(
        make_option(c("-d","--dir" ), action="store", default=NA, type='character',
             help="directory where SEEDS log files are located for an app subject"),
        make_option(c("-n","--name"), action="store", default=NA, type='character',
             help="name of subject application for which the experiments were run"),
        make_option(c("-t", "--numtrials"), action="store", default=NA, type='integer',
             help="number of trials the experiment was run (# solutions)"),
        make_option(c("-f", "--mapfile"), action="store", default=NA, type='character',
            help="path to file containing mapping of alternatives to IDs for subject app"),
        make_option(c("-o", "--outfile"), action="store", default=NA, type='character',
            help="path to file containing solutions details")
)

opt = parse_args(OptionParser(option_list=option_list))


library(stringr)
# read data frame with best solutions selected in each run
#data <- read.csv(file="", fill=TRUE)

subject <- opt$name
strategy <- "seeds-single"
nruns=opt$numtrials
mapfile <- opt$mapfile
outputfile <- opt$outfile

#TODO: check input values are correct

setwd(opt$dir)

print(paste('Printing solutions for ', subject, 'when ', strategy, ' run for ', nruns, ' times.', sep=''))

dfm <- data.frame(row.names=FALSE)
seqFiles <- seq(1:nruns)

for(x in seqFiles){
    df1a <- read.csv(paste("VAR",x, sep=""),header=FALSE, stringsAsFactors=FALSE, skipNul=TRUE, 	na.strings="")
    df1 <- toString(paste(unlist(strsplit(toString(df1a), split=" ")), sep=","))
    df2 <- read.csv(paste("FUN",x, sep=""),header=FALSE, stringsAsFactors=FALSE, skipNul=TRUE, 	na.strings="")
    df <- cbind(strategy, subject, solution=df1, energy=df2)
    dfm <-rbind(dfm, df)
}
colnames(dfm) <- c("strategy", "subject", "solution", "energy")
# data frame of solutions' energy usage
data <- dfm

#analyze which collection impl. was selected in each solution
solsdf <- data.frame(row.names=FALSE)
for(x in 1:nrow(data)){
    sol <- unlist(strsplit(as.character(data[x,3]),","))
    ans <- str_extract_all(sol, "([:digit:]{2,2})|([2-9]{1,1})")
    for(i in 1:length(ans)){
        if(!(is.character(ans[[i]]) && length(ans[[i]])==0)){
            row <- as.data.frame(t(c((i-1), as.integer(ans[[i]]))))
            names(row) <- names(solsdf)
            solsdf <- rbind(solsdf, row)
            print(paste(x, ': site ', (i-1),'), ', ans[[i]], sep=''))
        }
    }
}
colnames(solsdf) <- c("site", "idAlt")

#read mapping of alternatives to IDs for app site:
#data_map <- read.csv(file='../barbecue-1.0.0mappingAlt_jcf.csv', fill=TRUE, col.names=c("site","idAlt","Alternative"), stringsAsFactors=FALSE)
print(paste('Reading mapping of alternatives, file: ', mapfile, sep=''))
data_map <- read.csv(file=mapfile, fill=TRUE, col.names=c("site","idAlt","Alternative"), stringsAsFactors=FALSE)

#select name of alternative for selected by solution:
getNameAlternative <- function(solId){
    name <- data_map[data_map$site==solsdf[solId,1] & data_map$idAlt==solsdf[solId,2], c('Alternative')]
    return(name)
}
seqSols <- seq(1,nrow(solsdf),1)
alt <- unlist(lapply(seqSols, getNameAlternative))

#merge solutions (single changes) with alternatives used in each solution
df <- cbind(solsdf, alt)
# merge all data about solutions:
all.df <- cbind(data, df)
# write data to csv file:
#file_name <- '../solutions_bb_seeds_jcf_10R_cl.csv'
file_name <- outputfile
write.csv(all.df, file=file_name)