#!/usr/bin/env Rscript

# script to merge all data (energy,tsc) in one single file to then do correlation analysis
# on output file

args <- commandArgs(TRUE)

file_energy <- args[1]
file_tscjcf <- args[2]
file_tsco <- args[3]
repetitions <- as.integer(args[4])

#file1="DATA_ALL/data_barbecue.csv"
file1 = file_energy

df = read.csv(file1, header=TRUE, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="")
#change columns order
df <- df[,c(2,1)] #change order of columns

#file2 = "DATA_ALL/data_tsc_barbecue.txt"
file2 <- file_tscjcf
data <- read.csv(file2, header=TRUE, stringsAsFactors=FALSE)
# get index of run column
indexs = grep("run", colnames(data))+1
indexe = length(colnames(data))
data <- data[,c(indexs:indexe)] #remove 'run' column

alternative <- data$subject
tsc <- data$stop-data$start
data_tsc <- data.frame(alternative, tsc, stringsAsFactors=FALSE)

if(file.exists(file_tsco)){
    #file3 = "DATA_ALL/data_tsc_barbecue_others.txt"
    file3 <- file_tsco
    data <- read.csv(file3, header=TRUE, stringsAsFactors=FALSE)
    indexs = grep("run", colnames(data))+1
    indexe = length(colnames(data))
    data <- data[,c(indexs:indexe)] #remove 'run' column
    
    alternative <- data$subject
    tsc <- data$stop-data$start
    data_tsc2 <- data.frame(alternative, tsc, stringsAsFactors=FALSE)
    
    #append df_tsc and df_tsc2
    df_tsc <- rbind(data_tsc, data_tsc2)
}else
    df_tsc <- data_tsc

#first sort data so duplicates values be continous
df_tsc <- df_tsc[with(df_tsc,order(alternative)),]

#df <- df[order(alternative),]
df <- df[with(df,order(alternative)),]

#combine energy and tsc data in one file:
#merge produces all kind of combinations of data rows from two data.frames due duplicate values in alternatives

merged_data <- data.frame()
rep = repetitions

seqIter <- seq(from=2, to=nrow(df), by=rep)
for(k in seqIter){
    alt <- df$alternative[k]
    sub <- df[df$alternative==alt,]
    #cat(paste(alt, "\n",sep=""))
    if(nrow(sub)>0){
        tsc <- df_tsc[df_tsc$alternative==alt,]$tsc
        #if different rows keep the smaller number
        nr <- min(nrow(sub),nrow(tsc))
        sub <- sub[1:nr,]
        tsc <- tsc[1:nr]
        #combine energy and tsc data
        subset <- cbind(sub,tsc)
        #combine to whole set
        merged_data <- rbind(merged_data,subset)
    }
}

write.csv(merged_data,file=paste(file1,"_all.csv",sep=""))
cat("[done] creating data file with all results\n")


