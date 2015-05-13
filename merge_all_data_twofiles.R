#!/usr/bin/env Rscript

# script to merge all data (energy,tsc) in one single file to then do correlation analysis
# on output file

args <- commandArgs(TRUE)

file_energy <- args[1]
file_tscjcf <- args[2]
file_tsco <- args[3]
repetitions <- as.integer(args[4])
rep2 <- as.integer(args[5]) # repetitions for second file

#file1="DATA_ALL/data_barbecue.csv"
file1 = file_energy

df = read.csv(file1, header=TRUE, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="")
#change columns order
df <- df[,c(2,1)] #change order of columns

#file2 = "DATA_ALL/data_tsc_barbecue.txt"
file2 <- file_tscjcf
data <- read.csv(file2, header=TRUE, stringsAsFactors=FALSE)
data <- data[,c(2:4)] #remove 'run' column

alternative <- data$subject
tsc <- data$stop-data$start
data_tsc <- data.frame(alternative, tsc, stringsAsFactors=FALSE)

#file3 = "DATA_ALL/data_tsc_barbecue_others.txt"
file3 <- file_tsco
data <- read.csv(file3, header=TRUE, stringsAsFactors=FALSE)
data <- data[,c(2:4)] #remove 'run' column

alternative <- data$subject
tsc <- data$stop-data$start
data_tsc2 <- data.frame(alternative, tsc, stringsAsFactors=FALSE)

#append df_tsc and df_tsc2
df_tsc <- rbind(data_tsc, data_tsc2)
#first sort data so duplicates values be continous
#df_tsc <- df_tsc[order(alternative),]
df_tsc <- df_tsc[with(df_tsc,order(alternative)),]

#df <- df[order(alternative),]
df <- df[with(df,order(alternative)),]

#combine energy and tsc data in one file:
#merge produces all kind of combinations of data rows from two data.frames due duplicate values in alternatives

merged_data <- data.frame()
#rep = 10

if(args[5]>0){
    rep = repetitions
    rep2 = as.integer(args[5])
    
    minrep = min(rep,rep2)
    
    seqIter <- seq(from=2, to=nrow(data_tsc), by=rep)
    
    data_tsc <- data_tsc[with(data_tsc,order(alternative)),]
    
    
    for(k in seqIter){
        alt <- data_tsc$alternative[k]
        sub <- df[df$alternative==alt,]
        sub <- sub[1:minrep,]
        if(nrow(sub)>0 & !is.na(sub$eu[1])){
            
            tsc <- df_tsc[df_tsc$alternative==alt,]$tsc
            tsc <- tsc[1:minrep]
            #combine energy and tsc data
            subset <- cbind(sub,tsc)
            #combine to whole se
            merged_data <- rbind(merged_data,subset)
        }
    }
    
    seqIter <- seq(from=2, to=nrow(data_tsc2), by=rep2)
    
    data_tsc2 <- data_tsc2[with(data_tsc2,order(alternative)),]
    
    for(k in seqIter){
        
        alt <- data_tsc2$alternative[k]
        sub <- df[df$alternative==alt,]
        if(nrow(sub)>0 & !is.na(sub$eu[1])){
            sub <- sub[1:minrep,]
            tsc <- data_tsc2[data_tsc2$alternative==alt,]$tsc
            tsc <- tsc[1:minrep]
            #combine energy and tsc data
            subset <- cbind(sub,tsc)
            #combine to whole set
            merged_data <- rbind(merged_data,subset)
        }
    }
    
    
}

write.csv(merged_data,file=paste(file1,"_all.csv",sep=""))

