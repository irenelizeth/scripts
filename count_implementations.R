#!/usr/bin/env Rscript

# file to reshape data files that contain the list of implementations
# available for each subject app, obtained from bash script

# file: path to data file containing results of impplementations per site
# format input file: <row> : site#,impl1, impl2, impl3,...,impln

args <- commandArgs(TRUE)

file = args[1]
top_par = as.integer(args[2])

if(!file.exists(file))
    stop("file doesn't exist")

if(top_par<=0)
    top_par=100

cat(paste("subject, ","site, ","total, ","top","\n", sep=""))

#read top alternatives file and create a set of top alternatives
top_list = read.csv("freqCollections.csv")

    per = top_par/100
    top_par = floor(per*(nrow(top_list))) # take only the percentage of implem. indicated by top_par
    topM <- as.vector(top_list$implementation[c(1:top_par)])


data = read.csv(file, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="", header=FALSE)

# analyze how many in top list
for (item in 1:nrow(data)){
    
    #site number
    nSite = data[item, 1]
    
    #compute number of implementations in total per site
    nImpl = length(data[item,][!is.na(data[item,])])-1
    
    list <- list()
    # restructure implementation names from data
    if(nImpl>0){
        for(i in 2:(nImpl+1)){
            str = unlist(strsplit(data[item,i],split=data[item,1],fixed=TRUE))
            # get subject's name
            subject = unlist(strsplit(str[1],split="alternatives",fixed=TRUE))[1]
            subject_name <- substr(subject,1,nchar(subject)-1)
            # get the name of the implementation from the string
            list[i-1] <- substr(str[2],2,nchar(str[2])-4)
        }
    }
    
    count = 0
    for(i in 1:length(topM)){
        for(j in 1:length(list)){
            res = grepl(topM[i],list[j])
            
            if(isTRUE(res)){
                count=count+1
                #cat(paste(list[j],", ",count,"\n"),sep="")
            }
        }
    }
    
    #res = mapply(grepl, topM, list)
    #nTop = length(which(res==TRUE))
    
    # which top implementations are included?
    nTop = length(which(res==TRUE,)) # return indices of implementations in topM
    cat(paste(subject_name, ",", nSite, ", ",nImpl, ", ", count, "\n", sep=""))
    
}


