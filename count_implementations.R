#!/usr/bin/env Rscript

# file to reshape data files that contain the list of implementations
# available for each subject app, obtained from bash script

# file: path to data file containing results of impplementations per site
# format input file: <row> : site#,impl1, impl2, impl3,...,impln

count_top_implementations = function(file){

    cat(paste("site, ","total, ","top","\n", sep=""))

    # freqCollections.csv
    #read top alternatives file and create a set of top alternatives
    top_list = read.csv("freqCollections.csv")
    
    if(top_par>0){
        top_par = floor((nrow(top_list))) # take only the percentage of implem. indicated by top_par
        topM <- as.vector(top_list$implementation[c(1:top_par)])
        #cat(paste("\n",per*100," % top implementations: \n", sep=""))
        #cat(paste(topM, "\n", sep=""))
    }

    data = read.csv(file, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="", header=FALSE)

    # analyze how many in top list
    for (item in 1:nrow(data)){
        
        #cat(paste("row ",item, "\n",sep=""))
        
        #site number
        nSite = data[item, 1]
        
        #compute number of implementations in total per site
        nImpl = length(data[item,][!is.na(data[item,])])-1

        list <- list()
        # restructure implementation names from data
        
        if(nImpl>0){
            for(i in 2:(nImpl+1)){
                # get the name of the implementation from the string
                str = unlist(strsplit(data[item,i],split=data[item,1],fixed=TRUE))[2]
                list[i-1] <- substr(str,2,nchar(str)-4)
            }
        }
        

    #cat(paste("######### site ",data[item,1],"\n", sep=""))
        
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
        #print site #, # total implemen. , # implem. top list
        cat(paste(nSite, ", ",nImpl, ", ", count, "\n", sep=""))

    }
    
}

