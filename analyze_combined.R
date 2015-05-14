#!/usr/bin/env Rscript

# analyze_combined strategies: using top implementations and top hotspot sites (percentages)

# all, specify if analysis should include all possible combinations (1), or only those with same percentage of sites and top implementations (0 - default)

source("analyze_tradeoff_alternatives.r")

arg <- commandArgs(TRUE)

path_sites_hitcount <- arg[1]
path_freq_alt <- arg[2]
path_data <- arg[3]
path_implem <- arg[4]
all <- arg[5]


analyze_combined_strategies = function(path_sites_hitcount, path_freq_alt, path_data, path_implem, all){
    
    ## check valid paths
    if(!file.exists(path_sites_hitcount)){
        stop("path_sites_hitcount doesn't exist")
    }
    
    if(!file.exists(path_data)){
        stop("path_data doesn't exist")
    }
    
    if(!file.exists(path_freq_alt)){
        stop("path_freq_alt doesn't exist")
    }
    
    if(all!=1)
    all=0;
    
    setwd(path_data)
    wd = path_data
    
    # as.is=c(2) do not factor values in second column
    sites_data = read.csv(path_sites_hitcount, as.is=c(2))
    
    ## 1)Option: Select sites based on higher execution count
    sorted_sites <- sites_data[order(sites_data$clover, decreasing=TRUE),]
    
    # filter sites to those  with implementations only:
    data <- read.csv(path_implem, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    # 1) get indices of sites which have implementations in dataset
    ind <- which(sorted_sites[,1] %in% data[,2])
    # 2) update hitcount data, only consider sites with hitcount data
    sorted_sites <- sorted_sites[ind,]
    
    #iterate over percentage of sites and implementations to consider
    
    if(all==0){
        cat("[simple analysis] same proportion fo sites and top implementations\n")
        per = 0.1
        while(per<=1){
            limit <- round(per*nrow(sorted_sites))
            cat(paste("Top ",per*100,"% ","Sites:",sep=""))
            sites <- sorted_sites[1:limit,1]
            
            #cat("\n") #cat(sites) #cat(" | ") #cat(format(length(sites)),";")
            
            list_results = list()
            # get results for given percentage of sites and same percentage of alternatives in top list (ee)
            list_results = c(list_results, analyze_alternatives(path_freq_alt, path_data, per*100 , sites))
            writeLines(formatUL(list_results, label=""))
            
            per = per + 0.1
        }
        
        cat(paste("Top ","All Sites:",sep=""))
        limit <- round(nrow(sorted_sites))
        sites <- sorted_sites[1:limit,1]
        
        list_results = list()
        list_results = c(list_results, analyze_alternatives(path_freq_alt, path_data, -1 , sites))
        writeLines(formatUL(list_results, label=""))

        
    }else{ #all==1
        #cat("[all combinations analysis] all diff combination of sites and implementations\n")
        
        per = 0.1
        perImpl = 0.1
        #cat("subject, pSites, pAlt, euSav\n") # proportion of sites, proportion of implementations, proportion of Energy usage Savings
        cat("X, 10%, 20%, 30%, 40%, 50%, 60%, 70%, 80%, 90%, 100%\n") # format as matrix of values
        subjName = data$row.names[1]
        if(is.null(subjName))
            subjName = data$subject[1]
        
        while(per<=1){
            limit <- round(per*nrow(sorted_sites))
            sites <- sorted_sites[1:limit,1]
            #cat("\n") #cat(sites) #cat(" | ") #cat(format(length(sites)),";") <- old output format
            
            while(perImpl<=1){
                # get results for given percentage of sites and same percentage of alternatives in top list (ee)
                result = analyze_alternatives(path_freq_alt, path_data, perImpl*100 , sites)
                val = format(as.double(unlist(strsplit(result,split=",", fixed=TRUE))[2]), digits=4)
                
                #strSubj = unlist(strsplit(unlist(strsplit(result,split=",", fixed=TRUE))[1], split="-", fixed=TRUE))[1]
                #subjName = unlist(strsplit(strSubj, split=":", fixed=TRUE))[2]
                #cat(paste(subjName, ", ", per, ", ", perImpl,", ", val, "\n", sep="")) <- old output format
                
                # format as matrix of values (rows: % sites, cols: % alternatives)
                if(perImpl==0.1){
                    line = paste(per*100,"% ", sep="")
                }
                
                if((1-perImpl)>=0.1){
                        line = paste(line, ", ", val, sep="")
                }
                
                if((1-perImpl)<0.01){
                        line = paste(line, ", ", val, "\n", sep="")
                        cat(line)
                }

                perImpl = perImpl + 0.1
            }
            
        
            # results when All implementations are considered (not only the ones in top list)
            result =  analyze_alternatives(path_freq_alt, path_data, -1 , sites)
            val = format(as.double(unlist(strsplit(result,split=",", fixed=TRUE))[2]), digits=4)
            #writeLines(formatUL(list_results, label=""))
            #cat(paste(subjName, ", ", per, ", All, ", val, "\n", sep="")) <- old output format (not printing all altern.)

            perImpl = 0.1 # restart proportion
            per = per + 0.1
        }
        
    }
    
    #cat("\n") #cat(sites) #cat(" | ") #cat(format(length(sites)),";")
    
}

# call function
analyze_combined_strategies(path_sites_hitcount, path_freq_alt, path_data, path_implem, all)


