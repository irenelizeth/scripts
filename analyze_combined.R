# analyze_combined strategies: using top implementations and top hotspot sites (percentages)

analyze_combined_strategies = function(path_sites_hitcount, path_freq_alt, path_data, path_implem){
    
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
    
    setwd(path_data)
    wd = path_data
    
    # as.is=c(2) do not factor values in second column
    sites_data = read.csv(path_sites_hitcount, as.is=c(2))
    
    ## 1)Option: Select sites based on higher execution count
    sorted_sites <- sites_data[order(sites_data$clover, decreasing=TRUE),]
    
    # filter sites to those  with implementations only
    data <- read.csv(path_implem, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    # get indices of sites which have implementations in dataset
    ind <- which(sorted_sites[,1] %in% data[,2])
    # update hitcount data, only consider sites with hitcount data
    sorted_sites <- sorted_sites[ind,]
    print(sorted_sites[,1])
    
    #iterate over percentage of sites to consider
    per = 0.1
    while(per<=1.1){
        
        if(per>=1){
            limit <- floor(nrow(sorted_sites))
            cat(paste("Top ","All Sites:",sep=""))
        }else{
            limit <- floor(per*nrow(sorted_sites))
            cat(paste("Top ",per*100,"% ","Sites:",sep=""))
        }
        
        sites <- sorted_sites[1:limit,1]
        cat("\n")
        cat(sites)
        cat(" | ")
        cat(format(length(sites)),";")
        
        list_results = list()
        list_results = c(list_results, analyze_alternatives(path_freq_alt, path_data, per*100 , sites))
        writeLines(formatUL(list_results, label=""))
        
        per = per + 0.1
    }
    
}

