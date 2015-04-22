# THIS R SCRIPT ANALYZES THE TRADEOFF BETWEEN ENERGY USAGE AND NUMBER OF CONSIDERED ALLOCATION SITES
# FOR A SUBJECT APPLICATION. ALLOCATION SITES CONSIDERED AS HOT SPOTS ARE THE ONLY ONES CONSIDERED
# FOR ANALYSIS.

# @path_sites_hitcount: path to the file containing the hit count (number of times executed) for each allocation site
# @path_freq_alt: path to the file containing the frequencies of selected alternatives
# @path_data: path to the folder containing the energy usage data for the subject app
# @top_par: number of top alternatives implementations to consider in total
# @path_implem: path to the file containing the list of implementations by site in the subject app

analyze_alloc_sites = function(path_sites_hitcount, path_freq_alt, path_data, top_par, path_implem){

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
    
    
	#iterate over percentage of sites to consider
	per = 0.1
	while(per<=1){
        
        limit <- per*nrow(sorted_sites)
		sites <- sorted_sites[1:limit,1]
        #cat("selected sites: ")
        #cat(sites)
        #cat("\n")

		cat(paste("Top ",per*100,"% ","Sites:",sep=""))
        #cat(format(sites),";")
        cat(format(length(sites)),";")

        get_top_alternatives_results(path_freq_alt, path_data, top_par, sites)
		per = per + 0.1
	}

}