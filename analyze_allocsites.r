# THIS R SCRIPT ANALYZES THE TRADEOFF BETWEEN ENERGY USAGE AND NUMBER OF CONSIDERED ALLOCATION SITES
# FOR A SUBJECT APPLICATION. ALLOCATION SITES CONSIDERED AS HOT SPOTS ARE THE ONLY ONES CONSIDERED
# FOR ANALYSIS.

# @path_sites_hitcount: path to the file containing the hit count (number of times executed) for each allocation site
# @path_freq_alt: path to the file containing the frequencies of selected alternatives
# @path_data: path to the folder containing the energy usage data for the subject app
# @top_par: number of top alternatives implementations to consider in total

analyze_alloc_sites = function(path_sites_hitcount, path_freq_alt, path_data, top_par){

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
	
	#iterate over percentage of sites to condider
	per = 0.1
	while(per<=1){
		
        limit <- per*nrow(sites_data)
		sites <- sorted_sites[1:limit,1]

		cat(paste("Top ",per*100,"% ","Selected Hot Spot Sites:",sep=""))
        cat(format(sites))

        get_top_alternatives_results(path_freq_alt, path_data, top_par, sites)
		per = per + 0.1
	}

	## 2)Option: Use ESD : extreme studentized deviation - outlier detection rule
    #if(num_sd<0)
    #    num_sd=0
    
    #max_hit_counts = max(sites_data$clover)
	#mean_sites = mean(sites_data$clover)
	#sd_sites = sd(sites_data$clover)
    	#print(paste("sd: ",sd_sites,sep=""))
    	#t = num_sd
    
	##minR = mean_sites - t*sd_sites
	#maxR = mean_sites + t*sd_sites
    	##print(paste("maxR: ",maxR,sep=""))

	##select hot spots based on outlier detection
	#sites <- sites_data[sites_data$clover>=maxR,1]
    
    	#print("Selected Hot Spot Sites:")
    	#print(sites)

	## only analyze directories of hot spot sites:
    	#get_top_alternatives_results(path_freq_alt, path_data, top_par, sites)

}
