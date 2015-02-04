# THIS R SCRIPT ANALYZES THE TRADEOFF BETWEEN SELECTING THE TOP X ALTERNATIVES
# IMPLEMENTATIONS OF A COLLECTION AND THE ENERGY USAGE OF THE APPLICATION
# WHEN ONE OF THOSE TOP ALTERNATIVES ARE SELECTED. TO FIND THE CORRECT CUTOFF
# POINT, WE ANALYZE SELECTING THE TOP ALTERNATIVES STARTING WITH A SELECTION OF
# 5 ALTERNATIVES AND INCREMENTING THE SELECTION BY 5 ALTERNATIVES EACH TIME.

# @path_freq_alt: path to the file containing the frequencies of selected alternatives
# @path_data: path to the folder containing the energy usage data for the subject app
# @top_par: number of top alternatives implementations to consider

analyze_alternatives = function(path_freq_alt, path_data, top_par){
    
    #load library for statistical analysis (multicomparisons)
    library(pgirmess)
    
    #current directory path
    
    if(!file.exists(path_data)){
        stop("path_data doesn't exist")
    }
    
    if(!file.exists(path_freq_alt)){
        stop("path_freq_alt doesn't exist")
    }
    
    if(top_par<=0)
    top_par=5
    else
    top = top_par # number of top alternatives to consider
    
    
    setwd(path_data)
    wd = path_data
    
    
    # freqCollections.csv
    #read top alternatives file and create a set of top alternatives
    top_list = read.csv(path_freq_alt) # columns: frequency and implementation
    
    list_files = list.files(, all.files=FALSE)
    
    topM <- as.vector(top_list$implementation[c(1:top)])
    
    #print("LIST SIGNIFICANT DIFFERENT ALTERNATIVES FROM TOP LIST")
    
    list_selAlt = list() # list of selected alternatives
    list_mEUAlt = list() # list of mean energy usage for selected alternatives
    
    # keep track of max savings
    max_sav = 0
    max_sav_name = ""
    
    # iterate over folders
    for (sd in list_files){
        
        test_file = paste(wd,sd,"/","kw-mc.csv",sep="")
        
        # only analyze sites with significant differences
        if(file.exists(test_file)){
            
            #test_file = paste(wd,"/",path_file,sep="")
            test_res = read.csv(test_file, row.names=1)
            
            # analyze which alternatives have a significant different energy usage from original
            # obtain name of pairs comparing alternative and original energy usage only:
            set_pairs = grep("original", row.names(test_res)) # retrieve row numbers of matching rows
            
            #read energy usage data: (this could be moved before the for loop)
            data_file = list.files(sd, pattern ='combinedFile*', all.files=FALSE)
            file = paste(wd, sd,"/",data_file,sep="")
            data_res = read.csv(file)
            
            mean_orig = mean(data_res[data_res$alternative==" original",1]) # mean original version of subject application
            
            for(IDpair in set_pairs){
                # is this comparison significant? dif.com.difference="TRUE" and statistic="alternative-original"
                if(test_res[IDpair,5]){
                    
                    #get name for this significant alternative:
                    alt = row.names(test_res[IDpair,])
                    
                    alt_name = substr(alt, 1, nchar(alt)-nchar("- original"))
                    #get energy usage mean for this alternative:
                    mean_alt = mean(data_res[data_res$alternative==alt_name,1])
                    
                    # is this alternative in top alternatives list?
                    if(any(mapply(grepl, topM, row.names(test_res[IDpair,]), SIMPLIFY=TRUE, USE.NAMES=FALSE))){
                        
                        # alternative : eu_mean, top_#
                        cat(alt, ": ", mean_alt, ",", top_par, "\n")
                        
                        list_selAlt = c(list_selAlt, alt)
                        list_mEUAlt = c(list_mEUAlt, mean_alt)
                        savings = ((mean_orig - mean_alt)/mean_orig)*100
                        
                        if(savings > max_sav){
                            max_sav = savings
                            max_sav_name = alt
                        }
                    }
                }
            }
        }
    }
    
    cat("\n\nMaximum savings: ",max_sav, "by Alternative: ", max_sav_name)

} # end function analyze_alternatives

# FUNCTION THAT RETURNS TRUE IF THE GIVEN IMPLEMENTATION IS IN THE TOP LIST
# consider using mapply instead of this function as follows:
# if(any(mapply(grepl, topM, row.names(test_res[IDpair,]), SIMPLIFY=TRUE, USE.NAMES=FALSE))) 

isImplementationInTopList = function(list, impl){
	for (item in list){
		if(grepl(item, impl))
			return(TRUE)	
	}	
	return(FALSE)
}
