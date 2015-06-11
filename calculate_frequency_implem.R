#!/usr/bin/env Rscript

# THIS R SCRIPT CALCULATES THE FREQUENCY OF SELECTION OF THE IMPLEMENTATIONS.
# IMPLEMENTATIONS ARE SELECTED IF THEY HAVE THE HIGHER ENERGY REDUCTION FOR A GIVEN
# SITE IN A SUBJECT APPLICATION.

compute_frequency_implem <- function(){
    
    
    path_data <- c("/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/jfreechart_jcf/results_jfreechart_jcf/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Apachexml/results_totaleu_apache_xml_sec/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Barbecue/results_totaleu_barbecue/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-beanutils/results_totaleu_commons_beanutils/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-cli/results_totaleu_commons_cli/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-lang/results_totaleu_commons_lang/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Gson/results_totaleu_gson/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Jdepend/results_totaleu_jdepend/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Jodatime/results_totaleu_jodatime/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Jtopas/results_totaleu_jtopas/")
    
    list_results = list()
    
    top_par = 100
    
    # environment: hash for implementations
    hash_impl <- new.env()

    count <- 0
    
    for(path in path_data){
        #analyze_alternatives = function(path_freq_alt, path_data, top_par, list_sites){
        
        #load library for statistical analysis (multicomparisons)
        library(pgirmess)
        
        #current directory path
        if(!file.exists(path)){
            cat(paste("path_data doesn't exist", "\n", sep=""))
        }
        
        setwd(path)
        wd = path
        
        flag = TRUE # review all alternative implementations
        
        list_files = list.files(, all.files=FALSE)
        
        list_selAlt = list() # list of selected alternatives
        list_mEUAlt = list() # list of mean energy usage for selected alternatives
        
        # keep track of max savings
        max_sav = 0
        max_sav_name = "NONE"
        altInTop = FALSE
        
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
                        
                        #get name of alternative
                        str <- unlist(strsplit(alt, split="alternatives", fixed=TRUE))[2]
                        index <- as.integer(unlist(gregexpr(pattern="-", str))[2])
                        str <- substring(str,index+1,nchar(str))
                        alt <- unlist(strsplit(str, split="- original", fixed=TRUE))[1]

                        #add selected alternatives to the list
                        list_selAlt = c(list_selAlt, alt)
                        
                        #insert into hash of implementations
                        if(is.null(hash_impl[[alt]])){
                            hash_impl[[alt]] <- 1
                        }else{
                            tempCount <- hash_impl[[alt]]
                            hash_impl[[alt]] <- tempCount + 1
                            #cat(paste(alt, ": ", hash_impl[[alt]], "\n", sep=""))

                        }
                        
                        # increase count of implementations being selected
                        count <- count + 1
                    }
                }
            }
        }
        
    } # end for path_data
    
    cat("\n")
    
    #print results:
    for(item in ls(hash_impl)){
        cat(paste(item, ", ", round(hash_impl[[item]]/count,3),"\n", sep=""))
    }
    
    #cat(count)
    
} # end compute_frequency_implem

compute_frequency_implem()

