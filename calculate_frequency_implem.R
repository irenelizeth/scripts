#!/usr/bin/env Rscript

# THIS R SCRIPT CALCULATES THE FREQUENCY OF SELECTION OF THE IMPLEMENTATIONS.
# IMPLEMENTATIONS ARE SELECTED IF THEY HAVE THE HIGHER ENERGY REDUCTION FOR A GIVEN
# SITE IN A SUBJECT APPLICATION.


get_name <- function(str){
    str <- unlist(strsplit(str, split="alternatives", fixed=TRUE))[2]
    index <- as.integer(unlist(gregexpr(pattern="-", str))[2])
    str <- substring(str,index+1,nchar(str))
    return(str)
}

changed_implementation <- function(str_impl, new_impl){
    
    elem <- unlist(strsplit(str_impl,split=","))
    index <- length(elem)
    str <- sub("\n","",elem[index])
    original_impl <- gsub("\\.","\\-",sub("\n","",str))
}

# get the data set that contains the original implementations per site for the
# corresponding subject app
get_dataset_origImpls <- function(subject){
    
    #list of files with original implementations per site:
    file <- switch(subject,barbecue="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/barbecue-hitcount.sites.csv", commons_lang="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/commons-lang-hitcount.sites.csv", jdepend="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/jdepend-hitcount.sites.csv", jfreechart="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/jfreechart-hitcount.sites.csv", jodatime="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/jodatime-hitcount.sites.csv", xml_security="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/xml-security-hitcount.sites.csv", gson="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/gson-hitcount.sites.csv", commons_beanutils="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/commons-beanutils-hitcount.sites.csv", commons_cli="/Users/irene/Documents/GreenProject/scripts/SITES_SUBJECTS/commons-cli-hitcount.sites.csv")
    
    data_res = read.csv(file, stringsAsFactors=FALSE)
    #return(data_res$TS.calls)
    return(data_res)
}

#get clean name of implementation
get_name_impl <- function(str){
    
    elem <- unlist(strsplit(str,split=","))
    index <- length(elem)
    str <- sub("\n","",elem[index])
    original_impl <- gsub("\\.","\\-",sub("\n","",str))
    return(original_impl)
    
}

compute_frequency_implem <- function(){
    
    
    path_data <- c("/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/jfreechart_jcf/results_totaleu_jfreechart/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Apachexml/results_totaleu_xml_security/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Barbecue/results_totaleu_barbecue/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-beanutils/results_totaleu_commons_beanutils/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-cli/results_totaleu_commons_cli/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Commons-lang/results_totaleu_commons_lang/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Gson/results_totaleu_gson/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Jdepend/results_totaleu_jdepend/",
    "/Users/irene/Documents/GreenProject/scripts/DATA_LEAP_SUBJECTS/Jodatime/results_totaleu_jodatime/")
    
    
    list_results = list()
    
    top_par = 100
    
    # environment: hash for implementations
    hash_impl <- new.env()
    
    # list of implementations
    list_impl <- list()
    # count of changes to impl in JCF (Java Collection Framework)
    count_jcf <- 0
    count_other <- 0
    count_sites <- 0
    # counter for implementations being selected
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
        
        #get subject's name
        subject <- sub("\\/","",unlist(strsplit(path,split="totaleu_"))[2])
        data_oi <- get_dataset_origImpls(subject)
        cat(paste("==== new path for: ", subject, "====\n\n", sep=""))
        
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
            
            #get site number
            siteNum <- as.integer(unlist(strsplit(sd,"site"))[2])
            
            # get the index of the corresponding site number
            indexSite  <- which(data_oi$Allocation.Site==siteNum)
            
            #cat(paste(sd,"\n","site: ",sep=""))
            #cat(paste(sd,"\n","site: ",siteNum," -- > index: ", indexSite, "\n", sep=""))
            # get name of implementation in site before change
            name_original <- get_name_impl(data_oi$TS.calls[indexSite])
            #cat(paste(name_original,"\n",sep=""))
            
            test_file = paste(wd,sd,"/","kw-mc.csv",sep="")
            
            data_file = list.files(sd, pattern ='combinedFile*', all.files=FALSE)
            file = paste(wd, sd,"/",data_file,sep="")
            #complete data set (All)
            data_res = read.csv(file, stringsAsFactors=FALSE)
            #data set with only jcf
            #data_res <- subset(data_res, grepl("java-util",data_res$alternative))
            
            #update list of implementations:
            list_impl <- unique(unlist(c(list_impl, unique(unlist(lapply(data_res$alternative,get_name))))))
            
            #obtain possible change (implementation with mininum energy usage )
            str_impl <- data_res[data_res$eu==min(data_res$eu),]$alternative
            alt_impl <- get_name(str_impl)
            #how many changes to implementation in JCF:
            str1 <- unlist(strsplit(alt_impl, split="java-util", fixed=TRUE))[2]
            if(!is.na(str1)){
                #cat("jcf impl: ", alt_impl, "\n", sep="")
                temp = unlist(strsplit(name_original, split="-"))
                index = length(temp)
                ori_impl = temp[index]
                
                    # original impl is generic type, if it is not contained in alt then it is change (approximation)
                    if(!grepl(ori_impl, alt_impl)){
                        count_jcf = count_jcf + 1
                        #cat(paste("original: ",ori_impl, "/ alternative: ",alt_impl, "\n", sep=""))
                    }
                    
                }else{
                #cat("other impl: ", alt_impl, "\n", sep="")
                count_other <- count_other + 1
                
            }
            
            count_sites <- count_sites + 1
            
            # only analyze sites with significant differences
            if(file.exists(test_file)){
                
                #test_file = paste(wd,"/",path_file,sep="")
                test_res = read.csv(test_file, row.names=1)
                
                # analyze which alternatives have a significant different energy usage from original
                # obtain name of pairs comparing alternative and original energy usage only:
                set_pairs = grep("original", row.names(test_res)) # retrieve row numbers of matching rows
                
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
                        #str <- unlist(strsplit(alt, split="alternatives", fixed=TRUE))[2]
                        #index <- as.integer(unlist(gregexpr(pattern="-", str))[2])
                        #str <- substring(str,index+1,nchar(str))
                        str <- get_name(alt)
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
    #for(item in ls(hash_impl)){
    #    cat(paste(item, ", ", round(hash_impl[[item]]/count,3),"\n", sep=""))
    #}
    
    #cat(count)
    cat(paste("total # implementations,  ", length(list_impl)-1, "\n\n", sep=""))
    #writeLines(formatUL(list_impl, label=""))
    cat(paste("total changes to JCF impl, ", count_jcf, "\n", sep=""))
    cat(paste("total changes to other impl, ", count_other, "\n", sep=""))
    cat(paste("total No. sites, ", count_sites, "\n", sep=""))
    
    
    
} # end compute_frequency_implem




compute_frequency_implem()

