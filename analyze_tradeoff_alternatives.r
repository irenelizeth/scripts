# THIS R SCRIPT ANALYZES THE TRADEOFF BETWEEN SELECTING THE TOP X ALTERNATIVES
# IMPLEMENTATIONS OF A COLLECTION AND THE ENERGY USAGE OF THE APPLICATION
# WHEN ONE OF THOSE TOP ALTERNATIVES ARE SELECTED. TO FIND THE CORRECT CUTOFF
# POINT, WE ANALYZE SELECTING THE TOP ALTERNATIVES STARTING WITH A SELECTION OF
# 5 ALTERNATIVES AND INCREMENTING THE SELECTION BY 5 ALTERNATIVES EACH TIME.


analyze_alternatives = function(path_freq_alternatives){

#load library for statistical analysis (multicomparisons)
library(pgirmess)

#current directory path
wd = getwd()

# freqCollections.csv
#read top alternatives file and create a set of top alternatives
top_list = read.csv(path_top_alt) # columns: frequency and implementation

list_files = list.files(, all.files=FALSE)
top = 5 # number of top alternatives to consider

topM <- as.vector(top_list$implementation[c(1:top)])

#print("LIST SIGNIFICANT DIFFERENT ALTERNATIVES FROM TOP LIST")

list_selAlt = list() # list of selected alternatives
list_mEUAlt = list() # list of mean energy usage for selected alternatives

# iterate over folders
for (sd in list_files){
	path_file = paste(sd,"/","kw-mc.csv",sep="")
	if(file.exists(path_file)){

		test_file = paste(wd,"/",path_file,sep="")
		test_res = read.csv(test_file, row.names=1)
	
		# analyze which alternatives have a significant different energy usage from original
		# obtain name of pairs comparing alternative and original energy usage only:
		set_pairs = grep("original", row.names(test_res)) # retrieve row numbers of matching rows	
		
		for(IDpair in set_pairs){
			# is this comparison significant? dif.com.difference="TRUE" and statistic="alternative-original"
			if(test_res[IDpair,5]){
				#read energy usage data: (this could be moved before the for loop)
	                	data_file = list.files(sd, pattern ='combinedFile*', all.files=FALSE)
	                	data_res = read.csv(paste(wd,"/",sd,"/",data_file,sep=""))
			
				#get name for this significant alternative:
				alt = row.names(test_res[IDpair,])

				alt_name = substr(alt, 1, nchar(alt)-nchar("- original"))
				#get energy usage mean for this alternative:
				mean_alt = mean(data_res[data_res$alternative==alt_name,1])
				mean_orig = mean(data_res[data_res$alternative==" original",1]) # mean original version of subject application

				# is this alternative in top alternatives list?
				if(any(mapply(grepl, topM, row.names(test_res[IDpair,]), SIMPLIFY=TRUE, USE.NAMES=FALSE))){

					list_selAlt = c(list_selAlt, alt)
                                        list_mEUAlt = c(list_mEUAlt, mean_alt)

				}
	
			}
		}
	}
}

}//end function analyze_alternatives

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
