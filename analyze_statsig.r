# clean out the workspace
rm(list=ls())

dir_name = "euDATA_jfreechart_jcf109_merge/"

#change to the new directory
setwd(dir_name)

#load libraries
library(pgirmess)

listSignificantDifferentAlternatives = function(){
	#list folder names
	list = list.files(, all.files=FALSE)
	print("LIST OF APP SITES WITH SIGNIFICANT DIFFERENCE IN ENERGY USAGE:")
	# iterate over folders
	for (sd in list){

        	file_name = list.files(sd, pattern ='*.csv', all.files=FALSE)
        	wd = getwd()
		file_name = paste(wd,"/",sd,"/",file_name,sep="")

		#print(file_name)

        	#read CSV file
        	data_name = read.csv(file_name)

        	#analyze p-value with kruskal wallis test
        	kw_data = kruskal.test(eu~alternative, data=data_name)
		#if p-value is low (<0.05) then print name for multi-comparison analysis
		if(kw_data[3]$p.value < 0.05){
			print(paste(sd,": ",kw_data[3]$p.value))
			# do multicomparison analysis on listed site:
			kmc <- kruskalmc(eu~alternative, data=data_name, probs = 0.05)
			#fileMC <- file(paste(sd,"-","kw-mc.csv"))
			print(kmc)
		}

			#close(fileMC)
			#print(kmc)
			#print("\n")
	
	}

}

#kmc <- kruskalmc(eu~alternative, data=data_name, probs = 0.05) # multiple-comparison t

