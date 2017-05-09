# Analysis of Ga generations per Run

# How many times the gGA algorithm was run for the given numGen
file_data <- "data_st.csv"
data_gga <- read.csv(file=file_data, row.names=NULL, fill=TRUE)
data_gga <- data_gga[,c("Group", "Value")]

numRuns <-3 
samplesPerSol <- 15
popSize <- 50
numMaxEval <- 5000
verbose = TRUE
rows_pop <- samplesPerSol*(popSize+1)

# collect data for each run that the GA was run in the experiment
# data_gga <- data_st.csv file with all samples collected while GA experiment was runnning
# numRuns: how many runs in total
# #samplesPerSol: how many samples of energy usage were collected per solution
# popSize: population size
# numMaxEval: number of maximum evaluations allowed in each run of the GA alg.
# verbose: TRUE to print info about function's tasks
# output: in current directory, a file data_run_k.csv will be created for each run's data

collect_data_for_each_runGAAlgorithm <- function(data_gga, numRuns, samplesPerSol, popSize, numMaxEval, verbose){

	numGen <- (numMaxEval/popSize)
	#numGen <- ceiling(((numMaxEval-50)/(popSize-2)))
	# rows per population
	rows_pop <- samplesPerSol*(popSize+1)
	# indices for all populations headers (Value)
	ind_df <- which(data_gga$Group=='Value')
	print(ind_df)
	start_ind <- 1
	end_ind <- 1
	
	if(length(ind_df) >= numGen*numRuns){
	
		for(k in 1:numRuns){
			cat(paste('Data for run', k,"\n", sep=""))
			if(k==1){
				ind_df_run <- ind_df[1:(numGen)]
				start_ind <- 1
				end_ind <- ind_df_run[length(ind_df_run)]+rows_pop
			}
			else if(k > 1 && k < numRuns)	{
				ind_df_run <- ind_df[(numGen*(k-1) +1):(numGen*(k)+1)] 
				start_ind <- ind_df_run[1]+1
				end_ind <- ind_df_run[length(ind_df_run)]+rows_pop
			}else{
				ind_df_run <- ind_df[(numGen*(k-1)+2):length(ind_df)]
				start_ind <- ind_df_run[1]+1
				end_ind <- ind_df_run[length(ind_df_run)]+rows_pop
			}
				
			cat(paste('indices [', start_ind , "-",end_ind,"]\n", sep=""))
			# collect data for all generations in the run (trial)
			df_run <- data_gga[start_ind:end_ind,]
			df_run <- df_run[-(which(df_run$Group=='Value')), ]
			df_run <- droplevels(df_run)
			df_run <- df_run[,c("Group", "Value")]
			num_gen <- nrow(df_run)/rows_pop
	
			if(verbose){
				print(paste("For run ",k, ":", sep=""))
				#print(ind_df_run)
				cat(paste("indices for data_run: [", start_ind, ":", end_ind, "]", sep=""))
				cat("\n")
				cat("Number of generations in data of run: ")
				cat(num_gen)
				cat("\n")
			}
	
			write.csv(file=paste("data_run_",k,".csv", sep=""), df_run, row.names=FALSE)
		}
	}else{
		stop(paste("incomplete data for runs: Only ", length(ind_df)+1, " populations registered \n", sep=""))
	}
} # end function collect_data_for_each_runGAAlgorithm


### START FUNCTION ###

# df_run: data set for a given run of the algorithm (csv file output of collect_data_for_each_runGAAlgorithm)
# rows_pop: total rows expected per population (samplesPerSolution*(num_gen+1))
# num_gen: total number of populations generated in generations including the initial population
# popSize: size of each population (ga parameter)
# best_sol: name of best solution found by algorithm for this run
collect_generations_data <- function(df_run, rows_pop, num_gen, popSize, best_sol){
	
	track_best_sol = "Best Solution Tracking:\n";
	s = 1 
	e = rows_pop
	table_summary <- data.frame(row.names=NULL, stringsAsFactors=FALSE)

	for(pop in 1:num_gen){
		cat(paste("Collect information for Generation: ", pop, "\n", sep=""))

		df_pop <- df_run[s:e,]
		s <- s + rows_pop
		e <- e + rows_pop
		
		# clean up data frame for population:
		df_pop <- droplevels(df_pop)
		
		mean_pop <- aggregate(.~Group, df_pop, mean)
		ind_bs <- which(mean_pop$Group==best_sol,)
		if(length(ind_bs)>0){
			track_best_sol <- cat(paste(track_best_sol, "\n ", "Gen.",pop, ") MEU Best Solution: ", mean_pop[ind_bs,2], sep=""))
		}
		
		# for each generation in the data of run: collect population's information:
		data_pop <- collect_info_significantGroups(df_pop, pop, popSize, TRUE, table_summary)	
		
			cat("Printing best solutions: \n")
			print(data_pop)
			table_summary <- rbind(table_summary, data_pop)

	}
	
	cat(track_best_sol)
	
	return(table_summary)
		
}


########## FUNCTION DEFINITIONS ##########

### START FUNCTION ###

get_significant_groups <- function(diff_groups, samples_groups, diffSamples){
    	pvalue <- 1 # significance tolerance rate
    	alpha <- 0.05 # false discovery rate
    	
    	if(diffSamples)
    		ans <- get_result_statistical_test_different_samples(diff_groups, samples_groups)
    	else
    		ans <- get_result_statistical_test(diff_groups, samples_groups)
    		
		data <- as.data.frame(ans[1])
		pvalue <- as.double(ans[2])
		
		# Returns:
		# sigGroupsPH[1] list of groups
		# sigGroupsPH[2] effect size measure for each group in list sigGroupsPH[1]
		
		return(get_results_multicomparison(data, pvalue))
    }
    
### END FUNCTION ###
	
### START FUNCTION ###

# copute CohensD and Cliff's delta for each group of data in df_samples
compute_CohensCliff_ESM <- function(df_samples){
	data_esm <- data.frame(row.names=FALSE)
	
	data_sg <- aggregate(.~Group, data=df_samples, function(x) unlist((strsplit(as.character(x), split=","))))
	idOri <- which(data_sg$Group=="Original")
	Original <- as.double(unlist(data_sg[idOri,2]))
	data_sg <- data_sg[-idOri,] # remove original samples from data set
	
	for(k in 1:nrow(data_sg)){
      group <- as.character(data_sg[k,1])
      sampleData <- as.double(unlist(data_sg[k,2]))
      listSamples <- paste(sampleData, collapse=" ")
      
      # compute ESM
      d <- cohensD(sampleData, Original)
      ce <- cliff.delta(sampleData, Original)$estimate
	  cm <- as.character(cliff.delta(sampleData, Original)$magnitude)
	  	
	  row <- data.frame(Group=group, CohensD=d, Cliff.estimate=ce, Cliff.magnitude=cm)
	  data_esm <- rbind(data_esm, row)
      
   }
   
   # order in ascending order by cliff values, and in descending order by cohensD values
   data_esm <- data_esm[order(data_esm$Cliff.estimate, -data_esm$CohensD),]
   
   return(data_esm)
}

### END FUNCTION ###


### START FUNCTION ###

#sam_exp: data points for solutions in generation's population
#genID: # of generation
table_summary <- data.frame(row.names=FALSE)

collect_info_significantGroups <- function(sam_exp, genID, popSize, verbose, table_summary){
	
	mean_exp <- aggregate(.~Group, data=sam_exp, mean)

	# find out first which groups are significant different from each other
	samples_groups <- unlist(aggregate(.~Group, data= sam_exp, function(x){paste(x,collapse=",")})$Value)
	#print(samples_groups)
	diff_groups <- unlist(as.vector(sapply(sam_exp, levels)$Group))
	
	# Are there repeated solutions/groups (individuals) in the population
	if(length(diff_groups) < (popSize+1)){
		cat("Printing duplicated solutions in population: \n")
		# get frequency of solutions in population
		count <- 0

		for(group in (diff_groups)){ 
        	rep <- nrow(sam_exp[sam_exp$Group==group,])/15 
        	if(rep>1){
             	count <- count + 1
             	cat(paste(count,") ",group," | ",rep,"\n",sep=""))
           	}
 		}
 		
 		#print(diff_groups)
		sigGroupsPH <- c(unlist(get_significant_groups(diff_groups, samples_groups, TRUE)[1]), "Original")

 		
	}else{
		#print(diff_groups)
		sigGroupsPH <- c(unlist(get_significant_groups(diff_groups, samples_groups, FALSE)[1]), "Original")
	}
	
	if(verbose){
		cat("\nSignificant Groups: \n")
		print(sigGroupsPH)
	}
		
		if(length(grep("No significant", sigGroupsPH[1]))==0){
		
			# then compute effect size for those groups only
			es_exp <- compute_CohensCliff_ESM(sam_exp[sam_exp$Group %in% sigGroupsPH,])
		
			#merge and keep intersection (groups with Effect Size -- significant groups)
			data_exp <- merge(mean_exp, es_exp, by= intersect(names(mean_exp), names(es_exp)))
			ans <- mean_exp[mean_exp$Group=="Original",]
			ans <- cbind(ans, CohensD=NA, Cliff.estimate=NA, Cliff.magnitude=NA)
			data_exp <- rbind(data_exp, ans)
			data_exp <- data_exp[order(data_exp$Value),]
		
			meu_original <- data_exp[data_exp$Group=='Original', 'Value']

			es1 <- as.double(data_exp[1, 'Cliff.estimate'])
			mean_exp <- mean_exp[order(mean_exp$Value),]	
			
			cat(paste('Effect Size: ',es1,"\n",sep=""))
			
			# if significant solution is worse than original 
			if(is.na(es1) || es1 > 0){
				es1 <- as.double(0.1) # default effect_size for not significant solution
				group1 <- paste(as.character(mean_exp[1, 'Group']),"**", sep="")
				meu_group1 <- as.double(mean_exp[1, 'Value'])
				mges1 <- "NA"
			}else{
				#print best alternative for this generation:
				data_exp <- data_exp[order(data_exp$Cliff.estimate),]
				group1 <- as.character(data_exp[1, 'Group'])
				meu_group1 <- as.double(data_exp[1, 'Value'])
				mges1 <- as.character(data_exp[1, 'Cliff.magnitude'])
			}
				
			data_g1 <- data.frame(Gen=genID, Group=group1, EffectSize=es1, Magnitude=mges1, MEU_Group=meu_group1, MEU_Original=meu_original, stringsAsFactors=FALSE)
			
			if(nrow(es_exp)>1){
				es2 <- as.double(data_exp[2, 'Cliff.estimate'])
				
				# if significant solution is worse than original 
				if(is.na(es2) || es2 > 0){
					es2 <- as.double(0.1) # default effect_size for not significant solution
					group2 <- paste(as.character(mean_exp[2, 'Group']), "**", sep="")
					meu_group2 <- as.double(mean_exp[2, 'Value'])
					mges2 <- "NA"
				}else{
					group2 <- as.character(data_exp[2, 'Group'])
					meu_group2 <- as.double(data_exp[2, 'Value'])
					mges2 <- as.character(data_exp[2, 'Cliff.magnitude'])
				}
				
				data_g2 <- data.frame(Gen=genID, Group=group2, EffectSize=es2, Magnitude=mges2, MEU_Group=meu_group2, MEU_Original=meu_original, stringsAsFactors=FALSE)
				#print(data_g2)
				data <- data.frame(row.names=NULL, stringsAsFactors=FALSE)
				data <- rbind(data, data_g1, data_g2)
				return(data)

			}else{
				return(data_g1)
			}
			
		}else{ # no significant difference between solutions and orginal
			cat("RETURNING ROW OF NO SIGNIFICANT DIFFERENCES: \n")
			# select best solutions as those having the lower MEU value
			mean_exp <- mean_exp[order(mean_exp$Value),]	
			meu_original <- mean_exp[mean_exp$Group=='Original', 'Value']

			#print best alternative for this generation:
			group1 <- paste(as.character(mean_exp[1, 'Group']),"**", sep="")
			meu_group1 <- as.double(mean_exp[1, 'Value'])
			es1 <- as.double(0.1)
			mges1 <- "NA"
			
			data_g1 <- data.frame(Gen=genID, Group=group1, EffectSize=es1, Magnitude=mges1, MEU_Group=meu_group1, MEU_Original=meu_original, stringsAsFactors=FALSE)
			
			if(nrow(mean_exp)>1){
				group2 <- paste(as.character(mean_exp[2, 'Group']), "**", sep="")
				meu_group2 <- as.double(mean_exp[2, 'Value'])
				es2 <- as.double(0.1)
				mges2 <- "NA"
				
				data_g2 <- data.frame(Gen=genID, Group=group2, EffectSize=es2, Magnitude=mges2, MEU_Group=meu_group2, MEU_Original=meu_original, stringsAsFactors=FALSE)
				#print(data_g2)
				data <- data.frame(row.names=NULL, stringsAsFactors=FALSE)
				data <- rbind(data, data_g1, data_g2)
				return(data)

			}else{
				return(data_g1)
			}
		}
}

### END FUNCTION ###

# get frequency of solutions in population
for(group in levels(groups_pop2)){ 
        rep <- nrow(df_pop2_run1[df_pop2_run1$Group==group,])/15 
        count <- 0
        if(rep>1){
             count <- count + 1
             cat(paste(count,") ",group," | ",rep,"\n",sep=""))
           }
 }

### START FUNCTION ###

# get statistical results test for groups having different amount of samples 
get_result_statistical_test_different_samples <- function(namesg, samplesg){
	data <- data.frame(row.names=FALSE)
	
    if(length(namesg)==0 || length(namesg)==1)
        stop("incomplete names provided for groups of data (nominal vble values)")

    total_groups = length(samplesg)
    if(total_groups > 0 && total_groups==length(namesg)){
        
        numb
        er_samples = length(unlist(strsplit(samplesg[1], split=",")))
        
        for(k in 1:total_groups){
            #cat(paste("analyzing Group: ",k, "-> ",namesg[k],"\n", sep=""))
            namegk <- namesg[k]
            #cat(paste("analyzing group: ", namegk,"\n", sep=""))
            asgk <- as.double(unlist(strsplit(samplesg[k], split=",")))

            if(k>1 && length(asgk)!=number_samples){
            	#cat(paste('# Samples for Solution ', namegk, ":", length(asgk), "\n", sep=''))
            	number_samples = length(unlist(strsplit(samplesg[k], split=",")))
                #stop(paste("missing data samples for group: ", namegk, ", expecting: ", number_samples, ", got: ", length(asgk),  sep=""))
            }
    
            gk <- data.frame(Group=rep(namesg[k], number_samples), Value=asgk)
            data <- rbind(data, gk)
        }

        data <- mutate(data, Group=factor(Group, levels(unique(Group))))
        # run kruskal wallis test
        res.kw <- kruskal.test(Value ~ Group, data=data)
        return(list(data, res.kw$p.value))

    }else{
        cat(paste("number of groups: ", total_groups, "; number of names provided: ", length(namesg), sep=""))
        stop("no data samples provided or incomplete names for groups of data samples")
    }

}
