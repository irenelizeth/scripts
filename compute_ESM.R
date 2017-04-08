suppressPackageStartupMessages(require(effsize))
suppressPackageStartupMessages(require(lsr)) # cohensD impl.
suppressPackageStartupMessages(require(methods)) # required by lsr library to load function 'is'

args <- commandArgs(TRUE)

path <- args[1]
trials <- args[2]
out_file <- args[3]


compute_ESM <- function(df_samples, trial){
	data_esm <- data.frame(row.names=FALSE)
	
	data_sg <- aggregate(.~Group, data=df_samples, function(x) unlist((strsplit(as.character(x), split=","))))
	idTrial <- paste("Trial-", trial, sep="")
	idOri <- which(data_sg$Group=="Original")
	Original <- as.double(data_sg[idOri,2])
	data_sg <- data_sg[-idOri,] # remove original samples from data set
	
	for(k in 1:nrow(data_sg)){
      group <- data_sg[k,1]
      sampleData <- as.double(data_sg[k,2])
      listSamples <- paste(sampleData, collapse=" ")
      
      # compute ESM
      d <- cohensD(sampleData, Original)
      Ae <- VD.A(sampleData, Original)$estimate  
      Am <- as.character(VD.A(sampleData, Original)$magnitude)
      ce <- cliff.delta(sampleData, Original)$estimate
	  cm <- as.character(cliff.delta(sampleData, Original)$magnitude)
	  	
	  row <- data.frame(Group=group, Samples=listSamples, CohensD=d, VDA.estimate=Ae, VDA.magnitude=Am, Cliff.estimate=ce, Cliff.magnitude=cm)
	  data_esm <- rbind(data_esm, row)
      
   }
   
   # order in ascending order by cliff values, and in descending order by cohensD values
   data_esm[order(data_esm$Cliff.estimate, -data_esm$CohensD),]
   
   return(data_esm)
}


# obtain the best solution for a given trial by computing the ESM values of all solutions in the trial
# path: path where data_sample.csv files for each trial are located
# trials: total # of trials to consider (total # of CSV files)
# out_file: name of output CSV file with summary of results (best sol. for each trial)
getBestSolutionsForTrial <- function(path, trials, out_file){
	
	data_results <- data.frame(Trial=integer(0), Group=character(0), N.Samples=integer(0),  SD.Samples =double(0), MeanEU.Samples=double(0), MeanEU.Original=double(0), SavingsMEU=double(0), Cliff.estimate= double(0), Cliff.magnitude=character(0), N.Groups.Same.Magnitud=integer(0))
	
	file_sample = 'data_samples'
	
	for(k in 1:trials){
		df_exp <- read.csv(file=paste(path, file_sample, k, '.csv', sep=''), header=TRUE)
		df_mean <- aggregate(.~Group, data=df_exp, mean)
		original_mean <- df_mean[df_mean$Group=="Original",]$Value
		
		res <- compute_ESM(df_exp, k)
		res <- merge(res, df_mean, by=intersect(names(res), names(df_mean)))
		res <- res[order(res$Cliff.estimate, res$Value),]
		
		# Number of solutions with same magnitud (large == -1)
		df_comp <- res[res$Cliff.estimate==res[1,]$Cliff.estimate,]
		sameMag <- nrow(df_comp)
		
		# select group with lowest cliff's value
		results <- res[1, c("Group", "Cliff.estimate", "Cliff.magnitude")]
		results <- cbind(N.Trial=k, results)
		results <- cbind(results, N.Groups.Same.Magnitud=sameMag)
		
		samples <- as.double(unlist(strsplit(as.character(res[1,2]), split=' ')))
		
		#save results of cliff's delta for all groups in trial
		dfEs_k <- merge(df_mean, res, by=intersect(names(res), names(df_mean)))
		dfEs_k <- dfEs_k[dfEs_k$Value < original_mean,]
		#write.csv(dfEs_k[order(dfEs_k$Cliff.estimate),c("Group","Value","CohensD","VDA.estimate","VDA.magnitude","Cliff.estimate","Cliff.magnitude")], file=paste(path, 'all_data_cliff_',k,'.csv', sep=''))
		
		#statistics samples taken
		n_samples <- length(samples)
		sd_samples <- sd(samples)
		#mean_samples <- mean(samples)
		mean_samples <- df_mean[df_mean$Group==results$Group,]$Value
		
		if(mean_samples > original_mean){
			results$Group = "Original"
			mean_samples = original_mean
		}
		
		savings <- 1-(mean_samples/original_mean)
		
		stat <- data.frame(N.Samples=n_samples, SD.Samples=sd_samples, MeanEU.Samples =mean_samples, MeanEU.Original=original_mean, SavingsMEU=savings)
		row <- as.data.frame(append(results, stat, after=2))
		
		data_results <- rbind(data_results, row)
	}
	
	print(data_results)
	
	write.csv(data_results, file=paste(path, out_file,".csv", sep=""), row.names=FALSE)
}

