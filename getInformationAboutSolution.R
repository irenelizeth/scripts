
path <- "~/Documents/GreenProject/SW Lab/2017/results_seeds17/altMappings/"

## Barbecue
# file <- "barbecue-1.0.0mappingAlt_jcf.csv"
file <- "barbecue-1.0.0mappingAlt_all.csv"

## Commons-cli
# file <- "commons-cli-1.2mappingAlt_jcf.csv"

## JodaTime
# file <- "joda-time-2.1mappingAlt_jcf.csv"
# file <- "joda-time-2.1mappingAlt_all.csv"

mapfile <- paste(path, file, sep='')

print(paste('Reading mapping of alternatives, file: ', mapfile, sep=''))
data_map <- read.csv(file=mapfile, fill=TRUE, col.names=c("site","idAlt","Alternative"), stringsAsFactors=FALSE, header=FALSE)

#select name of alternative for selected by solution:
getNameAlternative <- function(siteId, solId){
	name <- 'NA'
	if(siteId>-1 && solId>1)
    	name <- data_map[data_map$site==siteId & data_map$idAlt==solId, c('Alternative')]
    else
    	cat(paste('No information for Site: ', siteId, ', Alternative: ', solId, sep=''))

    return(name)
}

#given the siteID and the name of the alternative implementation, returns the ID of the given alternative in the mapping
getAlternativeID <- function(siteId, nameId){
	name <- 'NA'
	if(siteId>-1 && !is.na(nameId))
    	idAlt <- data_map[data_map$site==siteId & data_map$Alternative==nameId, c('idAlt')]
    else
    	cat(paste('No information for Site: ', siteId, ', Alternative: ', nameId, sep=''))

    return(idAlt)
}

# Analyze the following aspects about the solution:
# Number of changes in the solution (sitesId != 1)
# Where changes took place (siteIDs)
# Which library implementations where selected for changes
# frequency of selected library implementations
analyzeSolutionGroup <- function(solution, subjectApp, verbose=FALSE){
		
	library("hash")
	parts <- unlist(strsplit(solution, '\\['))[2]
	solID <- unlist(strsplit(parts, '\\]'))[1]
	
	#analyze solution by sites
	sitesSol <- unlist(strsplit(solID, split='[.]'))
	
	totalChanges <- 0
	hImpl <- hash()
	maxFreqRep <- 0
	maxFreqImpl <- 'NA'
	sitesSet <- set()
	count <- 1 
	
	for(k in 1:length(sitesSol)){
		siteChange <- unlist(strsplit(sitesSol[k], '[:]'))
		site <- as.integer(siteChange[1])
		impl <- as.integer(siteChange[2])
		
		if(impl!=1){
			totalChanges <- totalChanges+1
			sitesSet <- set_union(sitesSet, site)
			nameImpl <- getNameAlternative(site, impl)
			
			if(verbose){
				cat(paste('Site: ', site, '- Impl: ', impl, sep=''))
				cat(paste(' ', nameImpl, "\n", sep=''))
			}
			
			if(!has.key(nameImpl, hImpl)){
				.set(hImpl, nameImpl, 1)
			}else{
				count <- as.integer(hImpl[[nameImpl]])
				count <- count+1
				hImpl[[nameImpl]] <- count
			}
			
			if(count > maxFreqRep){
				maxFreqRep <- count
				maxFreqImpl <- nameImpl
			}
		}	
		
	}
	
	repImpl <- ""
	for(k in keys(hImpl)){
		if(as.integer(hImpl[[k]])>1)
			repImpl <- paste(repImpl, k, " (", hImpl[[k]], "), ", sep="")
	}
	
	#printing information
	cat(paste(solution, " for ", subjectApp, ': \n', sep=''))
		
	dfSol <- data.frame(Solution=solution, TotalSites=length(sitesSol), TotalChanges=totalChanges, FreqImpl=repImpl, SitesChanges=as.character(paste(sitesSet, collapse=",")), stringsAsFactors=FALSE)

	return(dfSol)
}
	
