#!/Library/Frameworks/R.framework/Resources/bin/Rscript 
 
#Irene Manotas
#2017-02-07
#Procesing GA's data file: input is a csv file with all data results from ga algorithm (solution, objective value, fitness value)
# output is two csv files containing the objective values per run of the ga algorithm, use columns format to store solutions per run 
# in one file and the individuals's objective values in another file.

suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(optparse))
  
 option_list= list(
    make_option(c("-d","--dir" ), action="store", default=".ga_runs", type='character',
    help="directory to store csv files with ga results per run"),
    make_option(c("-f","--file"), action="store", default=NA, type='character',
    help="single log file containing GA aggregated data results"),
    make_option(c("-n","--name"), action="store", default="app", type="character",
    help="subject application's name"),
    make_option(c("-r", "--numruns"), action="store", default=10, type="integer",
    help="number of runs the ga was executed"),
    make_option(c("-e","--maxeval"), action="store", default=5000, type="integer",
    help="maximum total of evaluations per run defined for GA algorithm"),
    make_option(c("-p","--popsize"), action="store", default=50, type="integer",
    help="population size used in ga algorithm")
 )    
         
opt = parse_args(OptionParser(option_list=option_list))


get_data_runs <- function(data_set, file){

    number_of_runs = opt$numruns
    max_number_eval_per_run = opt$maxeval
    appName = opt$name
    popSize = opt$popsize # every popSize-2 evaluations a new population is creaated
    numPop = 1
    gen=1
    ind=1
    pop=1
    #N=as.integer(nrow(data_set)/opt$popsize)
    N = as.integer((nrow(data_set)-popSize*number_of_runs)/(popSize-2))

    total_gen = ceiling((max_number_eval_per_run-popSize)/(popSize-2))

    # N should be 100: 100 generations of 50 individuals (population) each
    allpop <- data.frame(row.names=FALSE)
    allsol <- data.frame(row.names=FALSE)

    #compute extra individuals per generation -- need to delete extra individuals after collecting max. eval for each run
    #extra_ind <- nrow(data_set)/number_of_runs - max_number_eval_per_run

    collect_initial_population <- function(ind, popSize){

        runN <- data.frame(row.names=FALSE)
        runN <- rbind(runN, data_set[ind:(ind+popSize-1),])
        id_run <- paste('run', round(gen/total_gen), sep='')
        #print(paste(id_run, ' No. rows: ', nrow(runN),  sep=''))

        # add current best solutions to next population
        best1 <- which(runN[,2]==min(runN[,2]))[1] # could be more than one sln with same obj. value, select sln that appears first
        rb1 <- runN[best1,]
        runNu <- runN[-best1,]
        best2 <- which(runNu[,2]==min(runNu[,2]))[1]
        rb2 <- runNu[best2,]

        #update index
        ind <- ind + (popSize)
        resPop <- list(runN, rb1, rb2, ind)
        return(resPop)
    }

    resPop <- collect_initial_population(ind, popSize)
    runN <- as.data.frame(resPop[1]) 
    rb1 <- as.data.frame(resPop[2])
    rb2 <- as.data.frame(resPop[3])
    ind <- as.integer(resPop[4])

    #print(paste('best slns. obj. values: ', rb1[2],', ', rb2[2], sep=''))

    while(gen < N){
        if(numPop %% total_gen == 0){

            #adding last generation to data of run
            runN <- rbind(runN, rb1, rb2) # add previous best slns. from previous population
            runN <- rbind(runN, data_set[ind:(ind+(popSize-3)),])

            #print(paste('best slns. obj. values: ', rb1[2],', ', rb2[2], ' --> population: ', numPop, sep=''))
            id_run <- paste('run', round(gen/total_gen), sep='') 
            print(paste('Finished ', id_run, ' No. rows: ', nrow(runN),  sep=''))
          
            #write.csv(genN, file=paste("run", appName, round(gen/100), sep=""))
            # save objective values of each run as a column of allpop df
            allpop <- cbind(allpop, id_run=runN[,2])
            allsol <- cbind(allsol, id_run=runN[,1])
            ind <- ind + (popSize-2)
            #next population
            resPop <- collect_initial_population(ind, popSize)
            runN <- as.data.frame(resPop[1])
            rb1 <- as.data.frame(resPop[2])
            rb2 <- as.data.frame(resPop[3])
            ind <- as.integer(resPop[4])

            numPop <- 1
        }
        else if (ind < nrow(data_set)){
            
            genN <- data.frame(row.names=FALSE)
            genN <- rbind(genN, rb1, rb2) # add best solutions from previous population
            genN <- rbind(genN, data_set[ind:(ind+(popSize-3)),])
            
            runN <- rbind(runN, genN)
            
            #select current best solutions for next generation
            best1 <- which(genN[,2]==min(genN[,2]))[1]
            rb1 <- genN[best1,]
            genN <- genN[-best1,]
            best2 <- which(genN[,2]==min(genN[,2]))[1]
            rb2 <- genN[best2,]

            #print(paste('best slns. obj. values: ', rb1[2],', ', rb2[2], ' --> population: ', numPop, sep=''))
            
            numPop <- numPop + 1
            gen <- gen + 1
            ind <- ind + (popSize-2)
        }else{
            gen = N
        }
    }

    runs <- as.character(seq(1,10,1))
    col_names <- paste(runs, "run", sep='' )
    colnames(allpop) <- col_names
    colnames(allsol) <- col_names
    
    fileNameObj=paste(file, appName, "_objs.csv", sep='')
    fileNameSol=paste(file, appName, "_sols.csv", sep='')
    print(paste('file name for csv files is: ', fileNameObj, ' and ', fileNameSol,  sep=""))

    write.csv(allpop, file=fileNameObj, row.names=FALSE)
    write.csv(allsol, file=fileNameSol, row.names=FALSE)
}

if(!is.na(opt$file)){
    
    data <- read.csv(file=opt$file, fill=TRUE)
    if(ncol(data)>3)
        data <- data[,2:ncol(data)]
    
    get_data_runs(data, opt$dir)

}else{
    cat("No file for ga datas set was specified, see --help\n", file=stderr())
}

