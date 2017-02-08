#!/Library/Frameworks/R.framework/Resources/bin/Rscript 
 
#Irene Manotas
#2017-02-07
#Procesing GA's data file: input is a csv file with all data results from ga algorithm (solution, objective value, fitness value)
# output is two csv files containing the objective values per run of the ga algo. in columns format and the solutions per run in columns format, respectively

suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(optparse))
  
 option_list= list(
    make_option(c("-d","--dir" ), action="store", default=".ga_runs", type='character',
    help="directory to store csv files with ga results per run"),
    make_option(c("-f","--file"), action="store", default=NA, type='character',
    help="single log file containing GA aggregated data results"),
    make_option(c("-n","--name"), action="store", default="app", type="character",
    help="subject application's name"),
    make_option(c("-nr", "--numruns"), action="store", default=10, type="integer",
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
    numPop = 1
    gen=1
    ind=1
    pop=1
    N=as.integer(nrow(data_set)/opt$popsize)

    # N should be 100: 100 generations of 50 individuals (population) each
    allpop <- data.frame(row.names=FALSE)
    allsol <- data.frame(row.names=FALSE)

    xrun <- seq(from=1, to=10, by=1)
    runN <- data.frame(row.names=FALSE)

    #compute extra individuals per generation -- need to delete extra individuals after collecting max. eval for each run
    extra_ind <- nrow(data_set)/number_of_runs - max_number_eval_per_run

    # TODO: replace fixed 50 by popsize, and 100 by max_num_eval_per_run/popsize
    while(gen < N){
        if(numPop %% 100 == 0){
            runN <- rbind(runN, data_set[ind:(ind+49),])
            id_run <- paste('run', round(gen/100), sep='')
            print(paste(id_run, ' No. rows: ', nrow(runN),  sep=''))
            #write.csv(genN, file=paste("run", appName, round(gen/100), sep=""))
            # save objective values of each run as a column of allpop df
            allpop <- cbind(allpop, id_run=runN[,2])
            allsol <- cbind(allsol, id_run=runN[,1])
            numPop <- 1
            runN <- data.frame(row.names=FALSE)
            # advance row indice to exclude extra individuals generated in last run:
            print(paste('ind index at: ', ind, ' before removing extra ind', sep=''))
            ind <- ind + 50 + extra_ind
            print(paste('ind index at: ', ind, ' after removing extra ind', sep=''))
        }
        else if (ind < nrow(data_set)){
            if(nrow(runN)==1)
                print(paste('--> Next Run: ind index at: ', ind, ', until index:', (ind+49), sep=''))
            runN <- rbind(runN, data_set[ind:(ind+49),])
            numPop <- numPop + 1
            gen <- gen + 1
            ind <- ind + 50
        }else{
            gen = N
        }
    }

    runs <- as.character(seq(1,10,1))
    col_names <- paste(runs, "run", sep='' )
    colnames(allpop) <- col_names
    colnames(allsol) <- col_names
    
    print(paste('file name for csv files is: ', file, sep=""))

    write.csv(allpop, file=paste(file, appName, "_objs.csv", sep=''))
    write.csv(allsol, file=paste(file, appName, "_sols.csv", sep=''))
}

if(!is.na(opt$file)){
    
    data <- read.csv(file=opt$file, fill=TRUE)
    if(ncol(data)>3)
        data <- data[,2:ncol(data)]
    
    get_data_runs(data, opt$dir)

}else{
    cat("No file for ga datas set was specified, see --help\n", file=stderr())
}

