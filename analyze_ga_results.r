#!/Library/Frameworks/R.framework/Resources/bin/Rscript 

#Irene Manotas
#2016-12-14
#Procesing SEEDS GA Log files


suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(optparse))

option_list= list(
        make_option(c("-d","--dir" ), action="store", default=NA, type='character',
             help="directory for ga log files for an app subject"),
        make_option(c("-f","--file"), action="store", default=NA, type='character',
             help="single log file containing GA results"),
        make_option(c("-s", "--numsites"), action="store", default=NA, type='integer',
             help="number of application sites that were modified")
        )

opt = parse_args(OptionParser(option_list=option_list))



get_data_frame_generations <- function(data, total_sites){
    
    # keep rows containing configuration of slns, value=FALSE to return ID of rows matching pattern (value=TRUE, returns rows' content)
    data_sols <- grep('[[:digit:],]+', data$V1, value=TRUE)
    rows_to_keep <- grep('[[:digit:],]+', data$V1, value=TRUE)
    # create sln configuration based on number of sites
    s <- sprintf('%s,', seq(from=0, to=total_sites-1, by=1))
    config <- paste(s, collapse='')
    # remove rows that contain indices for configuration vector
    rows_delete <- grep('0,1,2,3,4,5,6,7,8,9,', data_sols, value=FALSE)
    data_sols <-  I(as.vector(as.character(data_sols[-rows_delete])))
    
    # select rows with objective function's value
    rows_obj <- grep('Objective', data$V2, value=FALSE)
    data_obj <-  as.vector(as.numeric(as.character(data[rows_obj,5])))
    
    #select rows with fitness value
    rows_fit <- grep('Fitness', data$V2, value=FALSE)
    data_fit <-  as.vector(as.numeric(as.character(data[rows_fit,5])))
    # join all data columns in one data frame
    data_subject <- data.frame(data_sols, data_obj, data_fit)
    
    gen=1
    ind=1
    N=as.integer(nrow(data_subject)/50)
    print(paste('Number of generations: ',N,sep=""))
    allpop <- data.frame(row.names=FALSE)
    while(gen<N+1){
        row <- which(data_subject[ind:(ind+50),2] == min(data_subject[ind:(ind+50),2]))
        res <- cbind(gen, (data_subject[ind:(ind+50),][row,]))
        allpop <- rbind(allpop, res)
        gen= gen+1
        ind=ind+50
    }
    
    return(allpop)
}


if(!is.na(opt$dir)|| !is.na(opt$file)){
    
    
    if(is.na(opt$numsites)){
        cat("WARNING! No total number of sites were specified (-s option). Using default number of sites = 10 (See --help)\n", stderr())
        opt$numsites = 10
    }
    
    if(!is.na(opt$file)){
    
        data <- read.table(file=opt$file, fill=TRUE)
        total_sites <- as.integer(opt$numsites)
        print(paste("sites: ", total_sites, sep=""))
        allpop <- get_data_frame_generations(data, total_sites)
        
        file_name = paste(opt$file, '_allgen', sep='')
        write.csv(allpop, file=paste(file_name, '.csv',sep=''))
        print(paste('Data file saved as: ', file_name, '.csv', sep=''))

    }else{
        
        path = opt$dir
        print(opt$dir)
        nf = 1
        
        for(data_file in list.files(path='bb_data_1r/')){
            
            data <- read.table(file=paste(opt$dir,"/",data_file,sep=""), fill=TRUE)
            total_sites <- opt$numsites
            print(paste("sites: ", total_sites, sep=""))
            allpop <- get_data_frame_generations(data, total_sites)
            
            file_name = paste(path, 'allgen', nf, sep='')
            write.csv(allpop, file=paste(file_name, '.csv',sep=''))
            print(paste('Data file saved as: ', file_name,'.csv', sep=''))

            # graph of best energy usage found in each population through the GA (gGA)
            jpeg(file=paste(file_name,'.jpg',sep=''))
            ggplot(data_subject, aes(gen, data_obj)) + geom_point() + geom_smooth() +
            labs(title='Barbecue GA Results gGA Algorithm', x='All Runs All Generations (population:51)', y='Best Energy    Usage')
            dev.off()
            
            nf = nf+1
        }

     
    }
    
}else{ # end options conditional
    cat("No file or directory of log files were specified, see --help\n", file=stderr())
}
