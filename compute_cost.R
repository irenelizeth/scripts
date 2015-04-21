#!/usr/bin/env Rscript

# format's input file :
# subject's name, site no., total implem., top 100 energy-efficient(ee), top 90 ee, ..., top 10 ee
# subject_name, site, total, top.100, top.90, ..., top.10

args <-commandArgs(TRUE)

# path to directory with data files
dir <- args[1]

if(!file.exists(args[1]))
stop(paste(args[1]," doesn't exist", sep=""))

suppressWarnings(suppressPackageStartupMessages(library(hash)))

# compute seeds cost

# order: barbecue, jodatime, commons-lang, xml-security, jdepend, jfreechart
test_time <- c(5.3, 3.0, 90.0, 124.0, 4.0, 60.0)
repetitions <- c(10, 10, 3, 10, 10, 5)

setwd(args[1])

compute_cost <- function(data_set, file_name){
    
    nsites <- nrow(data_set)
    cat(paste(data_set[1,1], ", ", sep=""))
    
    for(col in 3:13){ # indice col for number alternatives
        
        tcost = 0
        
        # time rewrite apps + optimize
        tRwr = sum(data_set[,col])*2 #tRr = data[i,3] + tRr
        
        # time test regression
        ttime = test_time[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6)]
        tReg = (sum(data_set[,col])+1)*ttime
        
        # time LEAP machine + time analyze (sync, filter, totaleu)
        rep = repetitions[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6)]
        tLeap = ttime*sum(data_set[,col])*3*rep
        
        # cost [hrs]
        tcost = (tRwr + tReg + tLeap)/3600
        
        cat(sprintf("%.2f, ", tcost))
    }
    
    cat("\n")
    
    # TO-DO:write alternatives cost to CSV file
    
}


# print cost of choosing different percentages of energy efficient alternatives:
cat(paste("subject, ", "cAll, ", "c100, ", "c90, ", "c80, ", "c70, ", "c60, ", "c50, ", "c40, ", "c30, ", "c20, ", "c10", "\n", sep=""))

for(item in list.files()){
    
    data <- read.csv(item, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    
    compute_cost(data, "alternatives")
}



