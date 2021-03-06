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

# order: barbecue, jodatime, commons-lang, xml-security, jdepend, jfreechart, gson
test_time <- c(5.3, 3.0, 90.0, 124.0, 4.0, 60.0, 2.0)
repetitions <- c(10, 10, 3, 10, 10, 5, 5)

setwd(args[1])


# compute SEEDS cost (original, exhaustive strategy) by considering JCF, others and ALL implementations
compute_cost_general <- function(data_set){
    
    nsites <- nrow(data_set)
    cat(paste(data_set[1,1], ", ", sep=""))
    
    for(col in c(3,5)){ # indice col for number alternatives
        tcost = 0
        # time rewrite apps + optimize
        tRwr = sum(data_set[,col])*2 #tRr = data[i,3] + tRr
        
        # time test regression
        ttime = test_time[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tReg = (sum(data_set[,col])+1)*ttime
        
        # time LEAP machine + time analyze (sync, filter, totaleu)
        rep = repetitions[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tLeap = ttime*sum(data_set[,col])*3*rep
        
        # cost [hrs]
        tcost = (tRwr + tReg + tLeap)/3600
        
        # print magnitude of search space
        cat(sprintf("%.2f, ", sum(data_set[,col])))
        
        # print cost of Analysis
        cat(sprintf("%.2f, ", (tLeap/3600)*2/3))
        
        cat(sprintf("%.2f, ", tcost))
    }
    
    cat("\n")
    
    # TO-DO:write alternatives cost to CSV file
    
}


compute_cost_alternatives <- function(data_set){
    
    nsites <- nrow(data_set)
    cat(paste(data_set[1,1], ", ", sep=""))
    
    for(col in 5:15){ # indice col for number alternatives
        
        tcost = 0
        
        # time rewrite apps + optimize
        tRwr = sum(data_set[,col])*2 #tRr = data[i,3] + tRr
        
        # time test regression
        ttime = test_time[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tReg = (sum(data_set[,col])+1)*ttime
        
        # time LEAP machine + time analyze (sync, filter, totaleu)
        rep = repetitions[switch(data_set[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tLeap = ttime*sum(data_set[,col])*3*rep=
        
        # cost [hrs]
        tcost = (tRwr + tReg + tLeap)/3600
        
        cat(sprintf("%.2f, ", tcost))
    }
    
    cat("\n")
    
    # TO-DO:write alternatives cost to CSV file
    
}

compute_cost_sites <- function(data_set){
    
    nsites <- nrow(data_set)
    cat(paste(data_set[1,1], ", ", sep=""))
    
    #iterate over percentage of sites to condider
    per = 1
    while(per>=0.1 && per<=1){
        
        limit <- floor(per*nrow(sorted_sites))
        if(limit<1) limit <- 1
        sites <- sorted_sites[1:limit,1]
        
        rows <- as.integer(row.names(data_set[which(data_set[,2] %in% sites),]))

        #only consider selected hotspot sites
        data <- data_set[rows,]

        tcost = 0
        # time rewrite apps + optimize
        tRwr = sum(data[,5])*2 #tRr = data[i,3] + tRr

        # time test regression
        ttime = test_time[switch(data[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tReg = (sum(data[,5])+1)*ttime
        
        # time LEAP machine + time analyze (sync, filter, totaleu)
        rep = repetitions[switch(data[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tLeap = ttime*sum(data[,5])*3*rep
        
        # cost [hrs]
        tcost = (tRwr + tReg + tLeap)/3600

        cat(sprintf("%.2f, ", tcost))

        per = per - 0.1
    }
    
    cat("\n")
    
    # TO-DO:write cost hotspots strategy to CSV file
    
}


compute_cost_combined <- function(data_set){
    
    nsites <- nrow(data_set)
    cat(paste(data_set[1,1], ", ", sep=""))
    
    #iterate over percentage of sites to condider
    per = 1.1
    col = 5
    while(per>=0.1 && per<=1.1){
        
        if(per==1.1)
            limit <- nrow(sorted_sites)
        else
            limit <- floor(per*nrow(sorted_sites))
        
        if(limit<1) limit <- 1
        sites <- sorted_sites[1:limit,1]
        
        rows <- as.integer(row.names(data_set[which(data_set[,2] %in% sites),]))
        
        #only consider selected hotspot sites
        data <- data_set[rows,]
        
        tcost = 0
        # time rewrite apps + optimize
        tRwr = sum(data[,col])*2 #tRr = data[i,3] + tRr
        
        # time test regression
        ttime = test_time[switch(data[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tReg = (sum(data[,col])+1)*ttime
        
        # time LEAP machine + time analyze (sync, filter, totaleu)
        rep = repetitions[switch(data[1,1], "barbecue"=1, "jodatime"=2, "commons-lang"=3, "xml-security"=4, "jdepend"=5, "jfreechart"=6, "gson"=7)]
        tLeap = ttime*sum(data[,col])*3*rep
        
        # cost [hrs]
        tcost = (tRwr + tReg + tLeap)/3600
        
        cat(sprintf("%.2f, ", tcost))
        
        per = per - 0.1
        col = col + 1

    }
    
    cat("\n")
    
    # TO-DO:write cost hotspots strategy to CSV file
    
}

# option: 1, get original costs
option <- args[2]

# get original costs
if(args[2]==1){

    cat("Costs: Alternatives Strategy\n")
    cat(paste("subject, ", "search(JCF), ", "analysis(JCF), ", "cJCF, ", "search(All), ", "analysis(All), ",  "cAll","\n", sep=""))

    for(item in list.files()){
    
        data <- read.csv(item, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
    
        compute_cost_general(data)
    }

}else{
# get costs for all strategies

    ###
    ### computing cost for implementations strategy
    
    cat("Costs: Alternatives Strategy\n")
    # print cost of choosing different percentages of energy efficient alternatives:
    cat(paste("subject, ", "cAll, ", "c100, ", "c90, ", "c80, ", "c70, ", "c60, ", "c50, ", "c40, ", "c30, ", "c20, ", "c10", "\n", sep=""))
    
    for(item in list.files()){
        
        data <- read.csv(item, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
        
        compute_cost_alternatives(data)
    }
    
    ###
    ### computing cost for hotspot strategy
    
    cat("\n")
    
    cat("Costs: Sites Strategy\n")
    cat(paste("subject, ", "s100, ", "s90, ", "s80, ", "s70, ", "s60, ", "s50, ", "s40, ", "s30, ", "s20, ", "s10", "\n", sep=""))
    
    for(item in list.files()){
        
        data <- read.csv(item, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
        subjectName <- data[1,1]
        
        path_sites_hitcount = paste("../../",subjectName,"-hitcount.sites.csv",sep="")
        
        # as.is=c(2) do not factor values in second column
        sites_data = read.csv(path_sites_hitcount, as.is=c(2))
        
        #select sites based on higher execution count
        sorted_sites <- sites_data[order(sites_data$clover, decreasing=TRUE),]
        
        # get indices of sites which have implementations in dataset
        ind <- which(sorted_sites[,1] %in% data[,2])
        # update hitcount data, only consider sites with hitcount data
        sorted_sites <- sorted_sites[ind,]
        
        compute_cost_sites(data)
    }
    
    ###
    ### computing cost for implementations/hotspot combined strategy
    
    cat("\n")
    
    cat("Costs: Strategies Combined\n")
    cat(paste("subject, ", "cAll, ", "c100, ", "c90, ", "c80, ", "c70, ", "c60, ", "c50, ", "c40, ", "c30, ", "c20, ", "c10", "\n", sep=""))
    
    for(item in list.files()){
        
        data <- read.csv(item, skipNul=TRUE, header=TRUE, stringsAsFactors=FALSE, row.names=NULL)
        subjectName <- data[1,1]
        
        path_sites_hitcount = paste("../../",subjectName,"-hitcount.sites.csv",sep="")
        
        # as.is=c(2) do not factor values in second column
        sites_data = read.csv(path_sites_hitcount, as.is=c(2))
        
        #select sites based on higher execution count
        sorted_sites <- sites_data[order(sites_data$clover, decreasing=TRUE),]
        
        # get indices of sites which have implementations in dataset
        ind <- which(sorted_sites[,1] %in% data[,2])
        # update hitcount data, only consider sites with hitcount data
        sorted_sites <- sorted_sites[ind,]
        
        compute_cost_combined(data)
    }

}




