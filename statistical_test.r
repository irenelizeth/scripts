#!/usr/bin/env Rscript 

#Irene Manotas
#2017-03-07
#Statistical Test - Non-parametric Kruskal Wallis Test
# Assumption: Data does not follow normal distribution
# H0: Groups of data have the same mean ranks
# to reject H0 p-value < 0.05

suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(FSA))
suppressPackageStartupMessages(require(lattice))
# for multicomparison analysis
suppressPackageStartupMessages(require(rcompanion))
suppressPackageStartupMessages(require(multcompView))
suppressPackageStartupMessages(require(optparse))
suppressPackageStartupMessages(require(lsr))
suppressPackageStartupMessages(require(methods)) # required by lsr library to load function 'is'

option_list = list(
    
    make_option(c("-n","--names"), action="store", default=NA, type="character",
        help="names for groups of data separated by a semicolon"),
    make_option(c("-s","--samples"), action="store", default=NA, type="character",
        help="samples for groups of data, each group separated by a semicolon"),
    make_option(c("-a", "--alpha"), action="store", default=0.05, type="double",
        help="significance level used for statistical test to reject null hypothesis")
)

opt = parse_args(OptionParser(option_list=option_list))


if(is.na(opt$names) || is.na(opt$samples))
    stop("Input parameters are incomplete, see --help")

list_names <- unlist(strsplit(opt$names,";"))
list_samples <- unlist(strsplit(opt$samples, ";"))
alpha <- opt$alpha

# global variables
data <- data.frame(row.names=FALSE)


# namesg: names for groups of data separated by semicolons
# samplesg: list of samples for groups of data separated by commas, groups separated by semicolon
get_result_statistical_test <- function(namesg, samplesg){

    if(length(namesg)==0 || length(namesg)==1)
        stop("incomplete names provided for groups of data (nominal vble values)")

    total_groups = length(samplesg)
    if(total_groups > 0 && total_groups==length(namesg)){
        
        number_samples = length(unlist(strsplit(samplesg[1], split=",")))
        
        for(k in 1:total_groups){
            
            namegk <- namesg[k]
            #cat(paste("analyzing group: ", namegk,"\n", sep=""))
            asgk <- as.double(unlist(strsplit(samplesg[k], split=",")))

            if(k>1 && length(asgk)!=number_samples){
                stop(paste("missing data samples for group: ", namegk, ", expecting: ", number_samples, ", got: ", length(asgk),  sep=""))
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

# data: data frame with one column for measurment variable and one column for nominal vble (groups)
# adjust.method: method used to correct the p-values (familywise error:bonf, or false discovery rate:bh, or none)
# post-hoc analysis: perform a Wilcoxon (Mann Whitman) Test
compute_wilcoxon <- function(data, adjust.method){
    
    data <- mutate(data, Group=factor(data$Group, levels(unique(data$Group))))
    PT <- pairwise.wilcox.test(data$Value, data$Group, p.adjust.method=adjust.method)
    
    #cat("\n\n Results from pairwise Wilcoxon test:\n\n")
    print(PT)
    PT$p.value
    PT <- PT$p.value
    PT1 <- fullPTable(PT)
    
    res <- multcompLetters(PT1, compare="<", threshold=alpha, Letters=letters, reversed=FALSE)
    print(res)
    ## groups sharing same character identify groups that are not significantly different
    
    dimMg <- dim(res$LetterMatrix)
    ## if only one column then all groups share same character
    if(dimMg[2]==1){
        cat("\nGroups are not significantly different from each other! \n")
        
    }else if(dimMg[1]>2){ # more than two groups to compare
        #cat("\nGroups are significantly different:")
        groupLetters <- res$Letters
        suppressWarnings(suppressMessages(library(sets)))
        setDiffGroups <- set()
        setDiffG <- set()
        
        for(k in 2:length(groupLetters)){
            letter <- as.character(groupLetters[k-1])
            letterk <- as.character(groupLetters[k])
            
            if(!(identical(letter, letterk))){
                setDiffGroups <- setDiffGroups + set(rownames(res$LetterMatrix)[k-1])
                setDiffGroups <- setDiffGroups + set(rownames(res$LetterMatrix)[k])
                #setDiffG <- setDiffG + set(k-1)+ set(k)
            }
        }
        return(setDiffGroups)
    }


} # end compute_wilcoxon


# data: data frame with one column for measurment variable and one column for nominal vble (groups)
# adjust.method: method used to correct the p-values (familywise error:bonf, or false discovery rate:bh, or none)
# post-hoc analysis: perform a Dunn Test
compute_dunntest <- function(data, adjust.method, q){

    suppressWarnings(suppressMessages(library(dunn.test)))
     
    #cat("\n\nResults from Dunn Test for multicomparison of groups:\n")
    # from package FSA
    #PTdt <- dunnTest(data$Value, data$Group, method=adjust.method, two.sided=FALSE)
    #print(PTdt)
    #PTdt <- PTdt$res
    #rdt <- cldList(comparison=PTdt$Comparison, p.value = PTdt$P.adj, threshold=alpha)

    # dunn.test function from dunn.test package
    PT2 <- dunn.test(data$Value, data$Group, method=adjust.method, table=FALSE, kw=FALSE)
    res <- data.frame(PT2$P, PT2$P.adjusted, PT2$comparisons, stringsAsFactors=FALSE)
    colnames(res) <- c("P", "P.adj", "test")
    
    # accept as significant all tests for which largest.p.adjusted < q

    listDiffGroups <- list()
    listESGroups <- list()
    for(k in 1:nrow(res)){
        if(adjust.method=="BH" && res$P.adj[k] < q){
            #cat(res$test[k])
            groups <- unlist(strsplit(res$test[k],"-"))
            group1 <- groups[1]
            group2 <- groups[2]
            group1 <- gsub(" ", "", group1)
            group2 <- gsub(" ", "", group2)
            #print(group1)
            g1 <- vector()
            if(group1=="Original" || group2=="Original"){
                if(group1=="Original"){
                    sGroup=group2
                    g1 <- as.vector(unlist(data[data$Group==sGroup,][2]))
                }else{
                    sGroup=group1
                    g1 <- as.vector(unlist(data[data$Group==sGroup,][2]))
                }

                listDiffGroups <- c(listDiffGroups, sGroup)  

                g2 <- as.vector(unlist(data[data$Group=="Original",][2]))
                #print(g1)
                #print(g2)
                # compute Cohen's d effect of size between signficant treatments (groups)
                effect_size <- cohensD(g1, g2) # from library(lsr)
                listESGroups <- c(listESGroups, effect_size)
            }
        }
        #else if (res$P.adj[k] < 0.05)
        #    listDiffGroups <- c(listDiffGroups, res$test[k])
    }

    return(list(listDiffGroups, listESGroups))
}


# perform pot-hoc analysis: multicomparison between groups of data
get_results_multicomparison <- function(data, pvalue){

    # write data values for every group under analysis
    write.table(data, file="data_st.csv", sep=",",append=TRUE)    
    
    adjust.method = "BH"
    # false discovery rate for BH method:
    q = 0.05
    
    dataEffectSize = data.frame(Group=character(0), EffectSize=integer(0))
    
    #if p-value < alpha then perform multicomparison test:
    if(pvalue < alpha){
        res <- compute_dunntest(data, adjust.method, q)
        
        if(length(unlist(res[1]))>0){
          cat(unlist(res[1]))
          cat(";")
          cat(unlist(res[2]))
       
          dataEffectSize = data.frame(Group=as.vector(unlist(res[1])), EffectSize=as.double(unlist(res[2])))
        }else{
            cat("No significant groups after post hoc analysis (multicomparison)")
        }
    }else{
        cat("No significant difference found")
    }

    write.table(dataEffectSize, file="data_effectsize.sigGroups.csv", sep=",", append=TRUE)    

}# end multicomparison

pvalue <- 1
# get p-value from statistical test:
ans <- get_result_statistical_test(list_names, list_samples)
data <- as.data.frame(ans[1])
pvalue <- as.double(ans[2])
get_results_multicomparison(data, pvalue)

