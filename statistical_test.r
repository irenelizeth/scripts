#!/Library/Frameworks/R.framework/Resources/bin/Rscript 

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
    
    cat("\n\n Results from pairwise Wilcoxon test:\n\n")
    print(PT)
    PT$p.value
    PT <- PT$p.value
    #print(PT)
    PT1 <- fullPTable(PT)
    
    res <- multcompLetters(PT1, compare="<", threshold=alpha, Letters=letters, reversed=FALSE)
    print(res)
    
    #print(res)
    ## groups sharing same character identify groups that are not significantly different
    
    dimMg <- dim(res$LetterMatrix)
    ## if only one column then all groups share same character
    if(dimMg[2]==1){
        cat("\nGroups are not significantly different from each other! \n")
        
    }else if(dimMg[1]>2){ # more than two groups to compare
        cat("\nGroups are significantly different:")
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

    suppressWarnings(suppressMessages(library(sets)))
    suppressWarnings(suppressMessages(library(dunn.test)))
    

    cat("\n\nResults from Dunn Test for multicomparison of groups:\n")
    setDiffGroups <- set()
    
    # from package FSA
    #PTdt <- dunnTest(data$Value, data$Group, method=adjust.method, two.sided=FALSE)
    #print(PTdt)
    #PTdt <- PTdt$res
    #rdt <- cldList(comparison=PTdt$Comparison, p.value = PTdt$P.adj, threshold=alpha)
    
    # from dunn.test package
    PT2 <- dunn.test(data$Value, data$Group, method=adjust.method)
    res <- data.frame(PT2$P, PT2$P.adjusted, PT2$comparisons, stringsAsFactors=FALSE)
    colnames(res) <- c("P", "P.adj", "test")
    
    # TO-DO: check process to accept test based on p.adjusted values
    # res <- res[order(P),] #sort results based on p.raw.value ?
    # find largest p.adjusted value for which p.adj < Q ?
    # accept as significant all tests below largest.p.adjusted < Q ?
    
    for(k in 1:nrow(res)){
        if(adjust.method=="BH" && res$P.adj[k] < q)
            #cat(res$test[k])
            setDiffGroups <- setDiffGroups + set(res$test[k])
        else if (res$P.adj[k] < 0.05)
            setDiffGroups <- setDiffGroups + set(res$test[k])
            
    }

    #TO-DO: sort p.adj-values, find largest p-value that is less than (i/m)*Q, where i is the rank,
    # m is the total number of comparisons, and Q is the choosen false discovery rate.
    
    # BH --> p.adjusted value = p.raw value * (m/i)
    # sort results by p.unadj or p.raw values (to consider rank position)
    # find largest p.adjusted value for which p.adj < Q
    # (test (comparison of groups) is significant if p.adjusted value < Q (false discovery rate))
    
    #groups <- as.character(rdt$Group)
    #groupLetters <- as.character(rdt$Letter)
    
    #for(k in 2:length(groups)){
    #    if(!(identical(groupLetters[k], groupLetters[k-1]))){
    #        if(!set_contains_element(setDiffGroups, groupLetters[k])){
    #            setDiffGroups <- setDiffGroups + set(groups[k])
    #        }
    #
    #        if(!set_contains_element(setDiffGroups, groupLetters[k-1])){
    #            setDiffGroups <- setDiffGroups + set(groups[k-1])
    #        }
    #    }
    #
    #}
    
    return(setDiffGroups)
    cat("End Dunn Test Results\n\n")
}


# perform pot-hoc analysis: multicomparison between groups of data
get_results_multicomparison <- function(data, pvalue){

    #adjust.method = "BH"
    adjust.method = "BH"
    # false discovery rate for BH method:
    q = 0.10
    
    #if p-value < alpha then perform multicomparison test:
    if(pvalue < alpha){
    
        #cat(paste("Significant difference found, p-value: ", pvalue, sep=""))
        cat(pvalue)
        print(compute_dunntest(data, adjust.method, q))

    }else{
        cat(paste("\n NO significant difference found, p-value ", pvalue,"\n", sep=""))
    }
}# end multicomparison

pvalue <- 1
# get p-value from statistical test:
ans <- get_result_statistical_test(list_names, list_samples)
data <- as.data.frame(ans[1])
pvalue <- as.double(ans[2])
get_results_multicomparison(data, pvalue)

