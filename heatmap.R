#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

file <- args[1]
name <- args[2]

# this is a modified version of the script made by Sebastian Raschka
# url: http://sebastianraschka.com/Articles/heatmaps_in_r.html


#########################################################
### A) Installing and loading required packages
#########################################################

if (!require("gplots")) {
   suppressWarnings(install.packages("gplots", dependencies = TRUE))
   suppressWarnings(library(gplots))
   }
if (!require("RColorBrewer")) {
   suppressWarnings(install.packages("RColorBrewer", dependencies = TRUE))
   suppressWarnings(library(RColorBrewer))
   }


#########################################################
### B) Reading in data and transform it into matrix format
#########################################################

data <- read.csv(file, comment.char="#")
rnames <- data[,1]                            # assign labels in column 1 to "rnames"
mat_data <- data.matrix(data[,2:ncol(data)])  # transform column 2-5 into a matrix
rownames(mat_data) <- rnames                  # assign row names 
colnames(mat_data) <- c("10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")

#########################################################
### C) Customizing and plotting the heat map
#########################################################

# creates a own color palette from red to green
my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)

# (optional) defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-1,0,length=100),  # for red
  seq(0,0.8,length=100),              # for yellow
  seq(0.8,1,length=100))              # for green

# creates a 5 x 5 inch image
png(paste("Tutorials/heatmap_",name,".png",sep=""),    # create PNG for the heat map
  width = 5*300,        # 5 x 300 pixels
  height = 5*300,
  res = 300,            # 300 pixels per inch
  pointsize = 8)        # smaller font size

heatmap.2(mat_data, 
  cellnote = mat_data,  # same data set for cell labels
  main = paste("Results ",name,sep=""), # heat map title
  notecol="black",      # change font color of cell labels to black
  density.info="none",  # turns off density plot inside color legend
  trace="none",         # turns off trace lines inside the heat map
  margins =c(12,9),     # widens margins around plot
  col=my_palette,       # use on color palette defined earlier 
  #breaks=col_breaks,    # enable color transition at specified limits
  dendrogram="none",     # no dendrogram
  Colv="NA",            # turn off column clustering
  Rowv="NA",            # turn off row clustering
  xlab="% Selected Sites",
  ylab="% Selected Implementations")

dev.off()               # close the PNG device