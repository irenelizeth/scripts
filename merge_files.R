#!/usr/bin/env Rscript

args <- commandArgs(TRUE)

file1= args[1]
file2= args[2]
file_name= args[3]

#read data files
data1 = read.csv(file1, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="", header=FALSE)
data2 = read.csv(file2, stringsAsFactors=FALSE, skipNul=TRUE, na.strings="", header=FALSE)

#merge data
merged.data <- merge(data1, data2, by="V1", all.x=TRUE, all.y=TRUE)

#write merged data to file 'file_name'
write.table(merged.data, file=file_name, append=FALSE, quote=FALSE, na="", row.names=FALSE, col.names=FALSE, sep=",")