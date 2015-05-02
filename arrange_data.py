## THIS SCRIPT READS A DIRECTORY CONTAINING THE DATA FOR EACH SUBJECT AND CREATES
## A SINGLE FILE CONTAINING THE ENERGY AND TIME STAMP COUNTER INFORMATION FOR ALL
## SITES AND IMPLEMENTATIONS ON EACH SUBJECT APPLICATION.

import os
import sys
import csv

from optparse import OptionParser
from os.path import isfile, isdir, join

usage = """%prog [options] <target_directory> <file_name>
    target_directory:  directory containing multiple directories with target files for which other files will be appended
    file_name: specify the name of the file that will have all the data
    """
parser = OptionParser(usage)

(options, args) = parser.parse_args()

# function to append files from a given directory to file_a
def appendDataIntoNewFile(new_file, file_r, pattern):

    path, filename = os.path.split(new_file)

    if(pattern=="results"):
        tname = os.path.abspath(path)
        aname = tname[tname.rfind("/",0,len(tname))+1:len(tname)]
        file_a = open(new_file+"_"+aname+".csv","wr+")
        
        #read temporal file
        file_r2= path+"/temp.csv"
        with open(file_r2, 'rU') as csvtemp:
            with open(file_r, 'rU') as csvfile:
                tfreader = csv.reader(csvtemp, delimiter=',')
                freader =  csv.reader(csvfile, delimiter=' ')
                for row in tfreader:
                    oline = ', '.join(freader.next())
                    nline = ', '.join(row) + oline + "\r"
                    file_a.write(nline)

        #delete temporal file
        os.remove(file_r2)
    else:
        file_a = open(new_file,"wr+")
        
        with open(file_r,'rU') as csvfile:
            freader = csv.reader(csvfile, delimiter=' ')
            for row in freader:
                nline=', '.join(row)+", \r"
                file_a.write(nline)
    file_a.close()


def traverseDirectories(directory, new_file, type):
    targetfiles = [fd for fd in os.listdir(directory)]
    for f in targetfiles:
        name_file = join(directory,f)
        if (isdir(name_file)):
            traverseDirectories(name_file, new_file, type)
        elif (isfile(name_file)) and (f.find(type)!= -1):
            new_file = join(directory,new_file)
            #print "join file output: "+ new_file +"\n"
            appendDataIntoNewFile(new_file,name_file, type)


def appendAllData(new_file, file_r):
    path, filename = os.path.split(new_file)
    file_a = open(new_file,"a+")
    
    with open(file_r,'rU') as csvfile:
        freader = csv.reader(csvfile, delimiter=' ')
        for row in freader:
            nline = ' '.join(row)+"\r"
            file_a.write(nline)
    file_a.close()



#args[1] file's name for subject unified data
def combineDataAllSites(directory, new_file):
    targetdirs = [fd for fd in os.listdir(directory)]
    for dir in targetdirs:
        name_dir = join(directory,dir)
        if(isdir(name_dir)):
            tname = os.path.abspath(name_dir)
            aname = tname[tname.rfind("/",0,len(tname))+1:len(tname)]
            #file = tname+"/unified_"+aname+".csv"
            file = tname+"/combinedFile-"+aname+".csv"
            appendAllData(new_file, file)

if len(args) < 1:
    parser.print_help()
    sys.exit(1)

if not os.path.exists(args[0]):
    print "target directory doesn't exist"
    sys.exit(1)


# start traversing the directory structure
traverseDirectories(args[0], "temp.csv", "subjects")
traverseDirectories(args[0], "unified", "results")
print "[done] creating unified data files"

combineDataAllSites(args[0], args[1])
print "[done] creating data file with all results"





