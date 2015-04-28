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
def appendFilesToNewFile(new_file, file_r, pattern):

    file_a = open(new_file,"wr+")
    print "to write in: " + new_file
    
    if(pattern=="results"):
        #read temporal file to
        file_r2="temp.csv"
        with open(file_r2, 'rU') as csvtemp:
            with open(file_r, 'rU') as csvfile:
                tfreader = csv.reader(csvtemp, delimiter=',')
                freader =  csv.reader(csvfile, delimiter=' ')
                for row in tfreader:
                    oline = ', '.join(freader.next())
                    #print(oline)
                    nline = ', '.join(row) + oline + "\r"
                    #print(nline)
                    file_a.write(nline)

        #delete temporal file
        os.remove("temp.csv")
    else:
        print "to read from: " + file_r + "\n"
        with open(file_r,'rU') as csvfile:
            freader = csv.reader(csvfile, delimiter=' ')
            for row in freader:
                nline=', '.join(row)+", \r"
                #print(nline)
                file_a.write(nline)
    file_a.close()


def traverseDirectories(directory, new_file, type):
    targetfiles = [fd for fd in os.listdir(directory)]
    for f in targetfiles:
        name_file = join(directory,f)
        if (isdir(name_file)):
            print "traversing dir call"
            traverseDirectories(name_file, new_file, type)
        elif (isfile(name_file)) and (f.find(type)!= -1):
            print name_file
            print "going for results file"
            appendFilesToNewFile(new_file,name_file, type)

if len(args) < 2:
    parser.print_help()
    sys.exit(1)

if (args[1]):
    if not args[1]:
        print "missing name of file containing all data!"
        parser.print_help()
        sys.exit(1)

if not os.path.exists(args[0]):
    print "target directory doesn't exist"
    sys.exit(1)

# start traversing the directory structure
traverseDirectories(args[0], "temp.csv", "subjects")
traverseDirectories(args[0], args[1], "results")

