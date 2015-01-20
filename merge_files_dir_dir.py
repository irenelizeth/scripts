## THIS SCRIPT TAKES TWO FILES IN A DIRECTORY WITH NAMES 'SUBJECTS' AND 'RESULTS' RESPECTIVELY AND
## APPEND THEIR CONTENT TO FILES IN OTHER DIRECTORIES THAT CONTAIN FILES WITH THE SAME NAMES
## THe GOAL  IS TO MERGE THE RESULTS FROM ONE DIRECTORY TO MULTIPLE DIRECTORIES THAT CONTAIN FILES 
## WITH SIMILAR INFORMATION

import os
import sys

from optparse import OptionParser
from os.path import isfile, isdir, join

usage = """%prog [options] <target_directory> <directory>
    target_directory:  directory containing multiple directories with target files for which other files will be appended
    Directory: specify the folder where data files are located and will be appended to target files
"""

parser = OptionParser(usage)

(options, args) = parser.parse_args()

# function to append files from a given directory to file_a
def appendFilesInDirectory(directory, file_a, pattern):
    depfiles = [fd for fd in os.listdir(directory)]
    for f in depfiles:
        name_file = join(directory,f)
        print name_file
        if (isfile(name_file)) and (f.find(pattern)!= -1):
            print name_file
            file_r = open(name_file,"r")
            #file_a.write('\n')
            file_a.write(file_r.read())	
        elif isdir(name_file):
            appendFilesInDirectory(name_file,file_a, pattern)

def traverseDirectories(directory):
    targetfiles = [fd for fd in os.listdir(directory)]
    for f in targetfiles:
        name_file = join(directory,f)
        print name_file
        
        if (isdir(name_file)):
            print "traversing dir call"
            traverseDirectories(name_file)
        elif (isfile(name_file)) and (f.find("results")!= -1):
            file_results = open(name_file,"a")
            print "going for results file"
            appendFilesInDirectory(args[1],file_results, "results")
        elif (isfile(name_file)) and (f.find("subjects")!= -1):
            file_sub = open(name_file, "a")
            print "going for subjects file"
            appendFilesInDirectory(args[1],file_sub, "subjects")

if len(args) < 2:
	parser.print_help()
	sys.exit(1)

if (args[1]):
	if not os.path.exists(args[1]):
		print "directory of additional file to append does not exist!"
		parser.print_help()
		sys.exit(1)

if not os.path.exists(args[0]):
	print "target directory doesn't exist"
	sys.exit(1)

# start traversing the directory structure

traverseDirectories(args[0]) 
