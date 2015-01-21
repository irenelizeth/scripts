import os
import subprocess
import sys
import difflib
import re
from os import listdir

from optparse import OptionParser

usage = """%prog [options] original testcase alternatives stringToMatch
    original: path to original jar file
    testcase: name of the test case to run
    alternatives: path to the directory where the alternative jar files for the program are located 
    stringToMatch: string to match the original test suite result, the files that do not contain this string are different (have failures or errors)
"""

parser = OptionParser(usage)
parser.add_option('-o', '--output-directory',
        action='store', dest='directory',
        default=os.getcwd(), help='specify directory to store outputs')

parser.add_option('-d', '--dependencies-directory',
        action='store', dest='dep_directory',
        default=False, help='specify the folder where library dependencies are located')

parser.add_option('-c','--classpath',
	action='store', dest='cp_dir',
	default=False, help='classpath directories')

(options, args) = parser.parse_args()


def getFilesDifferentFromOriginal(directory, list_files):
	f1 = os.path.join(directory, "tmpfile1.out")
	f2 = os.path.join(directory, "tmpfile2.out")

	with open(f1, "w") as tmpf1:
		subprocess.call(['tail','-n+4',directory+'original.out'], stdout=tmpf1)

	setFiles = set()
	for file in list_files:
		with open(f2, "w") as tmpf2:
			subprocess.call(['tail','-n+4',directory+file], stdout=tmpf2)
		
		res = subprocess.call(['diff',directory+'tmpfile1.out',directory+'tmpfile2.out'])

		if res == 1:
			setFiles.add(file)
	
	#os.remove(f1)
	#os.remove(f2)
	return setFiles

def getFilesContainingString(directory, list_files, string_compare):
	setFiles = set()
	for file in list_files:
		if string_compare in open(directory+file).read():
			setFiles.add(file)
	return setFiles

def getFilesNotContainingString(directory, list_files, string_compare):
	setFiles = set()
	for file in list_files:
		if not string_compare in open(directory+file).read():
			setFiles.add(file)
	return setFiles

def printEnumeratedList(list_elem):
	count = 0
	for elem in list_elem:
		count += 1
		print str(count)+") " +elem
	return

if len(args) < 3:
	parser.print_help()
	sys.exit(1)

if not os.path.exists(options.directory):
	os.mkdir(options.directory)

if options.dep_directory:
	if not os.path.exists(options.dep_directory):
		print "dependencies directory does not exist!"
		parser.print_help()
		sys.exit(1)

if options.cp_dir:
	if not os.path.exists(options.cp_dir):
		print "classpath directory doesn't exist: "
		print options.cp_dir
		parser.print_help()
		sys.exit(1)

classpath = ":".join(["/usr/share/java/junit4.jar"])

if options.dep_directory:
	#print options.dep_directory
	from os.path import isfile, join
	depfiles = [ fd for fd in os.listdir(options.dep_directory) if isfile(join(options.dep_directory,fd)) ]
	# add the dependency libraries path to the classpath
	for f in depfiles:
		classpath=classpath+":"+options.dep_directory+f
		
# run original jar

outputOriginal = os.path.join(options.directory, "original.out")

with open(outputOriginal, "w") as f:

	if options.cp_dir:
		subprocess.call(["java", "-cp", classpath+":"+args[0]+":"+options.cp_dir, "org.junit.runner.JUnitCore", args[1]],stdout=f)
	else:
		subprocess.call(["java", "-cp", classpath+":"+args[0], "org.junit.runner.JUnitCore", args[1]],stdout=f)

# run alternatives jar files

countfiles = 0;

#print classpath

for file in os.listdir(args[2]):
	countfiles+=1
	output = os.path.join(options.directory, os.path.splitext(file)[0] + ".out")

	with open(output, "w") as f:
		
		if options.cp_dir:
			subprocess.call(["java", "-cp", classpath +":" + args[2] + file+":"+options.cp_dir , "org.junit.runner.JUnitCore", args[1]],stdout=f)
		else:
			#print classpath+":"+args[2]+file
			subprocess.call(["java", "-cp", classpath +":" + args[2] + file , "org.junit.runner.JUnitCore", args[1]],stdout=f)

print "total number of alternative jar files: %d"% countfiles	

name_diff_files = set()

# compare the test results between the original jar file and the alternatives files
name_diff_files = getFilesDifferentFromOriginal(options.directory, os.listdir(options.directory))

print "\nfiles that differ in the test results:\n"
count = 0
printEnumeratedList(name_diff_files)

# iterate over the files in the set to identify those that have the same number of failures or ok tests than original

name_similar_files = getFilesContainingString(options.directory, name_diff_files, args[3])

print "\nfiles for revision:\n"
printEnumeratedList(name_similar_files)

# show files that do not match original test results and have failures:
discard_files = name_diff_files - name_similar_files

print "\nfiles discarded:\n"
printEnumeratedList(discard_files)

for file in discarded_files:
	file_name = args[2]+file[0:-3]+"jar"
	if os.path.exists(file_name):
		subprocess.call(["rm", file_name])



