## THIS SCRIPT CREATES A CSV FILE PER SITE FOLDER CONTAINING DATA FROM TWO FILES: RESULTS AND SUBJECTS
## THE OUTPUT DATA FILE PER SITE FOLDER IS THE ENERGY USAGE RESULTS OF THE APPLICATION'S SITE WHEN USING DIFFERENT COLLECTIONS IMPLEMENTATIONS

import sys
import os
import csv

from optparse import OptionParser

Usage = """%prog [options] <folder_name>
	<folder_name>: name of the folder containing the results per site of the application's energy usage

	the structure of the folder given as input should follow the following structure:

	parent_folder/
		subfolder_0/
			results_file_0
			subjects_file_0
		subfolder_1/
			results_file_1
			subjects_file_1
		...
		subfolder_N/
			results_file_N
			subjects_file_N

"""

parser = OptionParser()

#parser.add_option("-n","--name", dest="filename",
#		help="name for the CSV file")

(options, args) = parser.parse_args()

if len(args) <  1:
	parser.print_help()
	sys.exit(1)

if (args[0]):
	if not os.path.exists(args[0]):
		print "folder doesn't exist"
		parser.print_help()
		sys.exit(1)

# function to read content on files of directory and create csv file line per line
def createCSVFile(dir_name):
	listR = []
	listS = []
	files = [f for f in os.listdir(dir_name)]

	for f in files:
		f = os.path.join(dir_name,f)
		if (os.path.isfile(f)) and (f.find("subjects") != -1):
			file_s = open(f,'r')
			for line in file_s:
				listS.append(line[:-1])		

		elif (os.path.isfile(f)) and (f.find("results")!= -1):
			file_r = open(f,'r')
			for line in file_r:
				listR.append(line[:-1])

	cSite =  os.path.basename(dir_name)

	name_file = os.path.join(dir_name,'combinedFile-'+cSite+'.csv')

	with open(name_file,'wb') as csvfile:
		csvwriter = csv.writer(csvfile)
		csvwriter.writerow(['eu','alternative'])
		for i, val in enumerate(listR):
			csvwriter.writerow([listR[i],listS[i]])


#iterate over the folder's subfolders
list_dirs = os.listdir(args[0])

for f in list_dirs:
	cfile = os.path.join(args[0],f)
	if os.path.isdir(cfile):
		#call function to create CSV file
		createCSVFile(cfile)

	
