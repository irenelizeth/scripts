#COMBINE ALL LIBRARIES RESULTS IN ONE FOLDER (FOR JCF, GUAVA, GOLDMAN-SACHS, JAVOLUTION, FASTUTIL, ETC)

subject_path_other=$1
subject_path_jcf=$2
lowLimit = $3
upperLimit = $4

for n in {lowLimit..upperLimit}
do
	echo "Site$n \n"
	#echo "/Users/irene/Documents/GreenProject/scripts/merge_files_dir_dir.py ${subject_path_other}site$n/ ${subject_path_jcf}site$n/ "

	python /Users/irene/Documents/GreenProject/scripts/merge_files_dir_dir.py ${subject_path_other}site$n/ ${subject_path_jcf}site$n/

done
