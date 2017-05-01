import os
import csv
import string


## create process to run this sampling

def get_total_energy():
    total_energy=0
    with open('output_log.txt') as csvfile:
        fieldnames = ['sample', 'total_J','_pkg_', '_core_', '_gpu_', 'other', '_ram_ [J]']
        reader = csv.DictReader(csvfile, fieldnames=fieldnames)
        for row in reader:
            if string.find(row['total_J'], 'total_J')==-1:
                total_energy += float(row['total_J'])
                #print(total_energy)
    return total_energy

if __name__ == '__main__':
    print("total energy for data in output_log.txt")
    #p = Process(target=sample, args=(options.interval, options.nsamples))
    #p.start()
    #run_subject_app()
    #p.join()
    #print("return total energy usage")
    print(get_total_energy())

