#!/bin/bash

USAGE="sudo ./run_rapl_experiment.sh"

USER= # your user
SLEEP=120 # number of seconds
INIT_DATE=`date "+%m%d%y_%H%M%S"`

LOG_BASE=$HOME/rapl_experiments_logs
if [ ! -d "$LOG_BASE" ]; then
    echo "creating log folder..."
    (su $USER -c "mkdir $LOG_BASE")
fi
echo creating log...
(su $USER -c "touch $LOG_BASE/experiment_$INIT_DATE.txt")

LOG="tee -a $LOG_BASE/experiment_$INIT_DATE.txt"

function perf_paranoid() {
    echo 0 > /proc/sys/kernel/perf_event_paranoid
}

function disable_network() {
    echo disabling networking...
    ifdown eth0    
    sleep $SLEEP 
}

function enable_network() {
    echo enabling networking...
    ifup eth0
}

function run_ga() {
    echo running experiment...
    cd $HOME/Documents/Projects/ga_strategy
    time (su $USER -c ./run_ga_experiments.sh) 
}

function notify() {
    SUBJECT="Done with experiment $INIT_DATE"
    
    # Email To ?
    EMAIL=""
    
    # Email text/message
    EMAILMESSAGE=$LOG_BASE/experiment_$INIT_DATE.txt
    
    # send an email using mail.py
    ./mail.py "'$SUBJECT'" "$EMAIL" $EMAILMESSAGE
}

# init
echo "Starting experiments on $INIT_DATE..." | $LOG
perf_paranoid | $LOG
disable_network | $LOG

# running experiment
run_ga 2>&1 | $LOG

# cleanup
enable_network | $LOG
echo "Done with experiment on `date`..." | $LOG
notify
