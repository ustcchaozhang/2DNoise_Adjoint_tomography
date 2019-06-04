#!/bin/bash

ulimit -s unlimited
# load module
module load intel/14.0.1
module load gcc/5.2.0
module load openmpi/1.4.4-intel-v12.1

# cd workdir
export SUBMIT_DIR=$PBS_O_WORKDIR

cd $PBS_O_WORKDIR
source parameter
echo "system" $system


export user=$(whoami)

if [ $system == 'slurm' ]; then
    # Submit directory
    export SUBMIT_DIR=$SLURM_SUBMIT_DIR
    echo "$SLURM_JOB_NODELIST"  >  ./job_info/NodeList
    echo "$SLURM_JOBID"  >  ./job_info/JobID
elif [ $system == 'pbs' ]; then
    # Submit directory
    echo "$PBS_NODEFILE"  >  ./job_info/NodeList
    echo "$PBS_JOBID"  >  ./job_info/JobID
fi


cd $SUBMIT_DIR

