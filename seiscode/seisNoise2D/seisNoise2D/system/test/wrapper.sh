#!/bin/bash

# parameters
source $PBS_O_WORKDIR/parameter

# local id (from 0 to $ntasks-1)
if [ $system == 'slurm' ]; then
    iproc=$SLURM_PROCID
elif [ $system == 'pbs' ]; then
    iproc=$PBS_VNODENUM
fi

echo "wrapper.sh iproc=$iproc"

nproc_in_task=$(echo "$iproc $ntasks" | awk '{ print $1%$2}')

if [ $nproc_in_task -eq 0 ]; then
    PBS_VNODENUM=$(echo "$iproc $ntasks" | awk '{print int($1/$2)}')
    # run serial exe
    $PBS_O_WORKDIR/test/hello.exe
else
    echo "Not running again"
fi

# run mpi exe 
#mpirun -np 1 test/hello_mpi.exe
