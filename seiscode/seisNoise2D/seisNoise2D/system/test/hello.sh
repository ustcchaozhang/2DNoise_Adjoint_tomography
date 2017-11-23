#!/bin/bash

# parameters
source $PBS_O_WORKDIR/parameter

# local id (from 0 to $ntasks-1)
if [ $system == 'slurm' ]; then
    iproc=$SLURM_PROCID
elif [ $system == 'pbs' ]; then
    iproc=$PBS_VNODENUM
fi

echo "hello.sh iproc=$iproc"

# run serial exe
$PBS_O_WORKDIR/test/hello.exe

# run mpi exe 
#mpirun -np 1 test/hello_mpi.exe
