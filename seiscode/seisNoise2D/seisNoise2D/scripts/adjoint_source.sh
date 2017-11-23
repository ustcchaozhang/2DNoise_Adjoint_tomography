#!/bin/bash
########################################### 
#export SUBMIT_DIR='/scratch/l/liuqy/zhang18/seisDD/GJI2016/Exp1/submit_job'
##########################################

isource=$1
NPROC_SPECFEM=$2
compute_adjoint=$3
data_list=$4
measurement_list=$5
misfit_type_list=$6
WORKING_DIR=$7
Wscale=$8
wavelet_path=$9

#if [ $isource -eq 1 ]; then
#    echo "adjoint source ..."
#    echo "NPROC_SPECFEM=$NPROC_SPECFEM"
#    echo "compute_adjoint=$compute_adjoint"
#    echo "data_list=$data_list"
#    echo "measurement_list=$measurement_list"
#    echo "misfit_type_list=$misfit_type_list"
#    echo "Wscale=$Wscale"
#    echo "wavelet_path=$wavelet_path"
#    echo "WORKING_DIR=$WORKING_DIR"
#fi

ISRC_WORKING_DIR=$( seq --format="$WORKING_DIR/%06.f/" $(($isource-1)) $(($isource-1)) )
mkdir -p $ISRC_WORKING_DIR
#echo "adjoint: mkdir -p $ISRC_WORKING_DIR"


cd $ISRC_WORKING_DIR

if [ $Wscale -gt 0 ]; then
    cp -r $wavelet_path ./
fi

# adjoint source
echo "run -np $NPROC_SPECFEM ./bin/misfit_adjoint.exe $compute_adjoint $data_list $measurement_list $misfit_type_list $ISRC_WORKING_DIR"

if [ "$NPROC_SPECFEM" -eq 1 ]; then
    ./bin/misfit_adjoint.exe $compute_adjoint $data_list $measurement_list $misfit_type_list $ISRC_WORKING_DIR
else
    mpirun -np $NPROC_SPECFEM ./bin/misfit_adjoint.exe $compute_adjoint $data_list $measurement_list $misfit_type_list $ISRC_WORKING_DIR
fi



## copy and postprocessing of adjoint source
arr=$(echo $data_list | tr "," "\n")

for x in $arr

do
    if [ -f "SU_process/process_adj.sh" ]; then
        
        echo "SU_process/process_adj.sh"
        sh SU_process/process_adj.sh \
            SEM/U${x}_file_single.su.adj \
            SEM/U${x}_file_single.su.adj_proc
        mv SEM/U${x}_file_single.su.adj_proc SEM/U${x}_file_single.su.adj        
    fi
done
