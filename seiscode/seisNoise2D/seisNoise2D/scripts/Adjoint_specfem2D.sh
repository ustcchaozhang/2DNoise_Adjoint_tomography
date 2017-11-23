#!/bin/bash

isource=$1
NPROC_SPECFEM=$2
data_type=$3
velocity_dir=$4
SAVE_FORWARD=$5
WORKING_DIR=$6

#if [ $isource -eq 1 ] ; then
#    echo "SPECFEM2D Adjoint Modeling ..."
#    echo "NPROC_SPECFEM=$NPROC_SPECFEM"
#    echo "data_type=$data_type"
#    echo "velocity_dir=$velocity_dir"
#    echo "SAVE_FORWARD=$SAVE_FORWARD"
#    echo "WORKING_DIR=$WORKING_DIR"
#fi


ISRC_WORKING_DIR=$( seq --format="$WORKING_DIR/%06.f/" $(($isource-1)) $(($isource-1)) ) # working directory (on local nodes, where specfem runs)
ISRC_DATA_DIR=$ISRC_WORKING_DIR/$data_type

mkdir -p $ISRC_WORKING_DIR $ISRC_DATA_DIR

cd $ISRC_WORKING_DIR

##### edit 'Par_file' #####
FILE="./DATA/Par_file"
sed -e "s#^SIMULATION_TYPE.*#SIMULATION_TYPE = 3 #g"  $FILE > temp; mv temp $FILE
sed -e "s#^SAVE_FORWARD.*#SAVE_FORWARD = .$SAVE_FORWARD. #g"  $FILE > temp; mv temp $FILE

##### forward simulation (data) #####
./bin/xmeshfem2D > OUTPUT_FILES/output_mesher.txt

if [ "$NPROC_SPECFEM" -eq 1 ]; then
    ./bin/xspecfem2D > OUTPUT_FILES/output_adjoint.txt
else
    mpirun -np $NPROC_SPECFEM ./bin/xspecfem2D > OUTPUT_FILES/output_adjoint.txt
fi


#### mask source 
# Source location
export xs=$(awk -v "line=$isource" 'NR==line { print $1 }' DATA/sources.dat)
export zs=$(awk -v "line=$isource" 'NR==line { print $2 }' DATA/sources.dat) 

./bin/mask_func.exe $xs $zs DATA/ OUTPUT_FILES/ 

