#!/bin/bash


echo 
echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo
echo " source parameter file ..." 
source parameter

echo
echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo
#rm -rf job_info
#mkdir job_info

echo 
echo " create result file ..."
#rm -rf RESULTS
#mkdir -p RESULTS

echo
echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo
echo

workflow_DIR="$package_path/workflow"

echo 



echo
echo " renew parameter file ..."
cp $package_path/SRC/seismo_parameters.f90 ./bin/
cp $package_path/scripts/renew_parameter.sh ./
./renew_parameter.sh


echo 
echo " complile source codes ... "

#cp $package_path/lib/make_lib  ./
#make -f make_lib clean
#make -f make_lib


rm -rf *.mod make_file
cp $package_path/make/make_$compiler ./make_file
cp $package_path/lib/constants.mod ./
FILE="make_file"
sed -e "s#^SRC_DIR=.*#SRC_DIR=$package_path/SRC#g"  $FILE > temp;  mv temp $FILE
make -f make_file clean
make -f make_file

echo "the prepared work is done"


workflow_DIR="$package_path/workflow"



if [ "$job" ==  "modeling" ] || [ "$job" ==  "Modeling" ]
then
    echo " ########################################################"
    echo " Forward modeling .." 
    echo " ########################################################"
    cp $workflow_DIR/Modeling.sh $Job_title.sh

elif [ "$job" ==  "kernel" ] || [ "$job" ==  "Kernel" ]
then
    echo " ########################################################"
    echo " Adjoint Inversion .." 
    echo " ########################################################"
    cp $workflow_DIR/Kernel.sh $Job_title.sh

elif [ "$job" ==  "inversion" ] || [ "$job" ==  "FWI" ]
then
    echo " ########################################################"
    echo " Adjoint Inversion .." 
    echo " ########################################################"
    cp $workflow_DIR/AdjointInversion.sh $Job_title.sh
else
    echo "Wrong job: $job"
fi



echo " create new job_info file ..."
rm -rf job_info
mkdir job_info

echo 
echo " create result file ..."
rm -rf RESULTS
mkdir -p RESULTS


workflow_DIR="$package_path/workflow"

echo " edit request nodes and tasks ..."
nproc=$NPROC_SPECFEM
nodes=$(echo $(echo "$NSRC $NPROC_SPECFEM  $max_nproc_per_node" | awk '{ print $1*$2/$3 }') | awk '{printf("%d\n",$0+=$0<0?0:0.999)}')
echo " Request $nodes nodes, $ntasks tasks, $nproc cpus per task "

echo
echo '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
echo

echo "submit job"
echo
if [ $system == 'slurm' ]; then
    echo "slurm system ..."
    echo "sbatch -p $queue -N $nodes -n $ntasks --cpus-per-task=$nproc -t $WallTime -e job_info/error -o job_info/output $Job_title.sh"
    sbatch -p $queue -N $nodes -n $ntasks --cpus-per-task=$nproc -t $WallTime -e job_info/error -o job_info/output $Job_title.sh

elif [ $system == 'pbs' ]; then
    echo "pbs system ..."
    echo "qsub -l nodes=$nodes:ppn=$max_nproc_per_node  -l walltime=$WallTime -e job_info/error -o job_info/output  $Job_title.sh"
    qsub -l nodes=$nodes:ppn=$max_nproc_per_node  -l walltime=$WallTime -e job_info/error -o job_info/output  $Job_title.sh

fi
echo

