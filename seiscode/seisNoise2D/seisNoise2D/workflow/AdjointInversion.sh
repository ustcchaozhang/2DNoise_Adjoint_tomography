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

################### input parameters ###################################################
# directories
export SCRIPTS_DIR="$package_path/scripts"
export SUBMIT_RESULT="$SUBMIT_DIR/RESULTS/$job/Scale${Wscale}_${measurement_list}_${misfit_type_list}"     # final results
if [ -z "$working_path" ]; then
    export working_path=$SUBMIT_DIR
fi
export WORKING_DIR="$working_path/$Job_title/"  # directory on local nodes, where specfem runs

echo "Submit job << $Job_title >> in : $SUBMIT_DIR  "
echo "Working directory: $WORKING_DIR"
echo "FINAL results in :  $SUBMIT_RESULT"
#########################################################################################
STARTTIME=$(date +%s)
echo "start time is :  $(date +"%T")"


if $ReStart; then
    echo
    echo "Re-Starting job ..." 
    echo "Clean up result directories ..."
    rm -rf $SUBMIT_RESULT $WORKING_DIR
    mkdir -p $SUBMIT_RESULT $WORKING_DIR

    echo "prepare starting model ..."
    cp -r $initial_velocity_dir    $SUBMIT_RESULT/m_current
else
    echo
    echo "Continue with current job ..."
    mkdir -p $SUBMIT_RESULT $WORKING_DIR
fi

echo
echo "********************************************************************************************************"
echo "       Welcome job << $job >> " 
echo "       Scale: '$Wscale'; measurement: '${measurement_list}'; misfit_type: '${misfit_type_list}' " 
echo "********************************************************************************************************"
echo

echo "prepare data ..."
velocity_dir=$target_velocity_dir

if [ $system == 'slurm' ]; then
    srun -n $ntasks -c $NPROC_SPECFEM -l -W 0 $SCRIPTS_DIR/prepare_data.sh $velocity_dir 2> ./job_info/error_target
elif [ $system == 'pbs' ]; then
    echo "sh pbsssh.sh  $SCRIPTS_DIR/prepare_data.sh $velocity_dir $SCRIPTS_DIR  $WORKING_DIR"
    sh pbsssh.sh  $SCRIPTS_DIR/prepare_data.sh $velocity_dir $SUBMIT_DIR $SCRIPTS_DIR $SUBMIT_RESULT $WORKING_DIR 2> ./job_info/error_target



fi




if [ -d "$SUBMIT_RESULT/m_$(($iter_start-1))" ]; then
    rm -rf $SUBMIT_RESULT/m_$(($iter_start-1))
fi
cp -r $SUBMIT_RESULT/m_current   $SUBMIT_RESULT/m_$(($iter_start-1))

# iteration loop
for (( iter=$iter_start;iter<=$iter_end;iter++ ))
do
    smooth_x=$(echo $(echo "$sigma_x $iter $sigma_x_rate" | awk '{ print $1-($2-1)*$3 }'))
    smooth_z=$(echo $(echo "$sigma_z $iter $sigma_z_rate" | awk '{ print $1-($2-1)*$3 }'))
    echo "          ***************************************"
    echo "          ** iteration $iter **"
    echo "          smooth_x=${smooth_x} smooth_z=${smooth_z} "
    echo "          ***************************************"

    echo "Forward/Adjoint simulation for current model ...... "
    velocity_dir=$SUBMIT_RESULT/m_current
    compute_adjoint=true

if [ $system == 'slurm' ]; then
    srun -n $ntasks -c $NPROC_SPECFEM -l -W 0 $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint 2> ./job_info/error_current
elif [ $system == 'pbs' ]; then
    echo "sh pbsssh.sh  $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint"
    sh pbsssh.sh  $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint $SUBMIT_DIR $SCRIPTS_DIR $SUBMIT_RESULT $WORKING_DIR 2> ./job_info/error_current
fi




    echo
    echo "data misfit ...... "
    mkdir -p $SUBMIT_RESULT/misfit
    step_length=0.0



    echo "./bin/data_misfit.exe $iter $step_length $compute_adjoint $NPROC_SPECFEM $WORKING_DIR $SUBMIT_RESULT"
    ./bin/data_misfit.exe $iter $step_length $compute_adjoint $NPROC_SPECFEM $WORKING_DIR $SUBMIT_RESULT 2> ./job_info/error_data_misfit_$iter



    file=$SUBMIT_RESULT/misfit/search_status.dat
    is_cont=$(awk -v "line=1" 'NR==line { print $1 }' $file)
    is_done=$(awk -v "line=2" 'NR==line { print $1 }' $file)
    is_brak=$(awk -v "line=3" 'NR==line { print $1 }' $file)
    step_length=$(awk -v "line=4" 'NR==line { print $1 }' $file)
    optimal_step_length=$(awk -v "line=5" 'NR==line { print $1 }' $file)
    echo "is_cont=$is_cont; is_done=$is_done; is_brak=$is_brak"
    if [ $is_brak -eq 1 ]; then
        echo "stop due to close zero data misfit!"
        break
    fi

    echo 
    echo "misfit kernel ...... "
    mkdir -p $SUBMIT_RESULT/misfit_kernel
    # prepare necessary files for kernel sum and smoothing
    if [ $solver == 'specfem2D' ]; then
        cp -r $SUBMIT_RESULT/m_current/proc*_NSPEC_ibool.bin $SUBMIT_RESULT/misfit_kernel/
        cp -r $SUBMIT_RESULT/m_current/proc*_jacobian.bin $SUBMIT_RESULT/misfit_kernel/ 
        cp -r $SUBMIT_RESULT/m_current/proc*_x.bin $SUBMIT_RESULT/misfit_kernel/                    
        cp -r $SUBMIT_RESULT/m_current/proc*_z.bin $SUBMIT_RESULT/misfit_kernel/
    elif [ $solver == 'specfem3D' ]; then
        rm -rf OUTPUT_FILES    
        mkdir OUTPUT_FILES
        mkdir OUTPUT_FILES/DATABASES_MPI
        cp $SUBMIT_RESULT/misfit_kernel/proc*external_mesh.bin OUTPUT_FILES/DATABASES_MPI/
    fi
    echo "mpirun -np $NPROC_SPECFEM ./bin/sum_kernel.exe $kernel_list,$precond_name $WORKING_DIR $SUBMIT_RESULT 2> ./job_info/error_misfit_kernel_$iter"
    mpirun -np $NPROC_SPECFEM ./bin/sum_kernel.exe $kernel_list,$precond_name $WORKING_DIR $SUBMIT_RESULT 2> ./job_info/error_misfit_kernel_$iter

    if $smooth ; then
        echo 
        echo "smooth misfit kernel ... "
        echo "mpirun -np $NPROC_SPECFEM ./bin/xsmooth_sem $smooth_x $smooth_z $z_precond $kernel_list,$precond_name  $SUBMIT_RESULT/misfit_kernel/ $SUBMIT_RESULT/misfit_kernel/ $GPU_MODE"
        mpirun -np $NPROC_SPECFEM ./bin/xsmooth_sem $smooth_x $smooth_z $z_precond $kernel_list,$precond_name  $SUBMIT_RESULT/misfit_kernel/ $SUBMIT_RESULT/misfit_kernel/ $GPU_MODE 2> ./job_info/error_smooth_kernel_$iter
    fi

    echo 
    echo "optimization --> gradient/direction ... "
    mkdir -p $SUBMIT_RESULT/optimizer



    ####################################################################
    echo "./bin/optimization.exe $NPROC_SPECFEM $SUBMIT_RESULT $kernel_list $model_list $iter 2> ./job_info/error_optimizer_$iter"
    ./bin/optimization.exe $NPROC_SPECFEM $SUBMIT_RESULT $kernel_list $model_list $iter 2> ./job_info/error_optimizer_$iter
 
    echo
    echo 'line search along the update direction ......'
    # linearsearch_direction=0: stop all iterations without saving
    # linearsearch_direction=1: search by halving current step length: backward
    # linearsearch_direction=2: search by doubling current step length: forward
    # linearsearch_direction=3: stop searching for current iteration wth saving
    # linearsearch_direction=4: stop searching for current iteration without saving
    ### 'step_loop': successful iteration steps

    # line search
    while [ $is_done -eq 0 -a $is_brak -eq 0 -a $is_cont -eq 1 ]
    do

        echo 
        echo "update model ... "
        mkdir -p $SUBMIT_RESULT/m_try

       
        echo "./bin/model_update.exe $NPROC_SPECFEM $SUBMIT_RESULT $model_list $step_length "
        ./bin/model_update.exe $NPROC_SPECFEM $SUBMIT_RESULT $model_list $step_length  2> ./job_info/error_update_model_$iter

        echo
        echo "Forward simulation for update model ...... "
        velocity_dir=$SUBMIT_RESULT/m_try
        compute_adjoint=false
        if [ $system == 'slurm' ]; then
            srun -n $ntasks -c $NPROC_SPECFEM -l -W 0 $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint 2> ./job_info/error_update_$iter
        elif [ $system == 'pbs' ]; then
            echo "sh pbsssh.sh  $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint $SUBMIT_DIR $SCRIPTS_DIR $SUBMIT_RESULT $WORKING_DIR 2"
            sh pbsssh.sh  $SCRIPTS_DIR/Adjoint.sh $velocity_dir $compute_adjoint $SUBMIT_DIR $SCRIPTS_DIR $SUBMIT_RESULT $WORKING_DIR 2> ./job_info/error_update_$iter
 
        fi

        echo
        echo "data misfit ...... "
        mkdir -p $SUBMIT_RESULT/misfit

        echo "./bin/data_misfit.exe $iter $step_length $compute_adjoint $NPROC_SPECFEM $WORKING_DIR $SUBMIT_RESULT"
        ./bin/data_misfit.exe $iter $step_length $compute_adjoint $NPROC_SPECFEM $WORKING_DIR $SUBMIT_RESULT 2> ./job_info/error_data_misfit

        file=$SUBMIT_RESULT/misfit/search_status.dat
        is_cont=$(awk -v "line=1" 'NR==line { print $1 }' $file)
        is_done=$(awk -v "line=2" 'NR==line { print $1 }' $file)
        is_brak=$(awk -v "line=3" 'NR==line { print $1 }' $file)
        step_length=$(awk -v "line=4" 'NR==line { print $1 }' $file)
        optimal_step_length=$(awk -v "line=5" 'NR==line { print $1 }' $file)
        echo " is_cont=$is_cont; is_done=$is_done; is_brak=$is_brak"

        if [ $is_done -eq 1 ]; then
            echo 
            echo "final model optimal_step_length=$optimal_step_length"
            echo "./bin/model_update.exe $NPROC_SPECFEM $SUBMIT_RESULT $model_list $optimal_step_length " 
            ./bin/model_update.exe $NPROC_SPECFEM $SUBMIT_RESULT $model_list $optimal_step_length   2> ./job_info/error_update_model
            cp $SUBMIT_RESULT/m_try/proc* $SUBMIT_RESULT/m_current/
            cp -r  $SUBMIT_RESULT/m_current   $SUBMIT_RESULT/m_$iter
            break 
        fi

        if [ $is_brak -eq 1 ]; then
            echo "stop due to failure of data misfit reduction!"
            break
        fi

    done  # end of line search

    if [ $is_brak -eq 1 ]; then
        echo 
        echo "Terminate all iterations in $job"
        break
    fi

    echo 
    echo "prepare for next iteration ..."
    cp -r  $SUBMIT_RESULT/optimizer/g_new.bin $SUBMIT_RESULT/optimizer/g_old.bin
    cp -r  $SUBMIT_RESULT/optimizer/p_new.bin $SUBMIT_RESULT/optimizer/p_old.bin
    cp -r  $SUBMIT_RESULT/optimizer/m_new.bin $SUBMIT_RESULT/optimizer/m_old.bin

    echo 
    echo "******************finish iteration $iter for ${misfit_type_list} $job ************"
done  # end of iterative updates

echo
echo "******************finish all iterations for ${misfit_type_list} $job *************"

cp -r $SUBMIT_DIR/parameter $SUBMIT_RESULT/
cp -r $SUBMIT_DIR/job_info/output $SUBMIT_RESULT/

echo
echo " clean up local nodes (wait) ...... "
if ! $DISPLAY_DETAILS ; then
    rm -rf $working_path/$Job_title
    rm -rf OUTPUT_FILES
fi

ENDTIME=$(date +%s)
Ttaken=$(($ENDTIME - $STARTTIME))
echo
echo "finish time is : $(date +"%T")" 
echo "RUNTIME is :  $(($Ttaken / 3600)) hours ::  $(($(($Ttaken%3600))/60)) minutes  :: $(($Ttaken % 60)) seconds."

echo
echo "******************well done*******************************"

cp -r $SUBMIT_DIR/job_info/output $SUBMIT_RESULT/
