#!/bin/bash

# pass parameter 
source parameter

# directory
currentdir=`pwd`
EXE_DIR="$currentdir/bin"        # exacutable files directory

############################# parameter files ############################################################### 
FILE="$EXE_DIR/seismo_parameters.f90"
sed -e "s#^Job_title=.*#Job_title=$Job_title #g"  $FILE > temp;  mv temp $FILE

### solver related parameters
if [ ! -z "$solver" ]; then
    sed -e "s#^CHARACTER (LEN=20) :: solver=.*#CHARACTER (LEN=20) :: solver='$solver'#g"  $FILE > temp;  mv temp $FILE
fi
if [ "$solver" = "specfem2D" ]; then 
    sed -e "s#^INTEGER, PARAMETER :: NGLLY.*#INTEGER, PARAMETER :: NGLLY=1 #g"  $FILE > temp;  mv temp $FILE
    sed -e "s#^CHARACTER (LEN=MAX_STRING_LEN) :: LOCAL_PATH.*#CHARACTER (LEN=MAX_STRING_LEN) :: LOCAL_PATH='OUTPUT_FILES'  #g"  $FILE > temp;  mv temp $FILE
    sed -e "s#^CHARACTER (LEN=MAX_STRING_LEN) :: IBOOL_NAME.*#CHARACTER (LEN=MAX_STRING_LEN) :: IBOOL_NAME='NSPEC_ibool.bin'  #g"  $FILE > temp;  mv temp $FILE
fi
if [ "$solver" = "specfem3D" ]; then
    sed -e "s#^INTEGER, PARAMETER :: NGLLY.*#INTEGER, PARAMETER :: NGLLY=5 #g"  $FILE > temp;  mv temp $FILE
    sed -e "s#^CHARACTER (LEN=MAX_STRING_LEN) :: LOCAL_PATH.*#CHARACTER (LEN=MAX_STRING_LEN) :: LOCAL_PATH='OUTPUT_FILES/DATABASES_MPI'  #g"  $FILE > temp;  mv temp $FILE
    sed -e "s#^CHARACTER (LEN=MAX_STRING_LEN) :: IBOOL_NAME.*#CHARACTER (LEN=MAX_STRING_LEN) :: IBOOL_NAME='external_mesh.bin'  #g"  $FILE > temp;  mv temp $FILE
fi

### FORWARD MODELNG INFO 
if [ ! -z "$NSTEP" ]; then
    sed -e "s#^INTEGER, PARAMETER :: NSTEP=.*#INTEGER, PARAMETER :: NSTEP=$NSTEP #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$deltat" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: deltat=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: deltat=$deltat #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$t0" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: t0=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: t0=$t0 #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$f0" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: f0=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: f0=$f0 #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$NREC" ]; then
    sed -e "s#^INTEGER, PARAMETER :: NREC=.*#INTEGER, PARAMETER :: NREC=$NREC #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$NSRC" ]; then
    sed -e "s#^INTEGER, PARAMETER :: NSRC=.*#INTEGER, PARAMETER :: NSRC=$NSRC #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$seismotype" ]; then
    sed -e "s#^CHARACTER (LEN=20) :: seismotype=.*#CHARACTER (LEN=20) :: seismotype='$seismotype'#g"  $FILE > temp;  mv temp $FILE
fi

## PRE-PROCESSING
# wavelet
if [ ! -z "$Wscale" ]; then
    sed -e "s#^INTEGER, PARAMETER :: Wscale=.*#INTEGER, PARAMETER :: Wscale=$Wscale #g"  $FILE > temp;  mv temp $FILE
fi
# window
if [ ! -z "$TIME_WINDOW" ]; then
    sed -e "s#^LOGICAL :: TIME_WINDOW=.*#LOGICAL :: TIME_WINDOW=.$TIME_WINDOW.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$window_type" ]; then
    sed -e "s#^INTEGER, PARAMETER :: window_type=.*#INTEGER, PARAMETER :: window_type=$window_type#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$T0_TOP" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: T0_TOP=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: T0_TOP=${T0_TOP} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$T0_BOT" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: T0_BOT=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: T0_BOT=${T0_BOT} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$VEL_TOP" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: VEL_TOP=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: VEL_TOP=${VEL_TOP} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$VEL_BOT" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: VEL_BOT=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: VEL_BOT=${VEL_BOT} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$uncertainty" ]; then
    sed -e "s#^LOGICAL :: uncertainty=.*#LOGICAL :: uncertainty=.$uncertainty.#g"  $FILE > temp;  mv temp $FILE
fi




# bandpass
if [ ! -z "$IS_BANDPASS" ]; then
    sed -e "s#^LOGICAL :: IS_BANDPASS=.*#LOGICAL :: IS_BANDPASS=.$IS_BANDPASS.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin1" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin1=.*#REAL(KIND=8), PARAMETER :: Fmin1=${Fmin1} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax1" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax1=.*#REAL(KIND=8), PARAMETER :: Fmax1=${Fmax1} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin2" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin2=.*#REAL(KIND=8), PARAMETER :: Fmin2=${Fmin2} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax2" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax2=.*#REAL(KIND=8), PARAMETER :: Fmax2=${Fmax2} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin3" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin3=.*#REAL(KIND=8), PARAMETER :: Fmin3=${Fmin3} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax3" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax3=.*#REAL(KIND=8), PARAMETER :: Fmax3=${Fmax3} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin4" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin4=.*#REAL(KIND=8), PARAMETER :: Fmin4=${Fmin4} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax4" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax4=.*#REAL(KIND=8), PARAMETER :: Fmax4=${Fmax4} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin5" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin5=.*#REAL(KIND=8), PARAMETER :: Fmin5=${Fmin5} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax5" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax5=.*#REAL(KIND=8), PARAMETER :: Fmax5=${Fmax5} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmin6" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin6=.*#REAL(KIND=8), PARAMETER :: Fmin6=${Fmin6} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax6" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax6=.*#REAL(KIND=8), PARAMETER :: Fmax6=${Fmax6} #g"  $FILE > temp;  mv temp $FILE
fi

if [ ! -z "$Fmin7" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmin7=.*#REAL(KIND=8), PARAMETER :: Fmin7=${Fmin7} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$Fmax7" ]; then
    sed -e "s#^REAL(KIND=8), PARAMETER :: Fmax7=.*#REAL(KIND=8), PARAMETER :: Fmax7=${Fmax7} #g"  $FILE > temp;  mv temp $FILE
fi


# damping
if [ ! -z "$DAMPING" ]; then
    sed -e "s#^LOGICAL :: DAMPING=.*#LOGICAL :: DAMPING=.$DAMPING.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$X_decay" ]; then

    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: X_decay=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: X_decay=${X_decay} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$T_decay" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: T_decay=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: T_decay=${T_decay} #g"  $FILE > temp;  mv temp $FILE
fi
# mute
if [ ! -z "${MUTE_NEAR}" ]; then
    sed -e "s#^LOGICAL :: MUTE_NEAR=.*#LOGICAL :: MUTE_NEAR=.${MUTE_NEAR}.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$offset_near" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: offset_near=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: offset_near=${offset_near} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "${MUTE_FAR}" ]; then
    sed -e "s#^LOGICAL :: MUTE_FAR=.*#LOGICAL :: MUTE_FAR=.${MUTE_FAR}.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$offset_far" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: offset_far=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: offset_far=${offset_far} #g"  $FILE > temp;  mv temp $FILE
fi
# event normalize
if [ ! -z "$EVENT_NORMALIZE" ]; then
    sed -e "s#^LOGICAL :: EVENT_NORMALIZE=.*#LOGICAL :: EVENT_NORMALIZE=.$EVENT_NORMALIZE.#g"  $FILE > temp;  mv temp $FILE
fi
# trace normalize
if [ ! -z "$TRACE_NORMALIZE" ]; then        
    sed -e "s#^LOGICAL :: TRACE_NORMALIZE=.*#LOGICAL :: TRACE_NORMALIZE=.$TRACE_NORMALIZE.#g"  $FILE > temp;  mv temp $FILE    
fi

# measurement type weight 
if [ ! -z "$measurement_weight" ]; then
    sed -e "s#^INTEGER, PARAMETER :: mtype=.*#INTEGER, PARAMETER :: mtype=${#measurement_weight[*]}#g"  $FILE > temp;  mv temp $FILE
    sed -e "s#^REAL(KIND=CUSTOM_REAL), DIMENSION(mtype) :: measurement_weight=.*#REAL(KIND=CUSTOM_REAL), DIMENSION(mtype) :: measurement_weight=[${measurement_weight[*]}]#g"  $FILE > temp;  mv temp $FILE
fi

# DD
if [ ! -z "$cc_threshold" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: cc_threshold=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: cc_threshold=${cc_threshold} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$DD_min" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: DD_min=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: DD_min=${DD_min} #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$DD_max" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: DD_max=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: DD_max=${DD_max} #g"  $FILE > temp;  mv temp $FILE
fi

# optimization
if [ ! -z "$opt_scheme" ]; then
    sed -e "s#^CHARACTER (LEN=2) :: opt_scheme=.*#CHARACTER (LEN=2) :: opt_scheme='$opt_scheme'#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$CGSTEPMAX" ]; then
    sed -e "s#^INTEGER, PARAMETER :: CGSTEPMAX=.*#INTEGER, PARAMETER :: CGSTEPMAX=$CGSTEPMAX #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$CG_scheme" ]; then
    sed -e "s#^CHARACTER (LEN=2) :: CG_scheme=.*#CHARACTER (LEN=2) :: CG_scheme='$CG_scheme'#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$BFGS_STEPMAX" ]; then
    sed -e "s#^INTEGER, PARAMETER :: BFGS_STEPMAX=.*#INTEGER, PARAMETER :: BFGS_STEPMAX=$BFGS_STEPMAX #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$initial_step_length" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: initial_step_length=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: initial_step_length=$initial_step_length #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$min_step_length" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: min_step_length=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: min_step_length=$min_step_length #g" $FILE > temp;  mv temp $FILE 
fi
if [ ! -z "$max_step" ]; then
    sed -e "s#^INTEGER, PARAMETER :: max_step=.*#INTEGER, PARAMETER :: max_step=$max_step#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$backtracking" ]; then
    sed -e "s#^LOGICAL :: backtracking=.*#LOGICAL :: backtracking=.$backtracking.#g"  $FILE > temp;  mv temp $FILE
fi

# CONVERGENCE?
if [ ! -z "$iter_start" ]; then
    sed -e "s#^INTEGER, PARAMETER :: iter_start=.*#INTEGER, PARAMETER :: iter_start=$iter_start #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$iter_end" ]; then
    sed -e "s#^INTEGER, PARAMETER :: iter_end=.*#INTEGER, PARAMETER :: iter_end=$iter_end #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$misfit_ratio_initial" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: misfit_ratio_initial=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: misfit_ratio_initial=$misfit_ratio_initial #g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$misfit_ratio_previous" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: misfit_ratio_previous=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: misfit_ratio_previous=$misfit_ratio_previous #g"  $FILE > temp;  mv temp $FILE
fi

# POST-PROCESSING
if [ ! -z "$smooth" ]; then
    sed -e "s#^LOGICAL :: smooth=.*#LOGICAL :: smooth=.$smooth.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$MASK_SOURCE" ]; then
    sed -e "s#^LOGICAL :: MASK_SOURCE=.*#LOGICAL :: MASK_SOURCE=.$MASK_SOURCE.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$source_radius" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: source_radius=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: source_radius=$source_radius #g" $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$MASK_STATION" ]; then
    sed -e "s#^LOGICAL :: MASK_STATION=.*#LOGICAL :: MASK_STATION=.$MASK_STATION.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$station_radius" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: station_radius=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: station_radius=$station_radius #g" $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$precond" ]; then
    sed -e "s#^LOGICAL :: precond=.*#LOGICAL :: precond=.$precond.#g"  $FILE > temp;  mv temp $FILE
fi
if [ ! -z "$wtr_precond" ]; then
    sed -e "s#^REAL(KIND=CUSTOM_REAL), PARAMETER :: wtr_precond=.*#REAL(KIND=CUSTOM_REAL), PARAMETER :: wtr_precond=$wtr_precond #g" $FILE > temp;  mv temp $FILE
fi

### DISPLAY
if [ ! -z "$DISPLAY_DETAILS" ]; then
    sed -e "s#^LOGICAL :: DISPLAY_DETAILS=.*#LOGICAL :: DISPLAY_DETAILS=.$DISPLAY_DETAILS.#g"  $FILE > temp;  mv temp $FILE
fi

if [ ! -z "$DISPLAY_TRACES" ]; then
    sed -e "s#^LOGICAL :: DISPLAY_TRACES=.*#LOGICAL :: DISPLAY_TRACES=.$DISPLAY_TRACES.#g"  $FILE > temp;  mv temp $FILE
fi


# empire function for vp and vs and rho
if [ ! -z "$empire_function" ]; then
    sed -e "s#^LOGICAL :: empire_function=.*#LOGICAL :: empire_function=.$empire_function.#g"  $FILE > temp;  mv temp $FILE
fi

