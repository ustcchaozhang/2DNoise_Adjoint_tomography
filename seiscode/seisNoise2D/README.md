# seisNoise2D

This is the source code without users' manual about how to use it. If you need a detail description, feel free to send an email to Chao Zhang (czhang18@mail.ustc.edu.cn) with a brief introduction about yourself and why you want to use it. We will be happy to help.


INSTRUCTIONS:
# The default system is on pbs system used at University of Toronto
#
# seisNoise2D 
# download devel branch seisNoise2D from github
git clone https://github.com/ustcchaozhang/2DNoise_Adjoint_tomography/tree/master/seiscode/seisNoise2D  
# set seisNoise2D path
echo '# set seisNoise2D path' >> ~/.bashrc
echo 'export seisNoise2D=/path/of/seisNoise2D' >> ~/.bashrc 
source ~/.bashrc
echo $seisNoise2D
# make sure $seisDD is the same as '/path/to/seisNoise2D' 

# specfem2d
# compile with ifort and mpi
module load intel-mkl/11.0/1/64
module load intel/13.0/64/13.0.1.117
cd $seisNoise2D/specfem2d
## with mpi for parallel simulation
module load openmpi/intel-13.0/1.6.3/64
./configure FC=ifort --with-mpi
## without mpi for serial simulation (queue='serial') 
./configure FC=ifort   #(recommanded)
## or 
./configure FC=gfortran
# compile
make all

cd $seisDD/seisDD/lib
make -f make_lib clean
make -f make_lib

# Experiments in paper:
#Double-difference adjoint seismic tomography
#YO Yuan, FJ Simons, J Tromp
#Geophysical Journal International (2016) accepted for publication
cd $seisDD/GJI2016
# For details, refer to https://github.com/yanhuay/seisDD/tree/master/GJI2016

#
# Addtional comments
# How to start your own project in user private folder under EXAMPLES (not afftected in updating)
>> cd $seisDD/EXAMPLES 
# checklist (see GJI2016/Exp1 for demonstration):
# DATA/: interfaces.dat  Par_file  SOURCE  sources.dat  STATIONS 
# model_init_bin: initial gll binary model 
# model_true_bin: target gll binary model (optional if observed data exist)
# data: obaserved data (optional if model_true_bin exists)
# parameter: inversion parameter file including path setting and inversion parameters
# SU_process: preprocessing procedure using SU package (optional)
# run_this_example.sh (shared for all examples)










