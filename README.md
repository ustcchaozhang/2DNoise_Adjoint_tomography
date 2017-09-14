# 3D-2D
 scp -r  zhangc@172.21.51.221:/home/zhangc/
 ssh -X  zhangc@172.21.51.221
 ssh -X zhang18@login.scinet.utoronto.ca
 ssh zhangc@128.100.79.35 -X
 ssh zhangchao@222.195.76.41
 
 
 
 ssh -X  gpc03
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF/submit_job
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN1/submit_job 
 
  
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN/submit_job 
 
 cd /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC
 
vim /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC/misfit_adjoint.f90

vim /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC/data_misfit.f90

cd  /scratch/l/liuqy/zhang18/seisDD/seisDD/lib

vim  /scratch/l/liuqy/zhang18/seisDD/seisDD/lib/src/adjoint_lib.f90 

vim /scratch/l/liuqy/zhang18/seisDD/seisDD/lib/src/constants.f90 

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_smooth/submit_job

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_gauss/submit_job

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_line2/submit_job/

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_smooth/submit_job/RESULTS/invesion/Scale0_CC_AD/misfit_kernel/*beta_kernel_smooth*

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD/misfit_kernel/*beta_kernel_smooth*

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD/m_6/*vs*

scp -r zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/FwiEGF_fang/000000/DATA_*


mpirun -np 4 ./bin/misfit_adjoint.exe true z CC AD /oldscratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN/submit_job/FwiSYN//000000/

mpirun -np 4 ./bin/misfit_adjoint.exe true z CC AD /sgfs1/scratch3/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/FwiEGF_fang//000000/

mpirun -np 4 ./bin/misfit_adjoint.exe true z MT AD /sgfs1/scratch3/l/liuqy/zhang18/seisDD/GJI2016/FwiSGF/submit_job/FwiSGF/000000/






 
                              
                              
                              
                              
                               
