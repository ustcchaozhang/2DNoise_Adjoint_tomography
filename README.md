# 3D-2D


 ssh -X zhang18@login.scinet.utoronto.ca
 
 ssh -X  gpc03
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF/submit_job
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN1/submit_job 
 
  
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN/submit_job 
 
 cd /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC
 
 vim /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC/misfit_adjoint.f90
 
cd  /scratch/l/liuqy/zhang18/seisDD/seisDD/lib

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_smooth/submit_job/RESULTS/invesion/Scale0_CC_AD

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang_smooth/submit_job/RESULTS/invesion/Scale0_CC_AD/misfit_kernel/*beta_kernel_smooth*

cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD/misfit_kernel/*beta_kernel_smooth*

scp zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/RESULTS/invesion/Scale0_CC_AD/m_6/*vs*

scp -r zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/FwiEGF_fang/000000/DATA_*


mpirun -np 4 ./bin/misfit_adjoint.exe true z CC AD /oldscratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN/submit_job/FwiSYN//000000/

mpirun -np 4 ./bin/misfit_adjoint.exe true z CC AD /sgfs1/scratch3/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF_fang/submit_job/FwiEGF_fang//000000/



创新的live 2，飞利浦的SHP9500




 
                              
                              
                              
                              
                               
