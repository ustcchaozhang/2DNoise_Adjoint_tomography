# 3D-2D


 ssh -X zhang18@login.scinet.utoronto.ca
 
 ssh -X  gpc03
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF/submit_job
 
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN1/submit_job 
 
  
 cd /scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiSYN/submit_job 
 
 cd /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC
 
 vim /scratch/l/liuqy/zhang18/seisDD/seisDD/SRC/misfit_adjoint.f90

scp   zhang18@login.scinet.utoronto.ca:/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF/model_init_bin_empirefunc



/scratch/l/liuqy/zhang18/seisDD/GJI2016/FwiEGF/submit_job

/scratch/l/liuqy/zhang18/seisDD/seisDD/SRC


创新的live 2，飞利浦的SHP9500



empire function for vp and vs

    g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)=[(2.0947-&
    0.8206*2*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
    0.2683*3*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
    0.0251*4*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3)*&
    g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)+ &
    (1.6612-&
    0.4721*2*m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)+&
    0.0671*3*m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)**2-&
    0.0043*4*m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)**3+&
    0.000106*5*m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)**4)*&
    g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3) + &
    g_empire(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)]
    
empire function for vp and vs

            temp_store_rho(:,:,:,nspec_start:nspec_end,:) = &
                     1.6612*temp_store_vp(:,:,:,nspec_start:nspec_end,:) - &               
                     0.4721*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**2 + &
                     0.0671*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**3 - &        
                     0.0043*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**4 + &
                     0.000106*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**5


            temp_store_vp(:,:,:,nspec_start:nspec_end,:) = 0.9409 +  &
                     2.0947*temp_vp(:,:,:,nspec_start:nspec_end,:) - &    
                     0.8206*temp_vp(:,:,:,nspec_start:nspec_end,:)**2 + &
                     0.2683*temp_vp(:,:,:,nspec_start:nspec_end,:)**3 - &
                     0.0251*temp_vp(:,:,:,nspec_start:nspec_end,:)**4
 
                              
                              
                              
                              
                               
