program optimization
    ! To calculate gradient and update direction
    ! yanhuay@princeton.edu

    use seismo_parameters
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    integer, parameter :: NARGS = 6
    INTEGER :: itime, ier, isrc,i,j,iter
    character(len=MAX_STRING_LEN) :: kernel_names(MAX_KERNEL_NUM)
    character(len=MAX_STRING_LEN) :: kernel_names_comma_delimited
    character(len=MAX_STRING_LEN) :: model_names(MAX_KERNEL_NUM)
    character(len=MAX_STRING_LEN) :: model_names_comma_delimited
    character(len=MAX_STRING_LEN) :: precond_name
    character(len=MAX_STRING_LEN) :: arg(NARGS)
    character(len=MAX_STRING_LEN) :: directory
    real t1,t2
    character, parameter :: delimiter=','

    call cpu_time(t1)

    ! parse command line arguments
    if (command_argument_count() /= NARGS) then
        if (DISPLAY_DETAILS) then
            print *, 'USAGE:  mpirun -np NPROC bin/gradient.exe ...'
            stop ' Please check command line arguments'
        endif
    endif

    do i = 1, NARGS
    call get_command_argument(i,arg(i), status=ier)
    enddo

    read(arg(1),*) nproc
    directory=arg(2) 
    kernel_names_comma_delimited = arg(3)
    precond_name=arg(4)
    model_names_comma_delimited = arg(5)
    read(arg(6),*) iter

    call split_string(kernel_names_comma_delimited,delimiter,kernel_names,nker)
    call split_string(model_names_comma_delimited,delimiter,model_names,nmod)

    if (nker .ne. nmod) then
        print*, 'number of kernel ',nker,' is not equal to number of model ',nmod
        !stop
    endif
    if(precond) then
        print*,'optimization with preconditioning'
        print*,'preconditioner -- ', trim(adjustl(precond_name))
    else
        print*,'optimization without preconditioning'
    endif

    !! initialization  -- get number of spectral elements
    call initialize(directory,adjustl(kernel_names(1:NKER)),&
        adjustl(precond_name),adjustl(model_names(1:nmod))) 

    !! optimization(update) direction
    call update_direction(directory,adjustl(kernel_names(1:NKER)),iter)

    !! save update direction 
    call finalize(directory)   

    call cpu_time(t2)

    if (DISPLAY_DETAILS .and. myrank==0) print *,'Computation time with CPU:',t2-t1

end program optimization
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize(directory,kernel_names,precond_name,model_names)
    use seismo_parameters
    implicit none
    integer :: ier,iker,imod
    integer :: filesize,nspec_start,nspec_end
    real(kind=CUSTOM_REAL) :: wtr
    character(len=MAX_FILENAME_LEN) :: filename,filename_pred
    character(len=MAX_STRING_LEN) :: directory
    character(len=MAX_STRING_LEN) :: kernel_names(nker)
    character(len=MAX_STRING_LEN) :: model_names(nmod)
    character(len=MAX_STRING_LEN) :: precond_name
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_store
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_store_model
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_empire
    real(kind=CUSTOM_REAL), dimension(:,:,:,:),allocatable :: preconditioner

    ! slice database file
    allocate(nspec_proc(nproc))
    nspec_proc=0  

    do myrank=0,nproc-1

    ! nspec
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/misfit_kernel/proc',myrank,'_'//trim(IBOOL_NAME) 
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print *,'Error: could not open database file:',trim(filename)
        stop 'Error opening _NSPEC_IBOOL file'
    endif
    read(IIN) nspec_proc(myrank+1)
    close(IIN)

    if(DISPLAY_DETAILS .and. myrank==0) print*, 'nspec_proc=',nspec_proc(myrank+1)

    enddo

    nspec=sum(nspec_proc)
    if(DISPLAY_DETAILS) print*,'NGLLX*NGLLY*NGLLZ*NSPEC*nmod:',NGLLX,NGLLY,NGLLZ,NSPEC,nmod

    !! gloabl
    allocate(g_new(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    !!!!!!!!!!!!!!!!!! by chao!!!!!!!!!!!!!!!!!!!!!!notification
    allocate(g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*3))




    g_new = 0.0_CUSTOM_REAL
    allocate(p_new(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    p_new = 0.0_CUSTOM_REAL
    allocate(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    m_new = 0.0_CUSTOM_REAL
    allocate(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    m_new_vp = 0.0_CUSTOM_REAL




    !! local
    !! by chao
    allocate(temp_store_model(NGLLX,NGLLY,NGLLZ,NSPEC,nmod))
    allocate(temp_store(NGLLX,NGLLY,NGLLZ,NSPEC,nker))
    allocate(temp_empire(NGLLX,NGLLY,NGLLZ,NSPEC,1))



    allocate(preconditioner(NGLLX,NGLLY,NGLLZ,NSPEC))
    temp_store = 0.0_CUSTOM_REAL
    temp_store_model= 0.0_CUSTOM_REAL
    preconditioner = 0.0_CUSTOM_REAL



    !! prepare g_new
    do myrank=0,nproc-1

    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)

    do iker=1,nker
    if(smooth) then
        write(filename,'(a,i6.6,a)') &
            trim(directory)//'/misfit_kernel/proc',myrank,&
            '_'//trim(kernel_names(iker))//'_smooth.bin'
    else
        write(filename,'(a,i6.6,a)') &
            trim(directory)//'/misfit_kernel/proc',myrank,&
            '_'//trim(kernel_names(iker))//'.bin'
    endif
    if(myrank==0) print*,'LOAD misfit_kernel -- ',trim(filename)
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open gradient file: ',trim(filename)
        stop 'Error: could not open gradient file: '
    endif
    read(IIN) temp_store(:,:,:,nspec_start:nspec_end,iker)
    close(IIN)
    enddo  ! iker

    ! preconditioner 
    if (precond) then 
        if(smooth) then
            write(filename,'(a,i6.6,a)') &
                trim(directory)//'/misfit_kernel/proc',myrank,&
                '_'//trim(precond_name)//'_smooth.bin'
        else
            write(filename,'(a,i6.6,a)') &
                trim(directory)//'/misfit_kernel/proc',myrank,&
                '_'//trim(precond_name)//'.bin'
        endif
        if(myrank==0) print*,'LOAD hessian_kernel -- ',trim(filename)
        open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
        if (ier /= 0) then
            print*, 'Error: could not open hessian file: ',trim(filename)
            stop 'Error: could not open hessian file: '
        endif
        read(IIN) preconditioner(:,:,:,nspec_start:nspec_end)
        close(IIN)

    endif ! non-empty preconditioner

    enddo  ! myrank




    wtr = maxval(preconditioner(:,:,:,:)) 
    print *,'wtr=',wtr


    do myrank=0,nproc-1
    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)

    do iker=1,nker
    if(precond) then
        write(filename_pred,'(a,i6.6,a)') &
            trim(directory)//'/misfit_kernel/proc',myrank,&
            '_'//trim(kernel_names(iker))//'_smooth_pred.bin'
    else
        write(filename,'(a,i6.6,a)') &
            trim(directory)//'/misfit_kernel/proc',myrank,&
            '_'//trim(kernel_names(iker))//'_pred.bin'
    endif

    open(unit=IOUT,file=trim(filename_pred),status='unknown',form='unformatted',iostat=ier)
    write(IOUT) temp_store(:,:,:,nspec_start:nspec_end,iker)/(preconditioner(:,:,:,nspec_start:nspec_end) +wtr_precond * wtr)
    !print *,'Max=',maxval(temp_store(:,:,:,nspec_start:nspec_end,iker)),'Min=',minval(temp_store(:,:,:,nspec_start:nspec_end,iker))
    close(IOUT)
    enddo  ! iker
   
    if (precond) then
        if(smooth) then
            write(filename,'(a,i6.6,a)') &
                trim(directory)//'/misfit_kernel/proc',myrank,&
                '_'//trim(precond_name)//'_smooth_add.bin'
        endif
        print*,'LOAD misfit_kernel -- ',trim(filename)
        open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
        !print *,'wtr=',wtr
        write(IOUT) preconditioner(:,:,:,nspec_start:nspec_end)+wtr_precond*wtr
        close(IOUT)
    endif

    enddo  ! myrank  


    !! preconditioning [misfit_kernel --> g_new]
    wtr = maxval(preconditioner(:,:,:,:))
    print *,'wtr=',wtr
    if(wtr>SMALL_VAL)then
        do iker=1,nker
        temp_store(:,:,:,:,iker) = &
            temp_store(:,:,:,:,iker) / (preconditioner(:,:,:,:) + wtr_precond * wtr)
        enddo
    endif
    print *,'Max=',maxval(temp_store(:,:,:,:,iker)),'Min=',minval(temp_store(:,:,:,:,iker))

   ! do myrank=0,nproc-1
   ! nspec_start=sum(nspec_proc(1:myrank))+1
   ! nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)

   ! do iker=1,nker
   ! if(smooth) then
   !     write(filename_pred,'(a,i6.6,a)') &
   !         trim(directory)//'/misfit_kernel/proc',myrank,&
   !         '_'//trim(kernel_names(iker))//'_smooth_pred.bin'
   ! else
   !     write(filename,'(a,i6.6,a)') &
   !         trim(directory)//'/misfit_kernel/proc',myrank,&
   !         '_'//trim(kernel_names(iker))//'.bin'
   ! endif

   ! open(unit=IOUT,file=trim(filename_pred),status='unknown',form='unformatted',iostat=ier)    
   ! write(IOUT) temp_store(:,:,:,nspec_start:nspec_end,iker)
   ! print *,'Max=',maxval(temp_store(:,:,:,nspec_start:nspec_end,iker)),'Min=',minval(temp_store(:,:,:,nspec_start:nspec_end,iker))
   ! close(IOUT)
   ! enddo  ! iker

   ! if (precond) then
   !     if(smooth) then
   !         write(filename,'(a,i6.6,a)') &
   !             trim(directory)//'/misfit_kernel/proc',myrank,&
   !             '_'//trim(precond_name)//'_smooth_add.bin'
   !     endif
   !     print*,'LOAD misfit_kernel -- ',trim(filename)
   !     open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
   !     !print *,'wtr=',wtr
   !     write(IOUT) preconditioner(:,:,:,nspec_start:nspec_end)+wtr_precond*wtr
   !     close(IOUT)
   ! endif

   ! enddo  ! myrank




    g_empire=reshape(temp_store,shape(g_empire))

    !! convert to 1D vector

    g_new=reshape(temp_store,shape(g_new))
    if(DISPLAY_DETAILS .and. myrank==0) then
        print *,'myrank=',myrank,' Min / Max g_new = ',&
            minval(g_new(:)),maxval(g_new(:))
    endif

    !! prepare m_new
    temp_store = 0.0_CUSTOM_REAL
    temp_store_model = 0.0_CUSTOM_REAL
    !! prepare g_new
    do myrank=0,nproc-1

    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)
    do imod=1,nmod
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_current/proc',myrank,&
        '_'//trim(model_names(imod))//'.bin'
    if(myrank==0) print*,'LOAD m_current -- ',trim(filename)
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open m_new file: ',trim(filename)
        stop 'Error: could not open m_new file: '
    endif
    read(IIN) temp_store_model(:,:,:,nspec_start:nspec_end,imod)
    close(IIN)
    enddo ! imod
    enddo ! myrank
    m_new=reshape(temp_store_model,shape(m_new))

    if(DISPLAY_DETAILS .and. myrank==0) then
        print *,'myrank=',myrank,' Min / Max m_new = ',&
            minval(m_new(:)),maxval(m_new(:))
    endif

    !read rho
    temp_store_model = 0.0_CUSTOM_REAL
    m_new_vp =0.0_CUSTOM_REAL
    do myrank=0,nproc-1
    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)
    do imod=1,nmod
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_current/proc',myrank,&
        '_rho.bin'
    if(myrank==0) print*,'LOAD m_current -- ',trim(filename)
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open m_new file: ',trim(filename)
        stop 'Error: could not open m_new file: '
    endif
    read(IIN) temp_store_model(:,:,:,nspec_start:nspec_end,imod)
    close(IIN)
    enddo ! imod
    enddo ! myrank
    m_new_vp=reshape(temp_store_model,shape(m_new_vp))

    if(DISPLAY_DETAILS .and. myrank==0) then
        print *,'myrank=',myrank,' Min / Max m_new_vp = ',&
            minval(m_new_vp(:)),maxval(m_new_vp(:))
    endif

    !read vp
    temp_store_model = 0.0_CUSTOM_REAL
    m_new_vp =0.0_CUSTOM_REAL
    do myrank=0,nproc-1
    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)
    do imod=1,nmod
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_current/proc',myrank,&
        '_vp.bin'
    if(myrank==0) print*,'LOAD m_current -- ',trim(filename)
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open m_new file: ',trim(filename)
        stop 'Error: could not open m_new file: '
    endif
    read(IIN) temp_store_model(:,:,:,nspec_start:nspec_end,imod)
    close(IIN)
    enddo ! imod
    enddo ! myrank
    m_new_vp=reshape(temp_store_model,shape(m_new_vp))

    if(DISPLAY_DETAILS .and. myrank==0) then
        print *,'myrank=',myrank,' Min / Max m_new_vp = ',&
            minval(m_new_vp(:)),maxval(m_new_vp(:))
    endif


   !! by chao, here we use the empire function for vp and rho 
    !empire_function=.false.
    if(empire_function) then
            print *,'there is empire function for vp,vs and rho'
            g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)=[(2.0947-&
            0.8206*2*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.2683*3*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0251*4*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3)*&
            g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)+ &     
            (1.6612-&
            0.4721*2*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.0671*3*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0043*4*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3+&
            0.000106*5*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**4)*&
            g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3) + &   
            g_empire(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)]   

            !print *,'max=',maxval(g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)),'min=',minval(g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1))
            !print*,'m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)=',m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)
            !print*,'m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)=',m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)
          

            print*,'R1(z)=',(2.0947-&
            0.8206*2*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)+&
            0.2683*3*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**2-&
            0.0251*4*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**3)

            print*,'R2(z)=',  (1.6612-&
            0.4721*2*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)+&
            0.0671*3*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**2-&
            0.0043*4*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**3+&
            0.000106*5*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**4)

            print*,'R1(z)*alpha=',(2.0947-&
            0.8206*2*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)+&
            0.2683*3*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**2-&
            0.0251*4*(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**3)*&
            g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*3/2)

            print*,'R2(z)*rho=',  (1.6612-&
            0.4721*2*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)+&
            0.0671*3*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**2-&
            0.0043*4*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**3+&
            0.000106*5*(m_new_vp(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)/1000)**4)*&
            g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*5/2)

            print*,'1*beta=',g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)
            print*,'R1(z)*alpha+R2(z)*rho+1*beta=',g_new(NGLLX*NGLLY*NGLLZ*NSPEC*1/2)

            print*,'max/min R1(z)=',maxval(2.0947-&
            0.8206*2*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.2683*3*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0251*4*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3),&
            minval(2.0947-&
            0.8206*2*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.2683*3*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0251*4*(m_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3)
         
            print*,'max/min R2(z)=', maxval(1.6612-&
            0.4721*2*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.0671*3*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0043*4*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3+&
            0.000106*5*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**4),&            
            minval(1.6612-&
            0.4721*2*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)+&
            0.0671*3*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**2-&
            0.0043*4*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**3+&
            0.000106*5*(m_new_vp(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)/1000)**4)


            print*,'max/min 1*beta=',&
            maxval(g_empire(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)),minval(g_empire(1:NGLLX*NGLLY*NGLLZ*NSPEC*1))
            print*,'max/min 1*alpha=',&
            maxval(g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2)),minval(g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*1+1:NGLLX*NGLLY*NGLLZ*NSPEC*2))
            print*,'max/min 1*rho=',&
            maxval(g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3)),minval(g_empire(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3))

            print*,'max/min all=',&
            maxval(g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)),minval(g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1))            
            
    else 
            print *,'there is no empire function for vp,vs and rho'
            !g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)=g_empire(1:NGLLX*NGLLY*NGLLZ*NSPEC*1)
            g_new(:)=g_empire(:)
    endif 


    temp_empire=reshape(g_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1),shape(temp_empire))

    ! by chao write the smooth kernel after empire function for vp and vs
    do myrank=0,nproc-1
    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)


    do iker=1,1
        write(filename_pred,'(a,i6.6,a)') &
            trim(directory)//'/misfit_kernel/proc',myrank,&
            '_'//trim(kernel_names(iker))//'_smooth_empire.bin'
    open(unit=IOUT,file=trim(filename_pred),status='unknown',form='unformatted',iostat=ier)
    write(IOUT) temp_empire(:,:,:,nspec_start:nspec_end,iker)
    close(IOUT)

    enddo


   ! do iker=1,1
   !     write(filename_pred,'(a,i6.6,a)') &
   !         trim(directory)//'/misfit_kernel/proc',myrank,&
   !         '_'//trim(kernel_names(iker))//'_smooth_empire_pred.bin'
   ! open(unit=IOUT,file=trim(filename_pred),status='unknown',form='unformatted',iostat=ier)  
   ! write(IOUT) temp_empire(:,:,:,nspec_start:nspec_end,iker)/(preconditioner(:,:,:,nspec_start:nspec_end)+wtr_precond*wtr )
   ! close(IOUT)
   ! enddo

    enddo


    deallocate(temp_empire)
    deallocate(temp_store)
    deallocate(temp_store_model)
    deallocate(preconditioner)
    deallocate(nspec_proc)
end subroutine initialize
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine update_direction(directory,kernel_names,iter)
    use seismo_parameters
    implicit none
    integer :: iker,ier,iter
    character(len=MAX_STRING_LEN) :: kernel_names(nker)
    real(kind=CUSTOM_REAL), dimension(:,:,:,:),allocatable :: temp_store
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory
    real(kind=CUSTOM_REAL) :: pmax
    !! CG
    integer :: cgstep
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: g_old
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: p_old
    real(kind=CUSTOM_REAL) :: temp
    !! BFGS
    integer :: BFGS_step,m
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: m_old
    real(kind=CUSTOM_REAL), dimension(:,:),allocatable :: Deltam
    real(kind=CUSTOM_REAL), dimension(:,:),allocatable :: Deltag

    allocate(g_old(size(g_new)))
    allocate(p_old(size(g_new)))
    allocate(m_old(size(g_new)))
    allocate(Deltam(size(g_new),BFGS_stepmax))
    allocate(Deltag(size(g_new),BFGS_stepmax))
    g_old = 0.0_CUSTOM_REAL
    p_old = 0.0_CUSTOM_REAL
    m_old = 0.0_CUSTOM_REAL
    Deltam = 0.0_CUSTOM_REAL
    Deltag = 0.0_CUSTOM_REAL

    select case(opt_scheme)
    case("SD") !! steepest descent method  
        if(myrank==0) print*, 'steepest descent for iter  ',iter
        !! search direction
        call SD(g_new, size(g_new),p_new)
    case ("CG") 
        if(iter==1) then   !! first iter step, do SD
            !! search direction 
            if(myrank==0) print*, 'steepest descent for iter 1 '
            call SD(g_new, size(g_new), p_new)
            cgstep = 1

        else !! not the first iter step, try CG
            if(myrank==0) print*, 'CG for iter ',iter

            ! additional file needed: cgstep
            write(filename,'(a)') trim(directory)//'/optimizer/cgstep.dat'
            OPEN(IIN,FILE=trim(filename),STATUS='old',action='read',iostat=ier)
            if(ier>0) then
                print*, 'Error: could not open cgstep file:',trim(filename)

                stop 'Error: could not open cgstep file:'
            else
                read(IIN,*) cgstep
            end if
            close(IIN)
            if(myrank==0) print*,'LOAD cgstep -- ',trim(filename)

            !! second if   
            if( cgstep > cgstepmax ) then ! exceed max cg steps, do SD
                print*, 'restarting NLCG ... [periodic restart]'
                cgstep = 1
                !! search direction 
                print*, 'steepest descent for restarting iter=',&
                    iter, ' cgstep=', cgstep
                call SD(g_new, size(g_new), p_new)

            elseif(cgstep>=1 .and. cgstep<=cgstepmax) then !! not exceed maxcg steps, try CG 
                ! additional file needed: g_old, p_old                                 
                write(filename,'(a)')  trim(directory)//'/optimizer/g_old.bin'

                print*,'LOAD g_old -- ', trim(filename)
                open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
                if (ier /= 0) then
                    print*, 'Error: could not open g_old file: ',trim(filename)
                    stop 'Error: could not open g_old file: '
                endif
                read(IIN) g_old
                close(IIN) 

                !! p_old
                write(filename,'(a)')  trim(directory)//'/optimizer/p_old.bin'
                print*,'LOAD p_old -- ', trim(filename)
                open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
                if (ier /= 0) then
                    print*, 'Error: could not open p_old file: ',trim(filename)
                    stop 'Error: could not open p_old file: '
                endif
                read(IIN) p_old
                close(IIN)
                !! search direction 
                print*, 'conjugate gradient direction for iter=',&
                    iter, ' cgstep=', cgstep
                call NLCG(g_new, g_old, p_old, size(g_new), CG_scheme, cgstep, p_new)

            endif !! cgstep
        endif !! iter==1

        !! save cgstep
        write(filename,'(a)') trim(directory)//'/optimizer/cgstep.dat'
        OPEN(IOUT,FILE=trim(filename),STATUS='unknown',iostat=ier)
        if(ier>0) then
            print*, 'Error: could not open cgstep file:',trim(filename)

            stop 'Error: could not open cgstep file:'
        else
            write(IOUT,*) cgstep
        end if
        close(IOUT)
        print*,'SAVE cgstep -- ',trim(filename)


    case("QN") !! Qausi-Newton (L-BFGS) method   
        !! first if 
        if(iter==1) then   !! first iter step, do SD
            !! search direction 
            print*, 'steepest descent for iter 1 '
            call SD(g_new, size(g_new), p_new)
            BFGS_step = 1

        else !! not the first iter step, try L_BFGS
            print*, 'L-BFGS for iter ',iter

            ! additional file needed: BFGS_step, m_old, g_old
            write(filename,'(a)') trim(directory)//'/optimizer/BFGS_step.dat'
            OPEN(IIN,FILE=filename,STATUS='old',action='read',iostat=ier)
            if(ier>0) then
                print*,'Error opening BFGS_step.dat file : ',trim(filename)
                stop 'Error opening BFGS_step.dat file : '
            else
                read(IIN,*) BFGS_step
            end if
            close(IIN)
            print*,'LOAD old BFGS_step -- ',trim(filename)

            !! m_old -->  m_new -m_old
            write(filename,'(a)')  trim(directory)//'/optimizer/m_old.bin'
            print*,'LOAD m_old -- ', trim(filename)
            open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open m_old file: ',trim(filename)
                stop 'Error: could not open m_old file: '
            endif
            read(IIN) m_old
            close(IIN)
            if(DISPLAY_DETAILS .and. myrank==0) then
                print *,' Min / Max m_old = ', &
                    minval(m_old(:)),maxval(m_old(:))
            endif

            !! g_old --> g_new - g_old
            write(filename,'(a)')  trim(directory)//'/optimizer/g_old.bin'
            print*,'LOAD g_old -- ', trim(filename)
            open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open g_old file: ',trim(filename)
                stop 'Error: could not open g_old file: '
            endif
            read(IIN) g_old
            close(IIN)

            !! m- step L-BFGS (accumulative steps and max steps)
            m = min(BFGS_step,BFGS_stepmax)

            !! Deltam, Deltag: renew
            Deltam(:,1) = m_new(:)-m_old(:)
            Deltag(:,1) = g_new(:)-g_old(:)

            if(m>1 .and. m<=BFGS_stepmax) then !! consider multiple previous steps
                ! additonal files: old Deltam, Deltag
                !! LOAD Deltam
                write(filename,'(a)')  trim(directory)//'/optimizer/Deltam.bin'
                print*,'LOAD Deltam -- ', trim(filename)
                open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
                if (ier /= 0) then
                    print*, 'Error: could not open Deltam file: ',trim(filename)
                    stop 'Error: could not open Deltam file: '
                endif
                read(IIN) Deltam(:,2:BFGS_stepmax)
                close(IIN)

                !! LOAD Deltag
                write(filename,'(a)')  trim(directory)//'/optimizer/Deltag.bin'
                print*,'LOAD Deltag -- ', trim(filename)
                open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
                if (ier /= 0) then
                    print*, 'Error: could not open Deltam file: ',trim(filename)
                    stop 'Error: could not open Deltam file: '
                endif
                read(IIN) Deltag(:,2:BFGS_stepmax)
                close(IIN)
            endif ! m>1

            ! BFGS direction
            print*, 'L-BFGS direction for iter=',iter, &
                ' BFGS_step=', BFGS_step, ' m=',m

            ! B-BFGS
            call LBFGS(Deltam, Deltag, g_new, size(g_new), m, BFGS_step, p_new)

            !! check restarting or not 
            if(BFGS_step ==1) then
                Deltag(:,:)=0.0
                Deltam(:,:)=0.0
            endif
        endif !iter==1

        if(BFGS_step > 1) then
            !! SAVE Deltam
            write(filename,'(a)')  trim(directory)//'/optimizer/Deltam.bin'
            print*,'SAVE Deltam -- ', trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open Deltam file: ',trim(filename)
                stop 'Error: could not open Deltam file: '
            endif
            write(IOUT) Deltam
            close(IOUT)

            !! SAVE Deltag
            write(filename,'(a)')  trim(directory)//'/optimizer/Deltag.bin'
            if(DISPLAY_DETAILS .and. myrank==0) print*,'SAVE Deltag -- ', trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open Deltam file: ',trim(filename)
                stop 'Error: could not open Deltam file: '
            endif
            write(IOUT) Deltag
            close(IOUT)
        endif

        !! save BFGS_step
        write(filename,'(a)') trim(directory)//'/optimizer/BFGS_step.dat'
        OPEN(IOUT,FILE=trim(filename),STATUS='unknown',iostat=ier)
        if(ier>0) then
            print*, 'Error: could not open cgstep file:',trim(filename)
            stop 'Error: could not open cgstep file:'
        else
            write(IOUT,*) BFGS_step
        end if
        close(IOUT)
        print*,'SAVE BFGS_step -- ',trim(filename)

    case default
        print*, 'opt_scheme must be among "SD"/"CG"/"QN" ...'
        stop 'opt_scheme must be among "SD"/"CG"/"QN" ...'

    end select      

    if(DISPLAY_DETAILS) then
        print*,'Min / Max direction = ',&
            minval(p_new(:)),maxval(p_new(:))
    endif

    !! noramlize p_new
    pmax=maxval(abs(p_new(:)))

    if(DISPLAY_DETAILS) then
        print*,' Max pmax = ',&
            pmax
    endif
    
    if(pmax>SMALL_VAL)  p_new = p_new / pmax

    deallocate(g_old)
    deallocate(p_old)
    deallocate(Deltam)
    deallocate(Deltag)

end subroutine update_direction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine finalize(directory)
    use seismo_parameters
    implicit none
    integer :: ier
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    !! SAVE gradient
    write(filename,'(a)')  trim(directory)//'/optimizer/g_new.bin'
    print*,'SAVE g_new -- ', trim(filename)
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open gradient file: ',trim(filename)
        stop 'Error: could not open gradient file: '
    endif
    write(IOUT) g_new
    close(IOUT) 

    if(DISPLAY_DETAILS) then
        print *,'Min / Max gradient = ',&
            minval(g_new(:)),maxval(g_new(:))
    endif

    !! SAVE direction
    write(filename,'(a)')  trim(directory)//'/optimizer/p_new.bin'
    print*,'SAVE p_new -- ',trim(filename) 
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open direction file: ',trim(filename)
        stop 'Error: could not open direction file: '
    endif
    write(IOUT) p_new
    close(IOUT) 

    if(DISPLAY_DETAILS) then
        print *,'Min / Max direction = ', &
            minval(p_new(:)),maxval(p_new(:))
    endif

    !! SAVE model 
    write(filename,'(a)')  trim(directory)//'/optimizer/m_new.bin'
    print*,'SAVE m_new -- ',trim(filename)
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open model file: ',trim(filename)
        stop 'Error: could not open model file: '
    endif
    write(IOUT) m_new
    close(IOUT)

    if(DISPLAY_DETAILS) then
        print *,'Min / Max model = ', &
            minval(m_new(:)),maxval(m_new(:))
    endif

    ! deallocate
    deallocate(g_new)
    deallocate(g_empire)
    !deallocate(temp_empire)
    deallocate(p_new)
    deallocate(m_new)
    deallocate(m_new_vp)
end subroutine finalize
