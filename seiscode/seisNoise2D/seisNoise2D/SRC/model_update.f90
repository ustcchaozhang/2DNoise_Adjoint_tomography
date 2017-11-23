program model_update
    ! To update model along search direction
    ! yanhuay@princeton.edu

    use seismo_parameters
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
    integer, parameter :: NARGS = 4
    INTEGER :: itime, ier, isrc,i,j
    character(len=MAX_STRING_LEN) :: model_names(MAX_KERNEL_NUM)
    character(len=MAX_STRING_LEN) :: model_names_comma_delimited
    character(len=MAX_STRING_LEN) :: arg(NARGS)
    character(len=MAX_STRING_LEN) :: directory
    real t1,t2
    character, parameter :: delimiter=','

    call cpu_time(t1)

    ! parse command line arguments
    if (command_argument_count() /= NARGS) then
        print *, 'USAGE:  mpirun -np NPROC bin/model_update.exe nproc directory MODEL_NAME, step_length'
        stop ' Please check command line arguments'
    endif

    do i = 1, NARGS
    call get_command_argument(i,arg(i), status=ier)
    enddo

    read(arg(1),*) nproc
    directory=arg(2)
    model_names_comma_delimited = arg(3)
    read(arg(4),*) step_length

    


    if (myrank == 0) write(*,'(a,f15.2,a)') 'try step length -- ',step_length*100,'%'

    call split_string(model_names_comma_delimited,delimiter,model_names,nmod)

    !! initialization  -- get number of spectral elements
    call initialize(directory)

    !! update model 
    call update(directory)

    !! save updated model  
    call finalize(directory,adjustl(model_names(1:nmod)))

end program model_update
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize(directory)
    use seismo_parameters
    implicit none
    integer :: ier,filesize
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory
    character(len=MAX_STRING_LEN) :: model_name

    ! slice database file
    allocate(nspec_proc(nproc))
    nspec_proc=0
    do myrank=0,nproc-1

    ! nspec
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/misfit_kernel/proc',myrank,'_'//trim(IBOOL_NAME) 
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/misfit_kernel/proc',myrank,'_'//trim(IBOOL_NAME)
    open(IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)            
    if (ier /= 0) then                          
        print *,'Error: could not open database file:',trim(filename)
        stop 'Error opening NSPEC_IBOOL file'       
    endif                                                
    read(IIN) nspec_proc(myrank+1)
    close(IIN)                                                          

    enddo

    nspec=sum(nspec_proc)
    if(DISPLAY_DETAILS) print*,'NGLLX*NGLLY*NGLLZ*NSPEC*nmod:',NGLLX,NGLLY,NGLLZ,NSPEC,nmod

    allocate(m_new(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    m_new = 0.0_CUSTOM_REAL
    allocate(m_delta(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    m_delta = 0.0_CUSTOM_REAL
    allocate(p_new(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    p_new = 0.0_CUSTOM_REAL
    allocate(m_try(NGLLX*NGLLY*NGLLZ*NSPEC*nmod))
    m_try = 0.0_CUSTOM_REAL

end subroutine initialize
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine update(directory)
    use seismo_parameters
    implicit none
    integer :: ier,imod
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    !! LOAD p_new
    write(filename,'(a)') &
        trim(directory)//'/optimizer/p_new.bin'
    print*,'LOAD p_new -- ', trim(filename)
    open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open model file: ',trim(filename)
        stop 'Error reading neighbors external mesh file'
    endif
    read(IIN) p_new
    close(IIN)

    !! LOAD m_new
    write(filename,'(a)') &
        trim(directory)//'/optimizer/m_new.bin'
    print*,'LOAD m_new -- ', trim(filename)
    open(unit=IIN,file=trim(filename),status='old',action='read',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open model file: ',trim(filename)
        stop 'Error reading neighbors external mesh file'
    endif
    read(IIN) m_new
    close(IIN)
    !if(DISPLAY_DETAILS ) print *,'Min / Max m_new = ', &
    !    minval(m_new(:)),maxval(m_new(:))

   
    !! by chao 
    !! define m_delta
    m_delta= m_new * step_length*p_new
    !! update 
    !m_try =m_new+ m_delta
    ! zhu (GJI,2015)
    ! key 
    m_try = m_new*exp(step_length*p_new)    

    ! ln drho = 0.33 ln dbeta
    !m_try(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3)=m_new(NGLLX*NGLLY*NGLLZ*NSPEC*2+1:NGLLX*NGLLY*NGLLZ*NSPEC*3)*exp(0.33*&
    !              step_length*p_new(1:NGLLX*NGLLY*NGLLZ*NSPEC*1))


    !! According to the Zhu,et al (2015 GJI), page 27 (12) ln(Mi+1/Mi)=alpha*di
    !model_vs_new=model_vs * exp( model_dbeta )
    print *,"we use the ln(Mi+1/Mi)=alpha*di"
    print *,'step_length=',step_length

    if(DISPLAY_DETAILS) print *,'Min / Max m_new = ', &
        minval(m_new(:)),maxval(m_new(:))

    if(DISPLAY_DETAILS) print *,'Min / Max p_new = ', &
        minval(p_new(:)),maxval(p_new(:))

    if(DISPLAY_DETAILS) print *,'Min / Max m_delta = ', &
        minval(m_delta(:)),maxval(m_delta(:))

    if(DISPLAY_DETAILS) print *,'Min / Max m_delta = ', &
        minval(1+step_length*p_new(:)),maxval(1+step_length*p_new(:))

    if(DISPLAY_DETAILS) print *,'Min / Max m_delta_zhu = ', &
        minval(exp(step_length*p_new(:))),maxval(exp(step_length*p_new(:)))

    if(DISPLAY_DETAILS) print *,'Min / Max m_try = ', &
        minval(m_try(:)),maxval(m_try(:))

   !if(DISPLAY_DETAILS) print *,'Min / Max m_try: m(i+1) = m(i)+ alpha*p ', &
   !     minval( m_new(:)+ step_length*p_new(:) ),maxval( m_new(:)+ step_length*p_new(:) )

    if(DISPLAY_DETAILS) print *,'Min / Max m_try_zhu = ', &
        minval( m_new(:)*exp(step_length*p_new(:)) ),maxval( m_new(:)*exp(step_length*p_new(:)) )


end subroutine update
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine finalize(directory,model_names)
    use seismo_parameters
    implicit none
    integer :: ier,imod
    integer :: nspec_start,nspec_end
    character(len=MAX_STRING_LEN) :: model_names(nmod)
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_store
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_store_vp
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_store_rho
    real(kind=CUSTOM_REAL), dimension(:,:,:,:,:),allocatable :: temp_vp




    allocate(temp_store(NGLLX,NGLLY,NGLLZ,NSPEC,nmod))
    temp_store = 0.0_CUSTOM_REAL


    allocate(temp_store_vp(NGLLX,NGLLY,NGLLZ,NSPEC,nmod))
    temp_store_vp = 0.0_CUSTOM_REAL

    allocate(temp_store_rho(NGLLX,NGLLY,NGLLZ,NSPEC,nmod))
    temp_store_rho = 0.0_CUSTOM_REAL

    allocate(temp_vp(NGLLX,NGLLY,NGLLZ,NSPEC,nmod))
    temp_vp = 0.0_CUSTOM_REAL




    temp_store=reshape(m_try,shape(temp_store))

    do myrank=0,nproc-1
    nspec_start=sum(nspec_proc(1:myrank))+1
    nspec_end=sum(nspec_proc(1:myrank))+nspec_proc(myrank+1)
    do imod=1,nmod
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_try/proc',myrank,'_'//&
        trim(model_names(imod))//'.bin'
    if (myrank == 0) print*,'SAVE m_try -- ', trim(filename)
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open gradient file: ',trim(filename)
        stop 'Error reading neighbors external mesh file'
    endif
    write(IOUT) temp_store(:,:,:,nspec_start:nspec_end,imod)
    print*,"mim/max ",trim(model_names(imod)),minval(temp_store(:,:,:,nspec_start:nspec_end,imod)),maxval(temp_store(:,:,:,nspec_start:nspec_end,imod))

    close(IOUT)
    enddo ! imod
   
    !!! personalize your own rhop-vp-vs relationship using
    !trim(model_names(imod))
 
    if(empire_function) then
    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_try/proc',myrank,'_'//&
        'vp'//'.bin'
    if (myrank == 0) print*,'SAVE m_try -- ', trim(filename)
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open gradient file: ',trim(filename)
        stop 'Error reading neighbors external mesh file'
    endif

    temp_vp(:,:,:,nspec_start:nspec_end,:)=temp_store(:,:,:,nspec_start:nspec_end,:)/1000
    temp_store_vp(:,:,:,nspec_start:nspec_end,:) = 0.9409 +  &
                     2.0947*temp_vp(:,:,:,nspec_start:nspec_end,:) - &    
                     0.8206*temp_vp(:,:,:,nspec_start:nspec_end,:)**2 + &
                     0.2683*temp_vp(:,:,:,nspec_start:nspec_end,:)**3 - &
                     0.0251*temp_vp(:,:,:,nspec_start:nspec_end,:)**4

    write(IOUT) temp_store_vp(:,:,:,nspec_start:nspec_end,:)*1000
    close(IOUT)
    print *,'the vp empire function'
  

    write(filename,'(a,i6.6,a)') &
        trim(directory)//'/m_try/proc',myrank,'_'//&
        'rho'//'.bin'

    if (myrank == 0) print*,'SAVE m_try -- ', trim(filename)
    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open gradient file: ',trim(filename)
        stop 'Error reading neighbors external mesh file'
    endif

    temp_store_rho(:,:,:,nspec_start:nspec_end,:) = &
                     1.6612*temp_store_vp(:,:,:,nspec_start:nspec_end,:) - &               
                     0.4721*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**2 + &
                     0.0671*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**3 - &        
                     0.0043*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**4 + &
                     0.000106*temp_store_vp(:,:,:,nspec_start:nspec_end,:)**5
    !print *,minval(temp_store_rho(:,:,:,nspec_start:nspec_end,:))
    write(IOUT) temp_store_rho(:,:,:,nspec_start:nspec_end,:)*1000
    close(IOUT)
    print *,'the rho empire function'

   endif

    

    enddo ! myrank
    if(DISPLAY_DETAILS) then

            print*,'vs=',minval(temp_store),maxval(temp_store)
            print*,'vp=',minval(temp_store_vp*1000),maxval(temp_store_vp*1000)
            print*,'rho=',minval(temp_store_rho*1000),maxval(temp_store_rho*1000)

    endif


    deallocate(temp_store_rho)
    deallocate(temp_store_vp)
    deallocate(temp_store)
    deallocate(temp_vp)
    deallocate(m_new)
    deallocate(m_delta)
    deallocate(m_try)
    deallocate(p_new)
    deallocate(nspec_proc)
end subroutine finalize 
