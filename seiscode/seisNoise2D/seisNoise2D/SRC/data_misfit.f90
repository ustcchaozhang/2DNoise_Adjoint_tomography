program data_misfit
    ! sum of misfits and check iterative status
    ! yanhuay@princeton.edu

    use seismo_parameters
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer, parameter :: NARGS = 6
    INTEGER :: isrc,iter
    INTEGER :: NPROC_DATA 
    INTEGER :: i,j
    INTEGER :: ier
    real(kind=CUSTOM_REAL) ::misfit_cur,misfit_cur_mean,misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,misfit_cur_average
    real(kind=CUSTOM_REAL) ::misfit_cur_f2,misfit_cur_f3,misfit_cur_f4,misfit_cur_f5,misfit_cur_f6,misfit_cur_f7
    real(kind=CUSTOM_REAL) ::misfit_cur_mean2,misfit_cur_mean3,misfit_cur_mean4,misfit_cur_mean5,misfit_cur_mean6,misfit_cur_mean7
    real(kind=CUSTOM_REAL) ::misfit_cur_std2,misfit_cur_std3,misfit_cur_std4,misfit_cur_std5,misfit_cur_std6,misfit_cur_std7
    character(len=MAX_STRING_LEN) :: arg(NARGS)
    character(len=MAX_STRING_LEN) :: input_dir,output_dir
    character(len=MAX_FILENAME_LEN) :: FILENAME

    ! parse command line arguments
    if (command_argument_count() /= NARGS) then
        print *, 'USAGE: .bin/data_misfit.exe ...'
        stop ' Please check command line arguments'
    endif
    do i = 1, NARGS
    call get_command_argument(i,arg(i), status=ier)
    enddo
    read(arg(1),*) iter
    read(arg(2),*) step_length
    read(arg(3),*) compute_adjoint
    read(arg(4),*) NPROC_DATA
    input_dir=arg(5) 
    output_dir=arg(6)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! sum event_misfit
    !call sum_misfit(input_dir,misfit_cur,NPROC_DATA)
    call sum_misfit_mean(input_dir,misfit_cur,misfit_cur_f2,misfit_cur_f3,misfit_cur_f4,misfit_cur_f5,misfit_cur_f6,misfit_cur_f7,misfit_cur_mean2,misfit_cur_mean3,misfit_cur_mean4,misfit_cur_mean5,misfit_cur_mean6,misfit_cur_mean7,misfit_cur_std2,misfit_cur_std3,misfit_cur_std4,misfit_cur_std5,misfit_cur_std6,misfit_cur_std7,NPROC_DATA)
    !call sum_misfit_all(input_dir,misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,NPROC_DATA)
    !print *,misfit_cur_mean2,misfit_cur_mean3,misfit_cur_mean4,misfit_cur_mean5,misfit_cur_mean6,misfit_cur_mean7
    !print *,'all_misfit_4band:',misfit_cur_f2,misfit_cur_f3,misfit_cur_f4,misfit_cur_f5
    print *,'all misfit:',misfit_cur
    !print *,'misfit_cur=',misfit_cur,(misfit_cur_2+misfit_cur_3+misfit_cur_4+misfit_cur_5+misfit_cur_6+misfit_cur_7)/6

    !misfit_cur_mean=0.02
    if(iter>0) then ! doing inversion/kernel
        !!! add current result to search history
        write(filename, "(a)") trim(output_dir)//'/misfit/data_misfit_hist_detail'
        OPEN (IOUT, FILE=trim(filename),status='unknown',POSITION='APPEND')
        write(IOUT,'(i,f15.5,e15.8)') iter,step_length,misfit_cur
        close(IOUT)

        ! print *,'misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,misfit_cur_mean',misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,misfit_cur_mean
       ! write(filename, "(a)") trim(output_dir)//'/misfit/all_data_misfit_hist_detail'
       ! OPEN (IOUT, FILE=trim(filename),status='unknown',POSITION='APPEND')
       ! write(IOUT,'(i,f15.5,6f15.5)') iter,step_length,misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7
       ! close(IOUT)


        write(filename, "(a)") trim(output_dir)//'/misfit/all_misfit'
        OPEN (IOUT, FILE=trim(filename),status='unknown',POSITION='APPEND')
        write(IOUT,'(i,f15.5,5f15.5)') iter,step_length,misfit_cur_f2
        close(IOUT)



        ! check search status 
        if(step_length==0.0) then ! starting line search
            ! search status initilization
            is_cont=1
            is_done=0
            is_brak=0
            next_step_length=initial_step_length
            optimal_step_length=0.0
            if(iter==1) then ! starting iteration
                !!! misfit hist for iteration 
                write(filename,'(a)') trim(output_dir)//'/misfit/data_misfit_hist.dat'
                OPEN (UNIT=IOUT, FILE=trim(filename),status='unknown',POSITION='APPEND')
                write(IOUT,'(I5,e15.8)') iter-1,misfit_cur
                close(IOUT)
                print*,'misfit_hist for niter',iter, ':',misfit_cur
            else ! check iteration
                call check_iteration(output_dir)
            endif
        else ! line search 
            call check_linesearch(output_dir,iter)
        endif ! status of line search

        !! SAVE search status
        write(filename,'(a)') trim(output_dir)//'/misfit/search_status.dat'
        OPEN (IOUT, FILE=trim(filename))
        write(IOUT,'(I5)') is_cont
        write(IOUT,'(I5)') is_done
        write(IOUT,'(I5)') is_brak
        write(IOUT,'(f15.5)') next_step_length
        write(IOUT,'(f15.5)') optimal_step_length
        close(IOUT)

    else ! misfit evaluations
        write(filename, "(a)") trim(output_dir)//'/misfit/data_misfit'
        OPEN (IOUT, FILE=trim(filename),status='unknown',POSITION='APPEND')
        write(IOUT,'(e15.8)') misfit_cur
        close(IOUT)
    endif ! iter>0

end program data_misfit
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sum_misfit(directory,misfit_cur,NPROC_DATA)
    use seismo_parameters
    implicit none 
    integer :: ier,isrc,ip,NPROC_DATA
    real(kind=CUSTOM_REAL) :: temp,misfit_cur
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    misfit_cur = 0.0_CUSTOM_REAL

    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename 
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    print *,'misfit_cur for all=',misfit_cur

end subroutine sum_misfit

subroutine sum_misfit_mean(directory,misfit_cur,misfit_cur_f2,misfit_cur_f3,misfit_cur_f4,misfit_cur_f5,misfit_cur_f6,misfit_cur_f7,misfit_cur_mean2,misfit_cur_mean3,misfit_cur_mean4,misfit_cur_mean5,misfit_cur_mean6,misfit_cur_mean7,misfit_cur_std2,misfit_cur_std3,misfit_cur_std4,misfit_cur_std5,misfit_cur_std6,misfit_cur_std7,NPROC_DATA)
    use seismo_parameters
    implicit none
    integer :: irec,ier,isrc,ip,NPROC_DATA,inum,num,num_mean
    real(kind=CUSTOM_REAL) ::misfit_cur_std,misfit_cur_std2,misfit_cur_std3,misfit_cur_std4,misfit_cur_std5,misfit_cur_std6,misfit_cur_std7
    real(kind=CUSTOM_REAL) ::temp,misfit_cur_mean_save,misfit_cur,misfit_cur_mean,misfit_cur_mean1,misfit_cur_mean2,misfit_cur_mean3,misfit_cur_mean4,misfit_cur_mean5,misfit_cur_mean6,misfit_cur_mean7
    real(kind=CUSTOM_REAL) ::misfit_cur_f2,misfit_cur_f3,misfit_cur_f4,misfit_cur_f5,misfit_cur_f6,misfit_cur_f7  
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory
    integer :: nrec_process
    
    misfit_cur = 0.0_CUSTOM_REAL
    misfit_cur_mean = 0.0_CUSTOM_REAL
    num_mean=0 

    misfit_cur_mean2 = 0.0_CUSTOM_REAL
    misfit_cur_mean3 = 0.0_CUSTOM_REAL
    misfit_cur_mean4 = 0.0_CUSTOM_REAL
    misfit_cur_mean5 = 0.0_CUSTOM_REAL
    misfit_cur_mean6 = 0.0_CUSTOM_REAL
    misfit_cur_mean7 = 0.0_CUSTOM_REAL

    misfit_cur_f2 = 0.0_CUSTOM_REAL
    misfit_cur_f3 = 0.0_CUSTOM_REAL
    misfit_cur_f4 = 0.0_CUSTOM_REAL
    misfit_cur_f5 = 0.0_CUSTOM_REAL
    misfit_cur_f6 = 0.0_CUSTOM_REAL
    misfit_cur_f7 = 0.0_CUSTOM_REAL
    !! key
    nrec_process = 35

!!!!!!!!!!!!!!!!!!!!!!!!!! band F2 !!!!!!!!!!!!!!!!!!!! 
    do isrc=0,nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat2_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,'(F10.7,I4)') temp,num
    close(IIN)
    
   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num

    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean2=misfit_cur_mean/num_mean

!---------------misfit--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit2.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f2=misfit_cur_mean_save/num_mean
    print *,'nsrc=',nsrc
    print *,'num for f2 is:',num_mean,' misfit for f2 is:',misfit_cur_f2,'mean dt is:',misfit_cur_mean
!----------------------------------
!---------------std--------------
    misfit_cur_mean = 0.0_CUSTOM_REAL    
    misfit_cur_mean_save = 0.0_CUSTOM_REAL 
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat2.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
!    print *,'nsrc=',nrec_process,'std'
    do irec=1,nrec_process
        read(IIN,*) temp
!        print *, 'temp=',temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            misfit_cur_mean_save=misfit_cur_mean_save+ temp**2
            num=num+1
!            print *,'nrec_process=',nrec_process,'temp=',temp,'misfit_cur_mean2=',misfit_cur_mean2
        endif 
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std2=misfit_cur_std
!----------------------------------    

!!!!!!!!!!!!!!!!!!!!!!!!!! band F3 !!!!!!!!!!!!!!!!!!!! 

    misfit_cur_mean=0.0_CUSTOM_REAL
    num_mean=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat3_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,'(F10.7,I4)') temp,num
    close(IIN)

   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean3=misfit_cur_mean/num_mean

!---------------misfit--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit3.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f3=misfit_cur_mean_save/num_mean
    print *,'num for f3 is:',num_mean,' misfit for f3 is:',misfit_cur_f3,'mean dt is:',misfit_cur_mean
!----------------------------------

    misfit_cur_mean = 0.0_CUSTOM_REAL
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat3.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    do irec=1,nrec_process
        read(IIN,*) temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            misfit_cur_mean_save=misfit_cur_mean_save+ temp**2
            num=num+1
        endif
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std3=misfit_cur_std
!----------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!! band F4 !!!!!!!!!!!!!!!!!!!!
    misfit_cur_mean=0.0_CUSTOM_REAL
    num_mean=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat4_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,'(F10.7,I4)') temp,num
    close(IIN)

   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean4=misfit_cur_mean/num_mean
!---------------misfit--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit4.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f4=misfit_cur_mean_save/num_mean
    print *,'num for f4 is:',num_mean,' misfit for f4 is:',misfit_cur_f4,'mean dt is:',misfit_cur_mean
!----------------------------------
!---------------std----------------
    misfit_cur_mean = 0.0_CUSTOM_REAL
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat4.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    do irec=1,nrec_process
        read(IIN,*) temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            misfit_cur_mean_save=misfit_cur_mean_save+ temp**2
            num=num+1
        endif
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std4=misfit_cur_std
!----------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!! band F5 !!!!!!!!!!!!!!!!!!!!
    misfit_cur_mean=0.0_CUSTOM_REAL
    num_mean=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat5_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,'(F10.7,I4)') temp,num
    close(IIN)

   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num
    !enddo !! inum
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean5=misfit_cur_mean/num_mean

!---------------misfit--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit5.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f5=misfit_cur_mean_save/num_mean
    print *,'num for f5 is:',num_mean,' misfit for f5 is:',misfit_cur_f5,'mean dt is:',misfit_cur_mean
!----------------------------------
!---------------std----------------
    misfit_cur_mean = 0.0_CUSTOM_REAL
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat5.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    do irec=1,nrec_process
        read(IIN,*) temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            misfit_cur_mean_save=misfit_cur_mean_save+ temp**2
            num=num+1
        endif
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std5=misfit_cur_std
!----------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!! band F6 !!!!!!!!!!!!!!!!!!!!
    misfit_cur_mean=0.0_CUSTOM_REAL
    num_mean=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat6_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,'(F10.7,I4)') temp,num
        !read(IIN,'(F10.7,I5)') temp,num

    end if
    close(IIN)

   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num
    !enddo !! inum
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean6=misfit_cur_mean/num_mean

!---------------misfit6--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit6.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f6=misfit_cur_mean_save/num_mean
!----------------------------------
!---------------std----------------
    misfit_cur_mean = 0.0_CUSTOM_REAL
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat6.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    do irec=1,nrec_process
        read(IIN,*) temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            misfit_cur_mean_save=misfit_cur_mean_save+ temp**2
            num=num+1
        endif
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std6=misfit_cur_std
!----------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!! band F7 !!!!!!!!!!!!!!!!!!!!
    misfit_cur_mean=0.0_CUSTOM_REAL
    num_mean=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat7_all.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,'(F10.7,I4)') temp,num
        !read(IIN,'(F10.7,I5)') temp,num

    end if
    close(IIN)

   !! sum over source (chi-squared)
    misfit_cur_mean=misfit_cur_mean+temp
    num_mean=num_mean+num
    !enddo !! inum
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur_mean7=misfit_cur_mean/num_mean

!---------------misfit6--------------
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit7.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
        read(IIN,*) temp
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur_mean_save=misfit_cur_mean_save+temp
    enddo !! ip
    enddo !! source loop
    misfit_cur_f7=misfit_cur_mean_save/num_mean
!----------------------------------
!----------------------------------
    misfit_cur_mean = 0.0_CUSTOM_REAL
    misfit_cur_mean_save = 0.0_CUSTOM_REAL
    num=0
    do isrc=0, nsrc-1
    do ip=0,0
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_deltat7.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    do irec=1,nrec_process
        read(IIN,*) temp
        if (temp/=1.0000000E-06) then
            misfit_cur_mean=misfit_cur_mean+(temp-misfit_cur_mean2)**2
            num=num+1
        endif
    enddo
    enddo !! ip
    enddo !! source loop

    misfit_cur_mean=misfit_cur_mean/(num-1)
    misfit_cur_std=sqrt(misfit_cur_mean)
    misfit_cur_std7=misfit_cur_std
!----------------------------------

    !! all misfit 
    !! key
    misfit_cur = 0.0_CUSTOM_REAL
    !misfit_cur= (misfit_cur_f3+misfit_cur_f4+misfit_cur_f2)/3
    misfit_cur=(misfit_cur_f3+misfit_cur_f4+misfit_cur_f5+misfit_cur_f2)/4


end subroutine sum_misfit_mean

subroutine sum_misfit_all(directory,misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,NPROC_DATA)
    use seismo_parameters
    implicit none
    integer :: ier,isrc,ip,NPROC_DATA
    real(kind=CUSTOM_REAL) ::temp,misfit_cur,misfit_cur_2,misfit_cur_3,misfit_cur_4,misfit_cur_5,misfit_cur_6,misfit_cur_7,misfit_cur_average
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    misfit_cur = 0.0_CUSTOM_REAL

    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit2.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_2=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misfit_cur = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit3.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_3=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misfit_cur = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit4.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_4=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misfit_cur = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit5.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_5=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misfit_cur = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit6.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_6=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    misfit_cur = 0.0_CUSTOM_REAL
    ! source loop
    do isrc=0, nsrc-1
    do ip=0,NPROC_DATA-1
    ! open file 
    write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit7.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    if(ier>0) then
        print*,'Error opening event misfit file: ',filename
        stop
    else
        read(IIN,*) temp
    end if
    close(IIN)
    if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !! sum over source (chi-squared)
    misfit_cur=misfit_cur+temp
    enddo !! ip
    enddo !! source loop

    !! take average of nsrc
    misfit_cur=misfit_cur/nsrc
    misfit_cur_7=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !misfit_cur = 0.0_CUSTOM_REAL
    !! source loop
    !do isrc=0, nsrc-1
    !do ip=0,NPROC_DATA-1
    !! open file 
    !write(filename,'(a,i6.6,a,i6.6,a)') trim(directory)//'/',isrc,'/proc',ip,'_misfit.dat'
    !OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    !if(ier>0) then
    !    print*,'Error opening event misfit file: ',filename
    !    stop
    !else
    !    read(IIN,*) temp
    !end if
    !close(IIN)
    !if(DISPLAY_DETAILS) print*,'read misfit file ... '
    !if(DISPLAY_DETAILS) print*, 'myrank=',ip, ' misfit=', temp
    !!! sum over source (chi-squared)
    !misfit_cur=misfit_cur+temp
    !enddo !! ip
    !enddo !! source loop

    !!! take average of nsrc
    !misfit_cur=misfit_cur/nsrc
    !misfit_cur_average=misfit_cur
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !print *,'misfit_cur_average=',misfit_cur_average
    !print *,'misfit_cur_1-6=',(misfit_cur_7+misfit_cur_6+misfit_cur_5+misfit_cur_4+misfit_cur_3+misfit_cur_2)/6
    !print *,'misfit_cur_2=',misfit_cur_2
    !print *,'misfit_cur_3=',misfit_cur_3
    !print *,'misfit_cur_4=',misfit_cur_4
    !print *,'misfit_cur_5=',misfit_cur_5
    !print *,'misfit_cur_6=',misfit_cur_6
    !print *,'misfit_cur_7=',misfit_cur_7    

end subroutine sum_misfit_all



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine check_iteration(directory)
    use seismo_parameters
    implicit none
    integer ::  j,iter
    integer :: ier,i,niter
    real(kind=CUSTOM_REAL) :: temp(2)
    real(kind=CUSTOM_REAL) :: misfit_hist(iter_end)
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    misfit_hist=0.0_CUSTOM_REAL

    write(filename, "(a)") trim(directory)//'/misfit/data_misfit_hist.dat'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)

    j=0
    do i=1,iter_end+1
    read(IIN,*,iostat=ier) iter,misfit_hist(iter+1)
    if (ier/=0) exit
    j=j+1
    enddo
    close(IIN)
    niter = j

    ! absolute misfit small enough
    if(misfit_hist(niter)<=SMALL_VAL) then
        is_cont=0
        is_brak=1
        next_step_length=0.0
        print*, 'stop due to current misfit value is small enough :',misfit_hist(niter)
    else if(j>1) then
        ! relative to initial misfit
        if(misfit_hist(niter)/misfit_hist(1) <=misfit_ratio_initial) then 
            is_cont=0
            is_brak=1
            next_step_length=0.0
            print*, 'stop due to current misfit value is less than ',misfit_ratio_initial*100,'%',&
                ' relative to initial misfit :', &
                misfit_hist(niter)/misfit_hist(1)*100,'%'

            ! misfit reduction relative to previous iteration
        else if( (misfit_hist(niter-1) - misfit_hist(niter))/misfit_hist(niter-1) <=misfit_ratio_previous) then
            is_cont=0
            is_brak=1
            next_step_length=0.0
            print*, 'stop due to misfit reduction is less than ',misfit_ratio_previous*100,'%', &
                ' relative to previous iteration :',&
                (misfit_hist(niter-1) - misfit_hist(niter))/misfit_hist(niter-1) * 100, '%'
        endif
    endif

end subroutine check_iteration 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine check_linesearch(directory,iter)
    use seismo_parameters
    implicit none
    integer :: iter,j
    integer :: ier,i,step
    real(kind=CUSTOM_REAL) :: optimal_misfit
    real(kind=CUSTOM_REAL) :: temp(3)
    real(kind=CUSTOM_REAL) :: step_hist(max_step),misfit_hist(max_step)
    character(len=MAX_FILENAME_LEN) :: filename
    character(len=MAX_STRING_LEN) :: directory

    step_hist = 0.0_CUSTOM_REAL
    misfit_hist=0.0_CUSTOM_REAL  

    write(filename, "(a)") trim(directory)//'/misfit/data_misfit_hist_detail'
    OPEN (IIN,FILE= filename,STATUS='OLD',action='read',iostat=ier)
    j=0
    do i=1,(max_step+1)*iter
    read(IIN,*,iostat=ier) temp
    if (ier/=0) exit
    if(temp(1)==iter) then
        j=j+1
        step_hist(j)=temp(2)
        misfit_hist(j)=temp(3)
    endif
    enddo
    close(IIN)
    step=j

    print*,'misfit_hist at iter=',iter,',nstep=',step
    print*,'step_hist : ',step_hist(1:step)
    print*,'misfit_hist: ',misfit_hist(1:step)

    ! determine next step search status
    !  if(.not. backtracking) then
    print*,'line search method -- constant step size'

    if(step_length>=initial_step_length) then
        ! current status -- forward search     
        if(misfit_hist(step)<misfit_hist(step-1))  then    ! decrease misfit      
            ! two criteria : max_step; misfit reduction rate
            if(step<=max_step) then 
                ! next status -- forward continue
                is_cont=1
                is_done=0
                is_brak=0
                next_step_length=step_hist(step)+initial_step_length
                optimal_step_length=step_hist(step)
                write(*,'(a,f15.5)') 'next step : forward continue -- next step_length=',next_step_length

            else
                is_cont=0
                is_done=1
                is_brak=0
                next_step_length=0.0
                optimal_step_length=step_hist(step)
                optimal_misfit=misfit_hist(step)
                write(*,'(a,f15.5)') 'next step : forward stop -- exceed max step, optimal step_length=',optimal_step_length
            endif

        else   ! not decrease misfit
            if(step_length>=1.5*initial_step_length) then
                !! more than one step forwad,  next status -- forward stop
                is_cont=0
                is_done=1
                is_brak=0
                next_step_length=0.0
                optimal_step_length=step_hist(step-1)
                optimal_misfit=misfit_hist(step-1)
                write(*,'(a,f15.5)') 'next step : forward done -- optimal step_length=',optimal_step_length

            else    !! next status -- backward start
                is_cont=1
                is_done=0
                is_brak=0
                next_step_length=step_hist(step)/2
                optimal_step_length=0.0
                write(*,'(a,f15.5)') 'next step : backward start -- next step_length=',next_step_length
            endif
        endif

    else   ! current status -- backward search  
        if(misfit_hist(step)<misfit_hist(1)) then  ! next status -- backward stop
            is_cont=0
            is_done=1
            is_brak=0
            next_step_length=0.0
            optimal_step_length=step_hist(step)
            optimal_misfit=misfit_hist(step)
            write(*,'(a,f15.5)') 'next step : backward done -- optimal step_length=',optimal_step_length
        else
            if(step_length>min_step_length .and. step<=max_step) then !!  next status -- backward continue               
                is_cont=1
                is_done=0
                is_brak=0
                next_step_length=step_hist(step)/2
                optimal_step_length=0.0
                write(*,'(a,f15.5)') 'next step : backward continue -- next step_length=',next_step_length

            else    !!  next status -- break 
                is_cont=0
                is_done=0
                is_brak=1
                next_step_length=0.0
                optimal_step_length=0.0
                write(*,'(a)') 'next step : backward exit'
            endif
        endif
    endif

    if(is_done==1) then
        !! misfit hist for iteration 
        write(filename,'(a)') trim(directory)//'/misfit/data_misfit_hist.dat'
        OPEN (IOUT, FILE=filename,status='unknown',POSITION='APPEND')
        write(IOUT,'(I5,e15.8)') iter,optimal_misfit
        close(IOUT)

        ! check iteration for next step
        call check_iteration(directory)
    endif

end subroutine check_linesearch
