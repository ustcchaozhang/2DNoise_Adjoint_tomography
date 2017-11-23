program misfit_adjoint
    ! to calculate adjoint source and misfit
    ! yanhuay@princeton.edu

#ifdef USE_MPI
    use mpi
#endif

    use seismo_parameters
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    integer, parameter :: NARGS = 5
    integer ::  ier,i,icomp,irec,send_data_tag,receive_data_tag
    real(kind=CUSTOM_REAL), dimension(:,:), allocatable :: adj_master 
    character(len=MAX_STRING_LEN) :: data_names(MAX_DATA_NUM)
    character(len=MAX_STRING_LEN) :: data_names_comma_delimited
    character(len=MAX_STRING_LEN) :: arg(NARGS)
    character(len=MAX_STRING_LEN) :: input_dir
    character(len=MAX_FILENAME_LEN) :: filename
    real t1,t2
    real :: A_all, A,A1,A2,A3,A4,A5,A6,A7
#ifdef USE_MPI
    call MPI_INIT(ier)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nproc,ier)
    call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ier)
#else
    nproc = 1
    myrank = 0
#endif

    if (DISPLAY_DETAILS .and. myrank==0)&
        print *,"Running misfit_adjoint.exe on",NPROC,"processors"
    call cpu_time(t1)

    ! parse command line arguments
    if (command_argument_count() /= NARGS) then
        if (myrank == 0) then
            print *, 'USAGE:  mpirun -np NPROC bin/misfit_adjoint.exe ... ' 
            stop ' Please check command line arguments'
        endif
    endif

#ifdef USE_MPI
    call MPI_BARRIER(MPI_COMM_WORLD,ier)
#endif

    ! get input parameters
    do i = 1, NARGS
    call get_command_argument(i,arg(i), status=ier)
    enddo
    read(arg(1),*) compute_adjoint
    data_names_comma_delimited = arg(2)
    measurement_list=arg(3)
    misfit_type_list=arg(4)
    input_dir=arg(5)
    call split_string(data_names_comma_delimited,',',data_names,ndata)

    !! gloabl initialization
    misfit=0.0_CUSTOM_REAL
    misfit1=0.0_CUSTOM_REAL
    misfit2=0.0_CUSTOM_REAL
    misfit3=0.0_CUSTOM_REAL
    misfit4=0.0_CUSTOM_REAL
    misfit5=0.0_CUSTOM_REAL
    misfit6=0.0_CUSTOM_REAL
    misfit7=0.0_CUSTOM_REAL

    ! loop over comp
    do icomp=1,ndata

    ! step 1 -- load data and syn
    call initialize(input_dir,adjustl(data_names(icomp)))


    ! exist data/syn
    if(nrec_proc > 0 ) then  ! non-zero file

        if(norm2(seism_obs(:,:))<SMALL_VAL) cycle
        call process_data_all(seism_obs,'obs',seism_obs_mute,seism_obs1,seism_obs2,seism_obs3,seism_obs4,seism_obs5,seism_obs6,seism_obs7,seism_obs8,win1_obs,win2_obs)

        if(norm2(seism_syn(:,:))<SMALL_VAL) cycle
        call process_data_all(seism_syn,'syn',seism_syn_mute,seism_syn1,seism_syn2,seism_syn3,seism_syn4,seism_syn5,seism_syn6,seism_syn7,seism_syn8,win1_syn,win2_syn)
    

        if(compute_adjoint .and. DISPLAY_TRACES) then

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_mute.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_obs_mute(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_mute.bin'
            !print*,'SAVE processed seismic_syn -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_syn_mute(:,:)
            !close(IOUT)

            write(filename,'(a)') &
                trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed1.bin'
            print*,'SAVE processed seismic_obs -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_obs5(:,:)
            close(IOUT)

            write(filename,'(a)') &
                trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed1.bin'
            print*,'SAVE processed seismic_syn -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_syn5(:,:)
            close(IOUT)
            write(filename,'(a)') &
                trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed2.bin'
            print*,'SAVE processed seismic_obs -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_obs2(:,:)
            close(IOUT)
            write(filename,'(a)') &
                trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed2.bin'
            print*,'SAVE processed seismic_syn -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_syn2(:,:)
            close(IOUT)
            write(filename,'(a)') &
                trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed3.bin'
            print*,'SAVE processed seismic_obs -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_obs3(:,:)
            close(IOUT)

            write(filename,'(a)') &
                trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed3.bin'
            print*,'SAVE processed seismic_syn -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_syn3(:,:)
            close(IOUT)

            write(filename,'(a)') &
                trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed4.bin'
            print*,'SAVE processed seismic_obs -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_obs4(:,:)
            close(IOUT)

            write(filename,'(a)') &
                trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed4.bin'
            print*,'SAVE processed seismic_syn -- ',trim(filename)
            open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            if (ier /= 0) then
                print*, 'Error: could not open model file: ',trim(filename)
                stop 'Error reading neighbors external mesh file'
            endif
            write(IOUT) seism_syn4(:,:)
            close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed5.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_obs5(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed6.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_obs6(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_processed7.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_obs7(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed5.bin'
            !print*,'SAVE processed seismic_syn -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_syn5(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed6.bin'
            !print*,'SAVE processed seismic_syn -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_syn6(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_processed7.bin'
            !print*,'SAVE processed seismic_syn -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) seism_syn7(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_window1.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) win1_obs(:,:)
            !close(IOUT)

            !write(filename,'(a)') &
            !    trim(input_dir)//'/DATA_obs/'//trim(adjustl(data_names(icomp)))//'_window2.bin'
            !print*,'SAVE processed seismic_obs -- ',trim(filename)
            !open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
            !if (ier /= 0) then
            !    print*, 'Error: could not open model file: ',trim(filename)
            !    stop 'Error reading neighbors external mesh file'
            !endif
            !write(IOUT) win2_obs(:,:)
            !close(IOUT)

        endif


       ! step 3 -- misfit-adjoint
        num_AD=0
        num_AD1=0
        num_AD2=0
        num_AD3=0
        num_AD4=0
        num_AD5=0
        num_AD6=0
        num_AD7=0
        num_AD8=0

        misfit_AD=0.0_CUSTOM_REAL
        misfit_DD=0.0_CUSTOM_REAL
        misfit_AD1=0.0_CUSTOM_REAL
        misfit_DD1=0.0_CUSTOM_REAL
        misfit_AD2=0.0_CUSTOM_REAL
        misfit_DD2=0.0_CUSTOM_REAL
        misfit_AD3=0.0_CUSTOM_REAL
        misfit_DD3=0.0_CUSTOM_REAL
        misfit_AD4=0.0_CUSTOM_REAL
        misfit_AD5=0.0_CUSTOM_REAL
        misfit_AD6=0.0_CUSTOM_REAL
        misfit_AD7=0.0_CUSTOM_REAL
        misfit_AD8=0.0_CUSTOM_REAL

        
       


        ! absolute mwasurement  
        if(index(misfit_type_list,'AD')>0) then
            misfit_deltat = 0.0_CUSTOM_REAL
            !call Absolute_diff(seism_obs1,seism_syn1,Fmax1,Fmin1,Vp1)
            misfit_AD1= misfit_AD            
            seism_adj_AD1=seism_adj_AD
            num_AD1=num_AD
            if(DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat1.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)

            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat1_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD1
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)
            close(IOUT)


            endif            
             

            misfit_deltat = 0.0_CUSTOM_REAL
            call Absolute_diff(seism_obs2,seism_syn2,Fmax2,Fmin2,Vp2)            
            misfit_AD2= misfit_AD
            seism_adj_AD2=seism_adj_AD
            num_AD2=num_AD
            if( DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat2.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat2_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD2
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

          !  write(filename,'(a)') &
          !      trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj2_before.bin'
          !  print*,'SAVE processed seismic_obs -- ',trim(filename)
          !  open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
          !  write(IOUT) seism_adj_AD_before
          !  close(IOUT)

            endif



            misfit_deltat = 0.0_CUSTOM_REAL
            call Absolute_diff(seism_obs3,seism_syn3,Fmax3,Fmin3,Vp3)
            misfit_AD3= misfit_AD
            seism_adj_AD3=seism_adj_AD
            num_AD3=num_AD
            if(DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat3.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)

            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat3_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD3
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

          !  write(filename,'(a)') &
          !      trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj3_before.bin'
          !  print*,'SAVE processed seismic_obs -- ',trim(filename)
          !  open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
          !  write(IOUT) seism_adj_AD_before
          !  close(IOUT)

            endif


            misfit_deltat = 0.0_CUSTOM_REAL
            call Absolute_diff(seism_obs4,seism_syn4,Fmax4,Fmin4,Vp4)
            misfit_AD4= misfit_AD
            seism_adj_AD4=seism_adj_AD
            num_AD4=num_AD
            if( DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat4.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat4_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD4
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

          !  write(filename,'(a)') &
          !      trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj4_before.bin'
          !  print*,'SAVE processed seismic_obs -- ',trim(filename)
          !  open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
          !  write(IOUT) seism_adj_AD_before
          !  close(IOUT)

            endif

!!!! more band period !!!
            call Absolute_diff(seism_obs5,seism_syn5,Fmax5,Fmin5,Vp5)
            misfit_AD5= misfit_AD
            seism_adj_AD5=seism_adj_AD
            num_AD5=num_AD
            if( DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat5.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)


            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat5_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD5
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

            endif



            !call Absolute_diff(seism_obs6,seism_syn6,Fmax6,Fmin6,Vp6)
            misfit_AD6= misfit_AD
            seism_adj_AD6=seism_adj_AD
            num_AD6=num_AD
            if (DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat6.dat'

            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)


            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat6_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD6
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

            endif


            !call Absolute_diff(seism_obs7,seism_syn7,Fmax7,Fmin7,Vp7)
            misfit_AD7= misfit_AD
            seism_adj_AD7=seism_adj_AD
            num_AD7=num_AD
            if( DISPLAY_DETAILS) then
            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat7.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
            do irec=1,nrec_proc
                !print *,'misfit_deltat(irec)=',misfit_deltat(irec)
                write(IOUT,*) misfit_deltat(irec)
            enddo
            close(IOUT)

            write(filename,'(a,i6.6,a)') &
               trim(input_dir)//'/proc',myrank,'_deltat7_all.dat'
            OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)

                write(IOUT,'(F10.4,I4)') sum(misfit_deltat),num_AD7
                !print *, "sum(misfit_deltat)=",sum(misfit_deltat)

            close(IOUT)

            endif


       endif
        
 
        ! differential measurement
        if (nrec_proc>1 .and. index(misfit_type_list,'DD')>0) then
            call Relative_diff(input_dir,adjustl(data_names(icomp)),seism_obs1,seism_syn1)
            misfit_DD1= misfit_DD
            seism_adj_DD1=seism_adj_DD
            num_AD1=num_AD


          !if(compute_adjoint .and. DISPLAY_TRACES) then
          !write(filename,'(a,i6.6,a)') &
          !  trim(input_dir)//'/proc',myrank,'_deltat1.dat'
          !OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
          !if(ier/=0) then
          !  print*,'Error opening data misfit file: ',trim(filename)
          !  stop
          !else
          !  write(IOUT,*) misfit_deltat(:,:)
          !endif
          !close(IOUT)
          !endif 


            misfit_deltat = 0.0_CUSTOM_REAL
            call Relative_diff(input_dir,adjustl(data_names(icomp)),seism_obs2,seism_syn2)
            misfit_DD2= misfit_DD
            seism_adj_DD2=seism_adj_DD
            num_AD2=num_AD
            
           ! if(compute_adjoint .and. DISPLAY_TRACES) then
           ! write(filename,'(a,i6.6,a)') &
           !   trim(input_dir)//'/proc',myrank,'_deltat2.dat'
           ! OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
           ! if(ier/=0) then
           !   print*,'Error opening data misfit file: ',trim(filename)
           !   stop
           ! else
           !   write(IOUT,*) misfit_deltat(:,:)
           ! endif
           ! close(IOUT)
           ! endif


            call Relative_diff(input_dir,adjustl(data_names(icomp)),seism_obs3,seism_syn3)
            misfit_DD3= misfit_DD
            seism_adj_DD3=seism_adj_DD
            num_AD3=num_AD


           ! if(compute_adjoint .and. DISPLAY_TRACES) then
           ! write(filename,'(a,i6.6,a)') &
           !   trim(input_dir)//'/proc',myrank,'_deltat3.dat'
           ! OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
           ! if(ier/=0) then
           !   print*,'Error opening data misfit file: ',trim(filename)
           !   stop
           ! else
           !   write(IOUT,*) misfit_deltat(:,:)
           ! endif
           ! close(IOUT)
           ! endif 
 
!            if(DISPLAY_DETAILS .and. compute_adjoint) then
!                print*
!                print*, 'summed squared misfit_',trim(measurement_list),&
!                    '_DD=',misfit_DD
!                print*, trim(adjustl(data_names(icomp))), &
!                    ': total number of DD measurements :', &
!                    num_DD, num_DD*100.0/(nrec_proc*(nrec_proc-1)/2),'%'
!            endif
        endif


        !! misfit (AD+DD+...)

        misfit1=misfit1+ misfit_AD1        
        misfit2=misfit2+ misfit_AD2
        misfit3=misfit3+ misfit_AD3
        misfit4=misfit4+ misfit_AD4
        misfit5=misfit5+ misfit_AD5
        misfit6=misfit6+ misfit_AD6
        misfit7=misfit7+ misfit_AD7

        A=max(num_AD2,1)+max(num_AD3,1)+max(num_AD4,1)+max(num_AD5,1)+max(num_AD6,1)+max(num_AD7,1)    

        if(misfit_weighting) then
        A2= REAL(max(num_AD2,1)/A)
        A3= REAL(max(num_AD3,1)/A)
        A4= REAL(max(num_AD4,1)/A)
        A5= REAL(max(num_AD5,1)/A)
        A6= REAL(max(num_AD6,1)/A)
        A7= REAL(max(num_AD7,1)/A)
        else
        A_all=REAL(1.0/6.0)
        !print *,'A_all=',A_all 
        A2=A_all        
        A3=A_all
        A4=A_all
        A5=A_all
        A6=A_all
        A7=A_all
        endif  

        !print *,'A2=',A2,'A3=',A3,'A4=',A4,'A5=',A5,'A6=',A6,'A7=',A7
        misfit= 0.0_CUSTOM_REAL
        misfit=  misfit2+misfit3+misfit4+misfit5
        !misfit= misfit2*A2+misfit3*A3+misfit4*A4+misfit5*A5+misfit6*A6+misfit7*A7
        !misfit= (misfit2+misfit3+misfit4+misfit5+misfit6+misfit7)/6
        !print*,'misfit1=',misfit1,'num_AD1=',num_AD1
        print*,'misfit2=',misfit2,'num_AD2=',num_AD2
        print*,'misfit3=',misfit3,'num_AD3=',num_AD3
        print*,'misfit4=',misfit4,'num_AD4=',num_AD4
        print*,'misfit5=',misfit5,'num_AD5=',num_AD5
        !print*,'misfit6=',misfit6,'num_AD6=',num_AD6
        !print*,'misfit7=',misfit7,'num_AD7=',num_AD7
        print*,'misfit=',misfit

        !! adjoint
        if(compute_adjoint) then 
            print *,"computer the adjoint"
            seism_adj1 = seism_adj_AD1
            seism_adj2 = seism_adj_AD2
            seism_adj3 = seism_adj_AD3
            seism_adj4 = seism_adj_AD4
            seism_adj5 = seism_adj_AD5
            seism_adj6 = seism_adj_AD6
            seism_adj7 = seism_adj_AD7
            !!key step
            !seism_adj=seism_adj2*A2+seism_adj3*A3+seism_adj4*A4+seism_adj5*A5+seism_adj6*A6+seism_adj7*A7         
            !seism_adj =   seism_adj2+ seism_adj3+ seism_adj4 + seism_adj5+ seism_adj6 +seism_adj7
            !seism_adj =   seism_adj2*0.2+seism_adj3*0.8
            seism_adj =  (seism_adj2+seism_adj3+seism_adj4+seism_adj5)/4
            !seism_adj =   (seism_adj2+seism_adj3+seism_adj4)/3
            !seism_adj =   seism_adj3
            !seism_adj =  A*seism_adj1+B*seism_adj2+C*seism_adj3+D*seism_adj4

            call process_adj_all()
        endif

        !if(compute_adjoint .and. DISPLAY_TRACES) then

        !    write(filename,'(a)') &
        !        trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj1.bin'
        !    print*,'SAVE processed seismic_obs -- ',trim(filename)
        !    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
        !    if (ier /= 0) then
        !        print*, 'Error: could not open model file: ',trim(filename)
        !        stop 'Error reading neighbors external mesh file'
        !    endif
        !    write(IOUT) seism_adj1(:,:)
        !    close(IOUT)



        !    write(filename,'(a)') &
        !        trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj2.bin'
        !    print*,'SAVE processed seismic_obs -- ',trim(filename)
        !    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
        !    if (ier /= 0) then
        !        print*, 'Error: could not open model file: ',trim(filename)
        !        stop 'Error reading neighbors external mesh file'
        !    endif
        !    write(IOUT) seism_adj2(:,:)
        !    close(IOUT)

        !    write(filename,'(a)') &
        !        trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj3.bin'
        !    print*,'SAVE processed seismic_obs -- ',trim(filename)
        !    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
        !    if (ier /= 0) then
        !        print*, 'Error: could not open model file: ',trim(filename)
        !        stop 'Error reading neighbors external mesh file'
        !    endif
        !    write(IOUT) seism_adj3(:,:)
        !    close(IOUT)

        !    write(filename,'(a)') &
        !        trim(input_dir)//'/DATA_syn/'//trim(adjustl(data_names(icomp)))//'_adj4.bin'
        !    print*,'SAVE processed seismic_obs -- ',trim(filename)
        !    open(unit=IOUT,file=trim(filename),status='unknown',form='unformatted',iostat=ier)
        !    if (ier /= 0) then
        !        print*, 'Error: could not open model file: ',trim(filename)
        !        stop 'Error reading neighbors external mesh file'
        !    endif
        !    write(IOUT) seism_adj4(:,:)
        !    close(IOUT)

        !endif
        ! finalize
        call finalize(input_dir,adjustl(data_names(icomp)))

    endif ! nrec_proc>0
    enddo ! icomp

    ! step 5 -- save misfit
    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit
    endif
    close(IOUT)


    if(DISPLAY_DETAILS) then
    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit1.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit1
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit2.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit2
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit3.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit3
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit4.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit4
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit5.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit5
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit6.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit6
    endif
    close(IOUT)

    write(filename,'(a,i6.6,a)') &
        trim(input_dir)//'/proc',myrank,'_misfit7.dat'
    OPEN (IOUT, FILE=trim(filename),status='unknown',iostat = ier)
    if(ier/=0) then
        print*,'Error opening data misfit file: ',trim(filename)
        stop
    else
        write(IOUT,*) misfit7
    endif
    close(IOUT)
    endif
  
    if(DISPLAY_DETAILS .and. compute_adjoint .and. nrec_proc>0) then
        print*
        print*,'SAVE misfit -- ',trim(filename)
        print*,'myrank=',myrank,' final misfit_',trim(measurement_list), &
            '_',trim(misfit_type_list), '= ',misfit
    endif

    call cpu_time(t2)
    if(DISPLAY_DETAILS .and. compute_adjoint .and. myrank==0) &
        print *,'Computation time with CPU:',t2-t1

#ifdef USE_MPI
    ! stop all the processes and exit
    call MPI_FINALIZE(ier)
#endif

end program misfit_adjoint
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine initialize(directory,data_name)
    use seismo_parameters
    implicit none
    integer :: ier,iker,imod,irec,itime
    integer :: filesize, nrec_obs, nrec_syn
    logical :: ex_obs, ex_syn
    character(len=MAX_FILENAME_LEN) :: char_i, filen, filename_obs, filename_syn
    character(len=MAX_FILENAME_LEN) :: filename_snr
    character(len=MAX_STRING_LEN) :: directory
    character(len=MAX_STRING_LEN) :: data_name
    character(len=MAX_FILENAME_LEN) :: stf_filename,filename
    logical :: ex_stf
    real(kind=CUSTOM_REAL), dimension(:,:),allocatable :: temp
    integer :: irow, ncolum

    !! data file format
    filen='empty'
    select case(solver)
    case('specfem2D')   ! single file
        if(myrank==0 .and. data_name == 'x') filen='Ux_file_single.su'
        if(myrank==0 .and. data_name == 'y') filen='Uy_file_single.su'
        if(myrank==0 .and. data_name == 'z') filen='Uz_file_single.su'
        if(myrank==0 .and. data_name == 'p') filen='Up_file_single.su'

    case('specfem3D')  
        write(char_i, '(I5)') myrank        ! convert integer to char
        if(data_name == 'x') write(filen,'(a)') trim(adjustl(char_i))//'_dx_SU'
        if(data_name == 'y') write(filen,'(a)') trim(adjustl(char_i))//'_dy_SU'
        if(data_name == 'z') write(filen,'(a)') trim(adjustl(char_i))//'_dz_SU'
        if(data_name == 'p') write(filen,'(a)') trim(adjustl(char_i))//'_dp_SU'

    case default
        print*,'Currently only work for specfem2D and specfem3D solver'
        stop
    end select

    !! load data & syn
    nrec_proc=0
    write(filename_obs,'(a)') &
        trim(directory)//'/DATA_obs/'//trim(filen)
    write(filename_syn,'(a)') &
        trim(directory)//'/DATA_syn/'//trim(filen)
    !print *,'filename_syn=',filename_syn
    !! by chao
    !write(filename_snr,'(a)') &
    !    trim(directory)//'/DATA_snr//'//trim('SRNID_5_7')
    !print *,'filename_snr=',filename_snr

    !exist and size
    inquire (file=trim(filename_obs), exist=ex_obs)
    inquire (file=trim(filename_syn), exist=ex_syn)



    if(ex_obs .and. ex_syn) then ! exist
        inquire(file=trim(filename_obs), size=filesize)
        nrec_obs=filesize/(240+4*NSTEP) 
        inquire(file=trim(filename_syn), size=filesize)
        nrec_syn=filesize/(240+4*NSTEP)
        if(nrec_obs == nrec_syn) then
            nrec_proc=nrec_obs
            if(DISPLAY_DETAILS) then
                print*,'myrank=',myrank,' LOAD nrec_proc traces : ',nrec_proc
                print*,'seism_obs -- ',trim(filename_obs)
                print*,'seism_syn -- ',trim(filename_syn)
            endif
        else
            print*,'size for obs and syn file is not the same : ',nrec_obs, nrec_syn
            stop
        endif

        !! allocate 
        allocate(seism_obs(NSTEP,nrec_proc))
        allocate(seism_obs_mute(NSTEP,nrec_proc))
        allocate(seism_obs1(NSTEP,nrec_proc))
        allocate(seism_obs2(NSTEP,nrec_proc))
        allocate(seism_obs3(NSTEP,nrec_proc))
        allocate(seism_obs4(NSTEP,nrec_proc))
        allocate(seism_obs5(NSTEP,nrec_proc))
        allocate(seism_obs6(NSTEP,nrec_proc))
        allocate(seism_obs7(NSTEP,nrec_proc))
        allocate(seism_obs8(NSTEP,nrec_proc))

        allocate(win1_obs(NSTEP,nrec_proc))
        allocate(win2_obs(NSTEP,nrec_proc))
        allocate(win1_syn(NSTEP,nrec_proc))
        allocate(win2_syn(NSTEP,nrec_proc))
  
    
      


        allocate(seism_syn(NSTEP,nrec_proc))
        allocate(seism_syn_mute(NSTEP,nrec_proc))
        allocate(seism_syn1(NSTEP,nrec_proc))
        allocate(seism_syn2(NSTEP,nrec_proc))
        allocate(seism_syn3(NSTEP,nrec_proc))
        allocate(seism_syn4(NSTEP,nrec_proc))
        allocate(seism_syn5(NSTEP,nrec_proc))
        allocate(seism_syn6(NSTEP,nrec_proc))
        allocate(seism_syn7(NSTEP,nrec_proc))
        allocate(seism_syn8(NSTEP,nrec_proc))
        allocate(misfit_deltat(nrec_proc))

        allocate(seism_adj(NSTEP,nrec_proc))
        allocate(seism_adj1(NSTEP,nrec_proc))
        allocate(seism_adj2(NSTEP,nrec_proc))
        allocate(seism_adj3(NSTEP,nrec_proc))
        allocate(seism_adj4(NSTEP,nrec_proc))
        allocate(seism_adj5(NSTEP,nrec_proc))
        allocate(seism_adj6(NSTEP,nrec_proc))
        allocate(seism_adj7(NSTEP,nrec_proc))
        allocate(seism_adj8(NSTEP,nrec_proc))

        allocate(seism_adj_AD(NSTEP,nrec_proc))
        allocate(seism_adj_AD_before(NSTEP,nrec_proc))
        allocate(seism_adj_AD1(NSTEP,nrec_proc))
        allocate(seism_adj_AD2(NSTEP,nrec_proc))
        allocate(seism_adj_AD3(NSTEP,nrec_proc))
        allocate(seism_adj_AD4(NSTEP,nrec_proc))
        allocate(seism_adj_AD5(NSTEP,nrec_proc))
        allocate(seism_adj_AD6(NSTEP,nrec_proc))
        allocate(seism_adj_AD7(NSTEP,nrec_proc))
        allocate(seism_adj_AD8(NSTEP,nrec_proc))

        allocate(seism_adj_DD(NSTEP,nrec_proc))
        allocate(seism_adj_DD1(NSTEP,nrec_proc))
        allocate(seism_adj_DD2(NSTEP,nrec_proc))
        allocate(seism_adj_DD3(NSTEP,nrec_proc))






        allocate(st_xval(nrec_proc))
        allocate(st_yval(nrec_proc))
        allocate(st_zval(nrec_proc))
        allocate(win_start(nrec_proc))
        allocate(win_end(nrec_proc))
        allocate(trace_norm(nrec_proc))
        allocate(time(NSTEP))
        allocate(dis_sr(nrec_proc))



        !allocate(which_proc_receiver(NPROC))

        ! initialization
        seism_obs = 0.0_CUSTOM_REAL
        seism_obs_mute = 0.0_CUSTOM_REAL
        seism_obs1 = 0.0_CUSTOM_REAL
        seism_obs2 = 0.0_CUSTOM_REAL
        seism_obs3 = 0.0_CUSTOM_REAL
        seism_obs4 = 0.0_CUSTOM_REAL
        seism_obs5 = 0.0_CUSTOM_REAL
        seism_obs6 = 0.0_CUSTOM_REAL
        seism_obs7 = 0.0_CUSTOM_REAL
        seism_obs8 = 0.0_CUSTOM_REAL

        win1_obs = 0.0_CUSTOM_REAL
        win2_obs = 0.0_CUSTOM_REAL
        win1_syn = 0.0_CUSTOM_REAL
        win2_syn = 0.0_CUSTOM_REAL

        seism_syn = 0.0_CUSTOM_REAL
        seism_syn_mute = 0.0_CUSTOM_REAL
        seism_syn1 = 0.0_CUSTOM_REAL
        seism_syn2 = 0.0_CUSTOM_REAL
        seism_syn3 = 0.0_CUSTOM_REAL
        seism_syn4 = 0.0_CUSTOM_REAL
        seism_syn5 = 0.0_CUSTOM_REAL
        seism_syn6 = 0.0_CUSTOM_REAL
        seism_syn7 = 0.0_CUSTOM_REAL
        seism_syn8 = 0.0_CUSTOM_REAL
        misfit_deltat = 0.0_CUSTOM_REAL

        seism_adj = 0.0_CUSTOM_REAL

        seism_adj_AD = 0.0_CUSTOM_REAL
        seism_adj_AD_before = 0.0_CUSTOM_REAL
        seism_adj_AD1 = 0.0_CUSTOM_REAL
        seism_adj_AD2 = 0.0_CUSTOM_REAL
        seism_adj_AD3 = 0.0_CUSTOM_REAL
        seism_adj_AD4 = 0.0_CUSTOM_REAL
        seism_adj_AD5 = 0.0_CUSTOM_REAL
        seism_adj_AD6 = 0.0_CUSTOM_REAL
        seism_adj_AD7 = 0.0_CUSTOM_REAL
        seism_adj_AD8 = 0.0_CUSTOM_REAL
        seism_adj_DD = 0.0_CUSTOM_REAL
        seism_adj_DD1 = 0.0_CUSTOM_REAL
        seism_adj_DD2 = 0.0_CUSTOM_REAL
        seism_adj_DD3 = 0.0_CUSTOM_REAL




        win_start=NSTEP
        win_end=1
        event_norm=1.0
        trace_norm=1.0
        do itime=1,NSTEP
        time(itime)=(itime-1)*deltat+t0
        enddo
        ! which_proc_receiver=0

        ! allocate obs and syn
        call readSU(filename_obs,seism_obs)
        call readSU(filename_syn,seism_syn)
        do irec=1,nrec_proc
        dis_sr(irec)=sqrt((st_xval(irec)-x_source)**2 &            
            +(st_yval(irec)-y_source)**2 &                                            
            +(st_zval(irec)-z_source)**2)
                
       
       !!!!!!!by chao,normalize the EGF according the SGF: !EGF*max(SGF)/max(EGF)!!!!!!!!
       !d(:)=d(:)*maxval(s(:))/maxval(d(:))
       seism_obs(:,irec)=seism_obs(:,irec)*maxval(seism_syn(:,irec))/maxval(seism_obs(:,irec))

       !! by chao

        enddo


        if(DISPLAY_DETAILS) then
            print*,'Min / Max of seism_obs : ',&
                minval(seism_obs(:,:)),maxval(seism_obs(:,:))
            print*,'Min / Max of seism_syn : ',&
                minval(seism_syn(:,:)),maxval(seism_syn(:,:))
        endif
    endif ! exist

end subroutine initialize
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine process_data_all(seism,tag,seism_mute,seism1,seism2,seism3,seism4,seism5,seism6,seism7,seism8,win1,win2)

    use seismo_parameters
    implicit none

    integer :: ier,irec
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(kind=CUSTOM_REAL) :: seism(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism_mute(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism1(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism2(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism3(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism4(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism5(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism6(NSTEP,nrec_proc)

    real(kind=CUSTOM_REAL) :: seism7(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: seism8(NSTEP,nrec_proc)
    !!window
    real(kind=CUSTOM_REAL) :: win1(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: win2(NSTEP,nrec_proc)

    !!bandpass
    real(kind=CUSTOM_REAL) :: trace(NSTEP)
    real(kind=CUSTOM_REAL) :: trace_mute(NSTEP)
    real(kind=CUSTOM_REAL) :: trace1(NSTEP)
    real(kind=CUSTOM_REAL) :: trace2(NSTEP)
    real(kind=CUSTOM_REAL) :: trace3(NSTEP)
    real(kind=CUSTOM_REAL) :: trace4(NSTEP)
    real(kind=CUSTOM_REAL) :: trace5(NSTEP)
    real(kind=CUSTOM_REAL) :: trace6(NSTEP)
    real(kind=CUSTOM_REAL) :: trace7(NSTEP)
    real(kind=CUSTOM_REAL) :: trace8(NSTEP)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    character(len=3) :: tag
    real(kind=CUSTOM_REAL) :: wtr
    real(kind=8) :: deltat1
    !real(kind=CUSTOM_REAL) :: taper_len

    integer :: istart,iend,nps
    integer :: itime
    integer :: NA
    real(kind=CUSTOM_REAL) :: tas(NSTEP)

    !! Event normalization
    if(EVENT_NORMALIZE) then
        wtr=norm2(seism(:,:))
        if(wtr<SMALL_VAL) stop
        if (DISPLAY_DETAILS) print*,'Event normalization ',tag, wtr
        seism=seism/wtr
        if(tag=='syn') event_norm=wtr
    endif

    !! trace-wise processing
    do irec = 1,nrec_proc 
    !! by chao
    !!do irec = nrec_proc,nrec_proc

    trace(:) = seism(:,irec)
    ! initialization
    istart=1
    iend=NSTEP

    tstart=0.0
    tend=(NSTEP-1)*deltat

    wtr=norm2(trace(istart:iend))
    if(wtr>SMALL_VAL) then  ! non-zero trace
        !! trace normalization 
        if(TRACE_NORMALIZE) then
            if (DISPLAY_DETAILS .and. irec==1) print*,'Trace normalization ', tag
            trace=trace/wtr 
            if(tag=='syn') trace_norm(irec)=wtr
        endif
        ! WT filtering
        if( Wscale .gt. 0.and. istart<iend) then
            call WT(trace,NSTEP,Wscale,NA)
        endif

        !! mute near offset 
        if(MUTE_NEAR .and. dis_sr(irec)<=offset_near ) then
            trace(1:NSTEP) = 0.0
            istart=NSTEP
            iend=0
            tstart=0.0
            tend=0.0
        endif
        !! muter far offset
        if(MUTE_FAR .and. dis_sr(irec)>=offset_far) then
            trace(1:NSTEP) =0.0
            istart=NSTEP
            iend=0
            tstart=0.0
            tend=0.0
        endif
        !! laplace damping spatially and temporally 
        if (DAMPING .and. istart<iend) then
            ! spatial
            trace=trace*exp(-(dis_sr(irec)*lambda_x)**2)
            ! temporal
            trace(1:NSTEP)=trace(1:NSTEP)*exp(-(time(1:NSTEP)*lambda_t)**2)
        endif

        ! time-domain window using slopes 
        if(TIME_WINDOW .and. istart<iend) then
            istart=max(int((T0_TOP+dis_sr(irec)/VEL_TOP-taper_len)/deltat),istart)
            iend=min(int((T0_BOT+dis_sr(irec)/VEL_BOT+4*taper_len)/deltat),iend)

            tas(1:NSTEP)=0.d0
            if(iend>istart) then
                call window(NSTEP,istart,iend,window_type,tas)
            else
                istart=NSTEP
                iend=1
            endif
            trace=trace*tas
        endif ! window
        !!maybe it can print
        win_start(irec)=istart*deltat
        win_end(irec)=iend*deltat


!        ! time-domain window using slopes 
!        if(TIME_WINDOW) then
!            tstart=dis_sr(irec)/VEL_TOP
!            tend=dis_sr(irec)/VEL_BOT
!            if(tend>tstart) then
!                taper_len = taper_percentage/2.0 * (tend-tstart)
!                tstart=max(tstart-taper_len,0.0)
!                tend=min(tend+taper_len,(NSTEP-1)*deltat)
!                !!by chao
!                !istart=floor(tstart/deltat) 
!                !iend=floor(tend/deltat)
!                istart=3410
!                iend=7886
!                tstart=istart*deltat
!                tend=iend*deltat
!                nps=iend-istart+1
!
!
!                print *,'tstart=',tstart,'istart=',istart,'tend=',tend,'iend=',iend,'deltat=',deltat
!
!                tas(1:NSTEP)=0.d0
!                call window_mute(nps,istart,iend,taper_percentage,taper_type,tas)
!                !call window(NSTEP,istart,iend,window_type,tas) 
!            else
!                tstart=NSTEP*deltat
!                tend=1*deltat
!            endif
!
!            !! window by chao
!                trace=trace*tas
!
!
!        endif ! window
!        ! save
!        print *,'dis_sr(irec)=',dis_sr(irec),'VEL_TOP=',VEL_TOP,'VEL_BOT=',VEL_BOT,'tstart=',tstart,'tend=',tend
!        win_start(irec)=tstart
!        win_end(irec)=tend
              

        ! output the windows information
        win1(:,irec)=win_start(irec) 
        win2(:,irec)=win_end(irec)

        !! bandpass using SAC
        trace1(:)=trace(:)
        trace2(:)=trace(:)
        trace3(:)=trace(:)
        trace4(:)=trace(:)
        trace5(:)=trace(:)
        trace6(:)=trace(:)
        trace7(:)=trace(:)
        trace8(:)=trace(:)
        if(IS_BANDPASS) then
             deltat1=dble(deltat)
             !bandpass
             call bandpass(trace1,NSTEP,1/Fmin1,1/Fmax1,deltat1)
             call bandpass(trace2,NSTEP,1/Fmin2,1/Fmax2,deltat1)
             call bandpass(trace3,NSTEP,1/Fmin3,1/Fmax3,deltat1)
             call bandpass(trace4,NSTEP,1/Fmin4,1/Fmax4,deltat1)
             call bandpass(trace5,NSTEP,1/Fmin5,1/Fmax5,deltat1)
             call bandpass(trace6,NSTEP,1/Fmin6,1/Fmax6,deltat1)
             call bandpass(trace7,NSTEP,1/Fmin7,1/Fmax7,deltat1)

            ! trace1=trace1*tas
            ! trace2=trace2*tas
            ! trace3=trace3*tas
            ! trace4=trace4*tas
            ! trace5=trace5*tas
            ! trace6=trace6*tas
            ! trace7=trace7*tas
            ! trace8=trace8*tas       
              
        endif

    endif ! non-zero trace
   
  
    seism_mute(:,irec)=trace(:)
    seism1(:,irec)=trace1(:)
    seism2(:,irec)=trace2(:)
    seism3(:,irec)=trace3(:)
    seism4(:,irec)=trace4(:)
    seism5(:,irec)=trace5(:)
    seism6(:,irec)=trace6(:)
    seism7(:,irec)=trace7(:)
    seism8(:,irec)=trace8(:)
    enddo ! irec

end subroutine process_data_all
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine process_adj_all()
    use seismo_parameters
    implicit none
    integer :: irec
    real(kind=CUSTOM_REAL) :: adj(NSTEP), adj_vel(NSTEP), adj_acc(NSTEP)
    character(len=3) :: tag
    real(kind=CUSTOM_REAL) :: wtr
    real(kind=8) :: deltat1

    integer :: istart,iend
    integer :: itime
    integer :: NA
    real(kind=CUSTOM_REAL) :: tas(NSTEP)


        !! post-processing of seism_adj
        do irec=1,nrec_proc
        adj(:)=seism_adj(:,irec)
        !! windows for adjoint
        !if(TIME_WINDOW .and. istart<iend) then

        !    !print *,"time window for adjoint source"
        !    istart=max(int((T0_TOP+dis_sr(irec)/VEL_TOP-taper_len)/deltat),istart)
        !    !iend=min(int((T0_BOT+dis_sr(irec)/VEL_BOT+taper_len)/deltat),iend)
        !    !!!!!!!!!!!!!!!!!!!!!!modify by me!!!!!!!!!!!!!!!!!!

        !    iend=min(int((T0_BOT+dis_sr(irec)/VEL_BOT+4*taper_len)/deltat),iend)

        !    tas(1:NSTEP)=0.d0
        !    if(iend>istart) then
        !        call window(NSTEP,istart,iend,window_type,tas)
        !    else
        !        istart=NSTEP
        !        iend=1
        !    endif
        !    adj=adj*tas
        !endif ! window


        if(norm2(adj(:))<SMALL_VAL) cycle
        ! trace normalize
        if(TRACE_NORMALIZE) adj=adj/trace_norm(irec)
        ! seismotype
        if(DISPLAY_DETAILS .and. irec==1) print*, trim(seismotype) ,' adjoint'
        if(trim(seismotype) == "velocity") then
            call compute_vel(adj,NSTEP,deltat,NSTEP,adj_vel)
            seism_adj(:,irec)= - adj_vel(:)
        elseif(trim(seismotype) == "acceleration") then
            call compute_acc(adj,NSTEP,deltat,NSTEP,adj_acc)
            seism_adj(:,irec)=adj_acc(:)
        endif
        enddo
        ! event normalize
        if(EVENT_NORMALIZE) seism_adj=seism_adj/event_norm

end subroutine process_adj_all
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine process_adj_trace(trace,istart,iend,dis,Fmax,Fmin)
    use seismo_parameters
    implicit none
    real(kind=CUSTOM_REAL) :: trace(NSTEP)
    integer :: istart,iend,nps
    real(kind=CUSTOM_REAL) :: dis
    real(kind=CUSTOM_REAL) :: wtr
    integer :: NA
    integer :: itime
    real(kind=CUSTOM_REAL) :: tas(NSTEP)
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: stf_reverse
    real(kind=8) :: deltat1
    real(kind=8) :: Fmin
    real(kind=8) :: Fmax
    nps=iend-istart+1
    !print *,'nps=',nps

    wtr=norm2(trace(istart:iend))
    !print *,'nps=',nps,'wtr=',wtr,'SMALL_VAL=',SMALL_VAL

    if(wtr>SMALL_VAL) then  ! non-zero trace
        ! time-domain window using slopes 
      !  if(TIME_WINDOW) then
      !     ! window
      !      tas(1:NSTEP)=0.d0
      !      print *,'before window',trace(istart+200)
      !      call window(NSTEP,istart,iend,window_type,tas)
      ! 
      !      !call window_mute(nps,istart,iend,taper_percentage,taper_type,tas)
      !               
      !      trace=trace*tas
      !      print *,'after window=',trace(istart+200)
      !  endif
        !! laplace damping spatially and temporally 
     !   if (DAMPING) then
     !       ! spatial
     !       trace=trace*exp(-(dis*lambda_x)**2)
     !       ! temporal
     !       trace(1:NSTEP)=trace(1:NSTEP)*exp(-(time(1:NSTEP)*lambda_t)**2)
     !   endif
     !   !! mute near offset 
     !   if(MUTE_NEAR .and. dis<=offset_near) then
     !       trace(1:NSTEP) = 0.0
     !   endif
     !   !! mute far offset
     !   if(MUTE_FAR .and. dis>=offset_far) then                                                      
     !       trace(1:NSTEP) = 0.0
     !   endif
     !   ! WT filtering
     !   if( Wscale .gt. 0) then
     !       call WT(trace,NSTEP,Wscale,NA)
     !   endif
        ! bandpass for adjoint source
        if(IS_BANDPASS) then
             deltat1=dble(deltat)
             !print *,'Fmin=',Fmin,'Fmax=',Fmax
             call bandpass(trace,NSTEP,1/Fmin,1/Fmax,deltat1)
        endif

    endif ! non-zero trace

end subroutine process_adj_trace
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  `  
subroutine Absolute_diff(obs,syn,Tc,Te,Vp)
    use seismo_parameters
    implicit none
    integer :: irec, itype, ntype

    real(kind=CUSTOM_REAL) :: seism(NSTEP,nrec_proc)

    real(kind=CUSTOM_REAL) :: obs(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: syn(NSTEP,nrec_proc)

    real(kind=CUSTOM_REAL) :: d(NSTEP),s(NSTEP),adj_trace(NSTEP),adj(NSTEP)
    real(kind=CUSTOM_REAL) :: wtr

    real(kind=8) :: Tc,Vp,dismin,Te
    real(kind=8) :: std_temp,mean_temp,std

    real(kind=CUSTOM_REAL) ::misfit_value,misfit_value_before,misfit_trace,misfit_value_all,misfit_all
    integer :: ntstart,ntend,istart,iend,iconst,npoint
    integer :: ishift,t
    real(kind=CUSTOM_REAL) :: dlnA,cc_max
    character(len=MAX_STRING_LEN) :: measurement_types(MAX_MISFIT_TYPE)
    character(len=2) :: measurement_type
    character, parameter :: delimiter='+'

    integer :: nlen,num,num_measure
    real(kind=CUSTOM_REAL), dimension(NSTEP) ::E_d,E_s,E_ratio,hilbt_ratio,hilbt_d,hilbt_s
    real(kind=CUSTOM_REAL) :: const

    ! error
    real(kind=CUSTOM_REAL) ::  err_CC
    ! initialization

    d = 0.0_CUSTOM_REAL
    s = 0.0_CUSTOM_REAL
    num_measure=0
    num = 0

    ! by chao 
    num_AD=0
    num_DD=0
    misfit_AD=0.0_CUSTOM_REAL
    misfit_DD=0.0_CUSTOM_REAL
    seism_adj_AD = 0.0_CUSTOM_REAL
    seism_adj_DD = 0.0_CUSTOM_REAL
    t=0
    misfit_value_all=0.0_CUSTOM_REAL   

    call split_string(measurement_list,delimiter,measurement_types,ntype)
    if(ntype>mtype) then 
        print*,'number of measurement_types exceeds number of measurement_weight: ',&
            ntype, mtype
        stop
    endif
    ! key
    do irec=1,nrec_proc
    !do irec=34,34
    ! get data 
    d(:)=obs(:,irec)
    s(:)=syn(:,irec)

    !!!!!!!by chao,normalize the EGF according the SGF: EGF*max(SGF)/max(EGF)!!!!!!!!
    !! norm1:
    if( maxval(abs(d(:))) .eq. minval(abs(d(:))) ) then
    !print *,'maxval(abs(d(:)))=',maxval(abs(d(:)))
    !print *,'minval(abs(d(:)))=',minval(abs(d(:)))    
    d(:)=0.0
    s(:)=0.0
    endif

    d(:)=d(:)*maxval(s(:))/maxval(d(:))
    !! norm2:
    !s(:)=s(:)/maxval(abs(s(:)))
    !d(:)=d(:)/maxval(abs(d(:)))

    !! misfit and adjoint evaluation
    ! initialization
    adj_trace = 0.0_CUSTOM_REAL
    misfit_trace=0.0_CUSTOM_REAL
    hilbt_s=0.0_CUSTOM_REAL
    E_s=0.0_CUSTOM_REAL

    ! window info
    ! mute the hann window according to the envelope max time!!!
    ! hilbert transform of s

    if(Tc<=40) then
  
         hilbt_s(:) = s(:)
         call hilbert(hilbt_s,NSTEP)
    ! envelope
         E_s(:) = sqrt(s(:)**2+hilbt_s(:)**2)
    ! arrival time estimated from peak of envelopes
         const=MAXLOC(abs(E_s),1)*deltat
    ! calculate the tstart and tend
         tstart = const-2.5*(Tc+Te)/2
         tend = const+2.5*(Tc+Te)/2    
             
    else  
        tstart = win_start(irec)
        tend= win_end(irec)

    endif


    istart=max(floor(tstart / deltat),1)
    iend=min(floor(tend / deltat),NSTEP)
    iconst=floor(const / deltat)
    !print *,'istart=',istart,'iend=',iend,'iconst=',iconst
    !print *,'tstart=',tstart,'tend=',tend


    if((tend-tstart)<min_window_len) cycle


    do itype=1,ntype
    ! initialization
    misfit_value=0.0
    adj(:)=0.d0
    misfit_value=0.0
    misfit_all=0.0
    measurement_type=trim(measurement_types(itype))


    call misfit_adj_AD(measurement_type,d,s,NSTEP,&
        deltat,f0,tstart,tend,taper_percentage,taper_type,&
        compute_adjoint, &
        adj,num,misfit_all,misfit_value,Tc,Te,irec,err_CC)


    !misfit_value =sqrt(2*misfit_value)
    !misfit_value=0.1
    misfit_value_before =misfit_value
    print *,'irec=',irec,'Tc=',Tc,'Te=',Te,'DT=',misfit_value,'Misfit=',misfit_all

    !!by chao
    ! for ambient noise,satisfy d>1.5Tc*Vp

    dismin=2*(Tc+Te)/2*Vp*1000;
    print *,'min dismin=',dismin
    if(Tc<=8) then
        !!  test the dis_sr(irec) should be > offset_far
        !if(abs(misfit_value)>=Tc/2 .or. dis_sr(irec)<offset_far .or. st_xval(irec)-x_source<0  ) then
        if(abs(misfit_value)>=Tc/2 .or. dis_sr(irec)<offset_far  ) then
        !if(abs(misfit_value)>=Tc/2 .or. dis_sr(irec)<50000  ) then
          !print *, 'dismin=',dismin,'dis_sr(irec)=',dis_sr(irec),'st_xval(irec)=',st_xval(irec),'x_source=',x_source
          !print *,'abs(misfit_value)=',abs(misfit_value),'Tc/3=',Tc/3
          misfit_value=0.000001
          misfit_all=0.d0
          adj(:)=0.d0
          err_CC=1.0
          t=t+1
        endif

    else 
        !! by chao: just use the data once
        !if(abs(misfit_value)>4 .or. dis_sr(irec)<dismin .or. st_xval(irec)-x_source<0) then
        if(abs(misfit_value)>=4 .or. dis_sr(irec)<offset_far) then
        !if(abs(misfit_value)>=6 .or. dis_sr(irec)<dismin  ) then
          !print *, 'dismin=',dismin, 'dis_sr(irec)=',dis_sr(irec)
          !print *,'abs(misfit_value)=',abs(misfit_value),'Tc/3=',Tc/3
          misfit_value=0.000001
          misfit_all=0.d0
          adj(:)=0.d0
          err_CC=1.0
          t=t+1
          !print *,'t=',t
        endif
    endif

    num_AD =nrec_proc-t;
    !print *,'num_AD=',num_AD,'nrec_proc=',nrec_proc,'t=',t
    !!! test which will be deleted 
    if(DISPLAY_DETAILS) then
        !print*,'istart, iend,iconst=',istart,iend,iconst,misfit_value,irec
        open(4,file='window_env',status='unknown')
        write(4,'(2F10.6)') misfit_value,misfit_value_before
    endif


    ! save the deltat diff
    misfit_deltat(irec)=misfit_value
    !! weighting the misfit according to the std(deltat) for different band
    std=1.0


    if(IS_SIGMA) then
    if(Tc==Fmax2) then
      std_temp=std2
      mean_temp=mean2
      if(abs(misfit_value-mean_temp)>2*std_temp) then
         std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
         print *,'abs(misfit_value-mean_temp)>2*std_temp',misfit_value-mean_temp,'2*std_temp=',2*std_temp,'std=',std
      else 
         std=std_temp
         print *,'abs(misfit_value-mean_temp)<2*std_temp',misfit_value-mean_temp,'2*std_temp=',2*std_temp,'std=',std
      endif 
    endif 

    if(Tc==Fmax3) then
      std_temp=std3
      mean_temp=mean3
      if(abs(misfit_value-mean_temp)>2*std_temp) then
         !print
         std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
      else
         std=std_temp
      endif
    endif

    if(Tc==Fmax4) then
      std_temp=std4
      mean_temp=mean4
      if(abs(misfit_value-mean_temp)>2*std_temp) then
         !print
         !*,'abs(misfit_value-mean_temp)=',misfit_value,mean_temp,'2*std_temp=',2*std_temp

         std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
      else
         std=std_temp
      endif
    endif

    if(Tc==Fmax5) then
      std_temp=std5
      mean_temp=mean5
      if(abs(misfit_value-mean_temp)>2*std_temp) then
         !print
         !*,'abs(misfit_value-mean_temp)=',misfit_value,mean_temp,'2*std_temp=',2*std_temp
         std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
      else
         std=std_temp
      endif
    endif

   ! if(Tc==Fmax6) then
   !   std_temp=std6
   !   mean_temp=mean6
   !   if(abs(misfit_value-mean_temp)>2*std_temp) then
   !      !print
   !      !*,'abs(misfit_value-mean_temp)=',misfit_value,mean_temp,'2*std_temp=',2*std_temp
   !      std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
   !   else
   !      std=std_temp
   !   endif
   ! endif

   ! if(Tc==Fmax7) then
   !   std_temp=std7
   !   mean_temp=mean7
   !   if(abs(misfit_value-mean_temp)>2*std_temp) then
   !      !print
   !      !*,'abs(misfit_value-mean_temp)=',misfit_value,mean_temp,'2*std_temp=',2*std_temp
   !      std=std_temp*exp((abs(misfit_value-mean_temp)/(2*std_temp))**2-1)
   !   else
   !      std=std_temp
   !   endif
   ! endif
    endif
    ! sum over itype of misfit and adjoint
    ! misfit_trace = misfit_trace + (misfit_value/std)**2*measurement_weight(itype)
    misfit_trace = misfit_trace + misfit_all/(std**2)
    !print *,'misfit_trace=',misfit_trace,'std=',std,'mean_temp=',mean_temp
    
    if(compute_adjoint) adj_trace = adj_trace + (adj/std**2)*measurement_weight(itype)

    enddo !itype     



    if (ntype>=1) then
        ! window sum 
        misfit_AD = misfit_AD + misfit_trace / ntype
        if(compute_adjoint) then
            !seism_adj_AD_before(:,irec) = seism_adj_AD(:,irec) + adj_trace(:) / ntype
            call process_adj_trace(adj_trace(:),istart,iend,dis_sr(irec),Tc,Te)
            seism_adj_AD(:,irec) = seism_adj_AD(:,irec) + adj_trace(:) / ntype

        endif
        !num_AD = num_AD +1
    endif

    enddo ! irec

end subroutine Absolute_diff
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine Relative_diff(input_dir,data_name,obs,syn)
    use seismo_parameters
    implicit none
    integer :: irec,jrec,itype, ntype,ier
    character(len=MAX_STRING_LEN) :: input_dir
    character(len=MAX_STRING_LEN) :: data_name
    character(len=MAX_FILENAME_LEN) :: filename
    logical :: ex
    real(kind=CUSTOM_REAL) :: seism(NSTEP,nrec_proc)

    real(kind=CUSTOM_REAL) :: obs(NSTEP,nrec_proc)
    real(kind=CUSTOM_REAL) :: syn(NSTEP,nrec_proc)


    real(kind=CUSTOM_REAL) :: d(NSTEP),s(NSTEP),adj(NSTEP),adj_pair(NSTEP)
    real(kind=CUSTOM_REAL) :: d_ref(NSTEP),s_ref(NSTEP),adj_ref(NSTEP),adj_ref_pair(NSTEP)
    real(kind=CUSTOM_REAL) :: dis_sr1, dis_sr2, dis_rr
    real(kind=CUSTOM_REAL) :: cc_max_obs
    real(kind=CUSTOM_REAL) :: misfit_value,misfit_pair,misfit_all
    integer :: ntstart,ntend,nlen
    integer :: ntstart_ref,ntend_ref,nlen_ref

    character(len=MAX_STRING_LEN) :: measurement_types(MAX_MISFIT_TYPE)
    character(len=2) :: measurement_type
    character, parameter :: delimiter='+'

    call split_string(measurement_list,delimiter,measurement_types,ntype)

    ! initialization
    d = 0.0_CUSTOM_REAL
    s = 0.0_CUSTOM_REAL
    d_ref = 0.0_CUSTOM_REAL
    s_ref = 0.0_CUSTOM_REAL
    cc_max_obs = 0.0

    ! by chao 
    num_AD=0
    num_DD=0
    misfit_AD=0.0_CUSTOM_REAL
    misfit_DD=0.0_CUSTOM_REAL
    seism_adj_AD = 0.0_CUSTOM_REAL
    seism_adj_AD_before = 0.0_CUSTOM_REAL
    seism_adj_DD = 0.0_CUSTOM_REAL



    allocate(is_pair(nrec_proc,nrec_proc))
    is_pair=0

    write(filename,'(a)') trim(input_dir)//'DATA_obs/'//trim(data_name)//'.similarity.dat'
    ! if existence, read, otherwise, write 
    inquire (file=trim(filename), exist=ex)
    OPEN (UNIT=IIN, FILE=filename,iostat=ier)
    do while(ier==0)
    read(IIN,*,iostat=ier) irec,jrec,cc_max_obs,is_pair(irec,jrec)
    enddo

    ! loop over master trace
    do irec=1,nrec_proc

    ! get data 
    d(:)=obs(:,irec)
    s(:)=syn(:,irec)

    dis_sr1=dis_sr(irec)

    ! window info
    ntstart = win_start(irec)
    ntend = win_end(irec)
    nlen=ntend-ntstart+1
    if(nlen<1 .or. nlen>NSTEP) cycle

    if(norm2(d(ntstart:ntend),1)<SMALL_VAL .or. &
        norm2(s(ntstart:ntend),1)<SMALL_VAL) cycle ! zero  master trace
    ! loop over reference trace
    do jrec=irec+1,nrec_proc
    ! get data 
    d_ref(:)=seism_obs(:,jrec)
    s_ref(:)=seism_syn(:,jrec)

    ! window info
    ntstart_ref = win_start(jrec)
    ntend_ref= win_end(jrec)
    nlen_ref=ntend_ref-ntstart_ref+1
    if(nlen_ref<1 .or. nlen_ref>NSTEP) cycle

    if(norm2(d_ref(ntstart_ref:ntend_ref),1)<SMALL_VAL .or. &
        norm2(s_ref(ntstart_ref:ntend_ref),1)<SMALL_VAL) cycle  ! zero reference trace

    if(.not. ex) then
        cc_max_obs=0.0
        dis_rr=sqrt((st_xval(jrec)-st_xval(irec))**2 &
            +(st_yval(jrec)-st_yval(irec))**2 &
            +(st_zval(jrec)-st_zval(irec))**2)
        if(dis_rr<=DD_max .and. dis_rr>=DD_min)&
            call CC_similarity(d,d_ref,NSTEP,&
            ntstart,ntend,ntstart_ref,ntend_ref,window_type,cc_max_obs)
        if(cc_max_obs>cc_threshold)  is_pair(irec,jrec) = 1
    endif !! ex

    if(DISPLAY_DETAILS .and. compute_adjoint) then
        print*      
        print*,'pair irec=',irec, 'jrec=',jrec           
        print*,'window -- ',ntstart,ntend,ntstart_ref,ntend_ref
        print*, 'rr distance dis_rr, DD_min/DD_max: ',dis_rr, DD_min, DD_max 
        print*, 'cc similarity -- ', cc_max_obs 
        print*,'is_pair : ',is_pair(irec,jrec)
    endif 

    if(is_pair(irec,jrec)==1) then
        ! initialization
        misfit_pair = 0.0
        adj_pair = 0.0
        adj_ref_pair = 0.0

        dis_sr2=dis_sr(jrec)

        ! number of double difference measurements
        num_DD = num_DD+1

        do itype=1,ntype
        ! initialization
        misfit_value = 0.0
        adj = 0.0
        adj_ref = 0.0

        measurement_type=trim(measurement_types(itype))

        call misfit_adj_DD(measurement_type,d,d_ref,s,s_ref,NSTEP,deltat,f0,&
            ntstart,ntend,ntstart_ref,ntend_ref,window_type,compute_adjoint,&
            misfit_value,adj,adj_ref)

        ! sum over itype of misfit and adjoint
        ! save the deltat for CC or MT

        !misfit_deltat(:,irec)=misfit_value

        misfit_pair=misfit_pair+misfit_value**2


        if(DISPLAY_DETAILS .and. compute_adjoint) then
            print*,'misfit_',trim(measurement_type),'_DD=',misfit_value
            print*,'squared misfit_',trim(measurement_type),'_DD=',misfit_value**2
        endif

        if(compute_adjoint) then
            adj_pair=adj_pair+adj
            adj_ref_pair=adj_ref_pair+adj_ref
        endif

        enddo ! itype

        if (ntype>=1) then 

            ! sum of misfit over all stations
            misfit_DD=misfit_DD+ misfit_pair/ntype

            if(compute_adjoint) then
                call process_adj_trace(adj_pair,ntstart,ntend,dis_sr1)
                call process_adj_trace(adj_ref_pair,ntstart_ref,ntend_ref,dis_sr2)

                if(DISPLAY_DETAILS) then
                    print*, 'Min/Max of adj :',minval(adj_pair(:)),maxval(adj_pair(:))
                    print*, 'Min/Max of adj_ref:',minval(adj_ref_pair(:)),maxval(adj_ref_pair(:))
                endif

                ! sum of adjoint source over pair
                seism_adj_DD(:,irec) = seism_adj_DD(:,irec) +  adj_pair(:)/ntype
                seism_adj_DD(:,jrec) = seism_adj_DD(:,jrec) + adj_ref_pair(:)/ntype
            endif
        endif

        !! save waveform similarity
        if(.not. ex) write(IIN,'(2I5,1e15.5,I5)') irec,jrec,cc_max_obs,is_pair(irec,jrec)

    endif  ! is_pair

    enddo  ! jrec 
    enddo ! irec 

    close(IIN) ! close similarity file

    deallocate(is_pair)

end subroutine Relative_diff
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine finalize(directory,data_name)
    use seismo_parameters
    implicit none
    character(len=MAX_STRING_LEN) :: data_name
    character(len=MAX_FILENAME_LEN) :: char_i,filen,filename
    character(len=MAX_STRING_LEN) :: directory

    !! write SU adj
    if(compute_adjoint) then
        !! adjoint file format
        select case(solver)
        case('specfem2D')
            if(myrank==0 .and. data_name == 'x') filen='Ux_file_single.su.adj'
            if(myrank==0 .and. data_name == 'y') filen='Uy_file_single.su.adj'
            if(myrank==0 .and. data_name == 'z') filen='Uz_file_single.su.adj'
            if(myrank==0 .and. data_name == 'p') filen='Up_file_single.su.adj'

        case('specfem3D')
            write(char_i, '(I5)') myrank        ! convert integer to char
            if(data_name == 'x') write(filen,'(a)') trim(adjustl(char_i))//'_dx_SU.adj'
            if(data_name == 'y') write(filen,'(a)') trim(adjustl(char_i))//'_dy_SU.adj'
            if(data_name == 'z') write(filen,'(a)') trim(adjustl(char_i))//'_dz_SU.adj'
            if(data_name == 'p') write(filen,'(a)') trim(adjustl(char_i))//'_dp_SU.adj'

        case default
            print*,'Currently only work for specfem2D and specfem3D solver'
            stop
        end select

        !! by chao, need to be deleted after modify
        !write(filename,'(a)') &
        !    trim(directory)//'/SEM1/'//trim(filen)
        !call writeSU(filename,seism_adj1) 

        !write(filename,'(a)') &
        !    trim(directory)//'/SEM2/'//trim(filen)
        !call writeSU(filename,seism_adj2)

        !write(filename,'(a)') &
        !    trim(directory)//'/SEM3/'//trim(filen)
        !call writeSU(filename,seism_adj3)

        !write(filename,'(a)') &
        !    trim(directory)//'/SEM4/'//trim(filen)
        !call writeSU(filename,seism_adj4)


        !! write adjoint source
        write(filename,'(a)') &
            trim(directory)//'/SEM/'//trim(filen)
        if(DISPLAY_DETAILS) then
            print*
            print*,'SAVE seism_adj -- ',trim(filename)
            if(DISPLAY_DETAILS) print*,'Min / Max of final seism_adj : ',&
                minval(seism_adj(:,:)),maxval(seism_adj(:,:))
        endif

        call writeSU(filename,seism_adj)
    endif

    deallocate(seism_obs)
    deallocate(seism_obs_mute)
    deallocate(seism_obs1)
    deallocate(seism_obs2)
    deallocate(seism_obs3)
    deallocate(seism_obs4)
    deallocate(seism_obs5)
    deallocate(seism_obs6)

    deallocate(win1_obs)
    deallocate(win2_obs)
    deallocate(win1_syn)
    deallocate(win2_syn)

    deallocate(seism_syn)
    deallocate(seism_syn_mute)
    deallocate(seism_syn1)
    deallocate(seism_syn2)
    deallocate(seism_syn3)
    deallocate(seism_syn4)
    deallocate(seism_syn5)
    deallocate(seism_syn6)
    deallocate(seism_syn7)
    deallocate(seism_syn8)
    deallocate(misfit_deltat)

    deallocate(seism_adj)
    deallocate(seism_adj1)
    deallocate(seism_adj2)
    deallocate(seism_adj3)
    deallocate(seism_adj4)
    deallocate(seism_adj5)
    deallocate(seism_adj6)
    deallocate(seism_adj7)
    deallocate(seism_adj8)

    deallocate(seism_adj_AD)
    deallocate(seism_adj_AD_before)
    deallocate(seism_adj_AD1)
    deallocate(seism_adj_AD2)
    deallocate(seism_adj_AD3)
    deallocate(seism_adj_AD4)
    deallocate(seism_adj_AD5)
    deallocate(seism_adj_AD6)
    deallocate(seism_adj_AD7)
    deallocate(seism_adj_AD8)

    deallocate(seism_adj_DD)
    deallocate(seism_adj_DD1)
    deallocate(seism_adj_DD2)
    deallocate(seism_adj_DD3)





    deallocate(st_xval)
    deallocate(st_yval)
    deallocate(st_zval)
    deallocate(win_start)
    deallocate(win_end)
    deallocate(trace_norm)
    deallocate(time)
    deallocate(dis_sr)

end subroutine finalize
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine readSU(filename,seism)
    use seismo_parameters
    implicit none
    integer :: ier,irec
    real(kind=CUSTOM_REAL) :: seism(NSTEP,nrec_proc)
    character(len=MAX_FILENAME_LEN) :: filename

    seism = 0.0_CUSTOM_REAL
    ! open(IIN,file=trim(filename),access='direct',recl=240+4*NSTEP,iostat = ier)
    open(IIN,file=trim(filename),status='old',form='unformatted',&
        access='direct',recl=240+4*NSTEP,iostat = ier)

    if (ier /= 0) then
        print*, 'Error: could not open data file: ',trim(filename)
        stop
    endif

    do irec = 1, nrec_proc
    read(IIN,rec=irec,iostat=ier) r4head, seism(:,irec)
    ! header info
    z_source=r4head(13) ! Source depth below surface (sdepth) 
    x_source=r4head(19) ! Source x coord (sx)
    y_source=r4head(20) ! Source y coord  (sy)
    st_zval(irec)=r4head(11) ! Receiver group elevation (gelev)
    st_xval(irec)=r4head(21) ! Receiver x coord (gx)
    st_yval(irec)=r4head(22) ! Receiver y coord (gy)
    ! header2=int(r4head(29), kind=2)
    if (DISPLAY_DETAILS .and. irec==1) print *, 'xs,ys,zs',&
        x_source,y_source,z_source
    if (DISPLAY_DETAILS .and. irec==1) print *, 'xr,yr,zr',&
        st_xval(irec),st_yval(irec),st_zval(irec)

    enddo

    close(IIN)

end subroutine readSU
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine writeSU(filename,seism)
    use seismo_parameters
    implicit none
    integer :: ier,irec,itime
    real(kind=CUSTOM_REAL) :: seism(NSTEP,nrec_proc)
    character(len=MAX_FILENAME_LEN) :: filename
    integer :: deltat_int2

    open(IOUT,file=trim(filename),status='unknown',access='direct',recl=4,iostat=ier)
    if (ier /= 0) then
        print*, 'Error: could not open data file: ',trim(filename)
        stop
    endif

    do irec = 1, nrec_proc
    ! write header
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+1)  irec !receiver ID
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+10) NINT(st_xval(irec)-x_source)  ! offset
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+19) NINT(x_source)                ! source location xs
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+20) NINT(y_source)                ! source location ys
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+21) NINT(st_xval(irec))           ! receiver location xr
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+22) NINT(st_yval(irec))           ! receiver location zr
    if (nrec_proc>1) write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+48) SNGL(st_xval(2)-st_xval(1)) ! receiver interval
    header2(1)=0  ! dummy
    header2(2)=int(NSTEP, kind=2)
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+29) header2
    header2(1)=NINT(deltat*1.0d6, kind=2) ! deltat (unit: 10^{-6} s)
    header2(2)=0  ! dummy
    write(IOUT,rec=(irec-1)*60+(irec-1)*NSTEP+30) header2

    ! the "60" in the following corresponds to 240 bytes header (note the
    ! reclength is 4 bytes)
    do itime = 1, NSTEP
    write(IOUT,rec=irec*60+(irec-1)*NSTEP+itime) sngl(seism_adj(itime,irec))
    enddo
    enddo ! irec

    close(IOUT)

end subroutine writeSU
