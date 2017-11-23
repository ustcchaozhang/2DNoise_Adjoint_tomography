!! subroutines 
!! created by Yanhua O. Yuan ( yanhuay@princeton.edu)

! =================================================================================================
! subroutine mt_measure()
! Boxcar/Cosine/Multitaper estimates of the phase function between dat1 and dat2 (dat1-dat2)
!
!  Input:
!        dat1_dt(:), dat2_dt(:), t0, dt, npts -- original dat1 and dat2 array
!        tstart, tend -- start and end of the measurement window (can be from Alessia's code)
!  Output:
!        istart -- starting index of the windowed portion of  original trace
!        dat1_dtw(:), dat2_dtw(:), nlen -- windowed and shifted dat1, windowed dat2
!        tshift, dlnA, cc_max -- time shift and amplitude cross-correlation measurements
!        i_right -- the maximum reliable frequency estimate index
!        dtau_w(:), dlnA_w(:) -- estimates of travel-time and amplitude anomaly
!        err_dt(:), err_dlnA(:) -- error bar of the travel-time and amplitude estimates (MT only)
!
! =================================================================================================

subroutine mt_measure(dat1,dat2,npts,deltat,nlen,tshift_cc,dlnA_cc, i_fstart,i_fend,&
        wvec,&
        trans_func,dtau_w,dlnA_w,err_dtau,err_dlnA)
    use constants
    implicit none

    integer, intent(in) :: nlen, npts
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dat1,dat2
    real(kind=CUSTOM_REAL), intent(in) ::  deltat,tshift_cc,dlnA_cc 
    integer, intent(in) :: i_fstart,i_fend
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: wvec
    complex (CUSTOM_REAL), dimension(NPT), intent(out) :: trans_func
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: dtau_w,dlnA_w,err_dtau,err_dlnA


    !! fft -- double size
    ! FFT parameters 
    real(kind=SIZE_DOUBLE) :: ampmax,wtr_mtm_use,wtr_use
    integer :: i,ictaper,iom

    !! fft to estimate transfer function
    ! tapered data in time domain
    real(kind=CUSTOM_REAL), dimension(NPT) :: dat1_dtw_h,dat2_dtw_h
    ! fft domain
    complex (SIZE_DOUBLE), dimension(NPT) :: dat2_dtwo, dat1_dtwo, &
        dat2_dtw_ho, dat1_dtw_ho,  &
        top_mtm,bot_mtm
    !! transfer function to phase/amplitude measurement
    real(kind=CUSTOM_REAL), dimension(NPT) :: phi_mtm,dtau_mtm,abs_mtm,dlnA_mtm
    real(kind=CUSTOM_REAL), dimension(:,:),allocatable :: phi_mul,dtau_mul,abs_mul,dlnA_mul

    !! jackknife error
    real(kind=CUSTOM_REAL) :: edt_ave,edt_iom,edA_ave,edA_iom

    !-------------------------------------------------------------

    if ( nlen > npts .or. nlen <1) then
        print*, nlen, npts
        stop 'Check nlen'
    endif

    !-------------------------------------------------------------------------------
    ! multitaper estimation of transfer function 
    !-------------------------------------------------------------------------------
    !! estimate transfer function from mtm 
    ! initialize transfer function terms
    top_mtm(:)   = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)
    bot_mtm(:)   = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)
    trans_func(:) = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)

    do ictaper = 1, ntaper

    dat2_dtw_ho(:) = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE) ! note: this has to be initialized inside the loop
    dat1_dtw_ho(:) = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)

    ! apply time-domain taper
    do i = 1, nlen
    dat2_dtw_h(i) = dat2(i) * real(tapers(i,ictaper),CUSTOM_REAL)     ! single-tapered, windowed data2
    dat1_dtw_h(i) = dat1(i) * real(tapers(i,ictaper),CUSTOM_REAL)  ! single-tapered, windowed, shifted data1
    enddo

    dat2_dtw_ho(1:nlen) = dcmplx(dat2_dtw_h(1:nlen),0.0_SIZE_DOUBLE)
    dat1_dtw_ho(1:nlen) = dcmplx(dat1_dtw_h(1:nlen),0.0_SIZE_DOUBLE)

    ! apply FFT to get complex spectra
    call fft(LNPT,dat2_dtw_ho,dble(FORWARD_FFT),dble(deltat))
    call fft(LNPT,dat1_dtw_ho,dble(FORWARD_FFT),dble(deltat))

    if(DISPLAY_DETAILS .and. ictaper==1) then
        open(1,file=trim(output_dir)//'/fft_in',status='unknown')
        open(2,file=trim(output_dir)//'/fft_out',status='unknown')
        do i = 1, nlen
        write(1,'(3E15.5)') i,dat1_dtw_h(i),dat2_dtw_h(i)
        enddo
        do  i =  i_fstart,i_fend
        write(2,'(3E15.5)') wvec(i)/TWOPI, abs(dat1_dtw_ho(i)),abs(dat2_dtw_ho(i))
        enddo
        close(1)
        close(2)
    endif

    ! calculate top and bottom of transfer function 
    do i = i_fstart,i_fend
    top_mtm(i) = top_mtm(i) + dat1_dtw_ho(i) * conjg(dat2_dtw_ho(i))   
    bot_mtm(i) = bot_mtm(i) + dat2_dtw_ho(i) * conjg(dat2_dtw_ho(i))  
    enddo

    enddo  ! ictapers

    ! water level for transfer function
    ampmax = maxval(abs(bot_mtm))

    ! LQY -- is this too small ???
    wtr_mtm_use = ampmax * wtr_mtm**2

    ! calculate MT transfer function using water level
    do i =  i_fstart,i_fend
    if(abs(bot_mtm(i)) > abs(wtr_mtm_use)) trans_func(i) = cmplx(top_mtm(i) / bot_mtm(i))
    if(abs(bot_mtm(i)) < abs(wtr_mtm_use)) trans_func(i) = cmplx(top_mtm(i) / (bot_mtm(i)+wtr_mtm_use))
    !trans_func(i) = cmplx(top_mtm(i) / (bot_mtm(i)+wtr_mtm_use))
    enddo

    if(DISPLAY_DETAILS) then
        ! print*
        ! print*,'water level for multi taper transfer function is : ',wtr_use
        ! fft of untapered inputs 
        dat1_dtwo = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)
        dat2_dtwo = cmplx(0.0_SIZE_DOUBLE,0.0_SIZE_DOUBLE)
        dat1_dtwo(1:nlen) = dcmplx(dat1(1:nlen),0.0)
        dat2_dtwo(1:nlen) = dcmplx(dat2(1:nlen),0.0)
        call fft(LNPT,dat1_dtwo,dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,dat2_dtwo,dble(FORWARD_FFT),dble(deltat))
        open(1,file=trim(output_dir)//'/top_bot_mtm',status='unknown')
        open(2,file=trim(output_dir)//'/trans_func',status='unknown')
        open(3,file=trim(output_dir)//'/trans_error',status='unknown')
        do  i =  i_fstart,i_fend
        write(1,'(3E15.5)') wvec(i)/TWOPI, abs(top_mtm(i)),abs(bot_mtm(i))
        write(2,'(2E15.5)') wvec(i)/TWOPI, abs(trans_func(i))
        write(3,'(4E15.5)') wvec(i)/TWOPI, abs(dat1_dtwo(i)),abs(dat2_dtwo(i)),abs(dcmplx(trans_func(i))*dat2_dtwo(i))
        enddo
        close(1)
        close(2)
        close(3)
    endif

    !! phase and amplitude measurement derived from transfer functions
    phi_mtm(:) = 0.d0
    abs_mtm(:) = 0.d0
    dtau_mtm(:) = 0.d0
    dlnA_mtm(:) = 0.d0
    call write_trans(trans_func, wvec, i_fstart,i_fend,tshift_cc,dlnA_cc,phi_mtm, abs_mtm,dtau_mtm, dlnA_mtm)

    ! pass results to main routine
    dtau_w(i_fstart:i_fend) = dtau_mtm(i_fstart:i_fend)
    dlnA_w(i_fstart:i_fend) = dlnA_mtm(i_fstart:i_fend)

    !-------------------------------------------------------------------------------
    ! multitaper error estimation
    !-------------------------------------------------------------------------------

    ! by chao,
    !print *,'ntaper=',ntaper
    if (ntaper > 1 .and. USE_ERROR_MT) then

        ! allocate Jacknife MT estimates
        allocate(phi_mul(NPT,ntaper))
        allocate(dtau_mul(NPT,ntaper))
        allocate(abs_mul(NPT,ntaper))
        allocate(dlnA_mul(NPT,ntaper))

        do iom = 1, ntaper

        top_mtm(:) = cmplx(0.,0.)
        bot_mtm(:) = cmplx(0.,0.)

        do ictaper = 1, ntaper
        if(ictaper.eq.iom) cycle

        ! apply ictaper-th taper
        dat2_dtw_h(1:nlen) = dat2(1:nlen) * real(tapers(1:nlen,ictaper),CUSTOM_REAL)
        dat1_dtw_h(1:nlen) = dat1(1:nlen) * real(tapers(1:nlen,ictaper),CUSTOM_REAL)

        ! complex tapered series
        dat2_dtw_ho(:) = cmplx(0.,0.)
        dat1_dtw_ho(:) = cmplx(0.,0.)
        dat2_dtw_ho(1:nlen) = dcmplx(dat2_dtw_h(1:nlen),0.)
        dat1_dtw_ho(1:nlen) = dcmplx(dat1_dtw_h(1:nlen),0.)

        ! apply f.t. to get complex spectra
        call fft(LNPT,dat2_dtw_ho,dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,dat1_dtw_ho,dble(FORWARD_FFT),dble(deltat))

        ! calculate top and bottom of Jacknife transfer function
        do i = i_fstart,i_fend
        top_mtm(i) = top_mtm(i) + dat1_dtw_ho(i) * conjg(dat2_dtw_ho(i))
        bot_mtm(i) = bot_mtm(i) + dat2_dtw_ho(i) * conjg(dat2_dtw_ho(i))
        enddo
        enddo ! ictaper

        ! water level
        ampmax = maxval(abs(bot_mtm))
        wtr_use = cmplx(ampmax * wtr_mtm ** 2, 0.)
        wtr_mtm_use = ampmax * 0.01

        !  calculate transfer function using water level
        do i = i_fstart,i_fend
        !if(abs(bot_mtm(i)).gt.abs(wtr_use)) trans_func(i) = top_mtm(i) / bot_mtm(i)
        !if(abs(bot_mtm(i)).le.abs(wtr_use)) trans_func(i) = top_mtm(i) /(bot_mtm(i)+wtr_use)
        trans_func(i) = cmplx(top_mtm(i) /(bot_mtm(i)+wtr_use))
        enddo

        call write_trans(trans_func, wvec,i_fstart,i_fend,tshift_cc,dlnA_cc,&
            phi_mul(:,iom),abs_mul(:,iom),dtau_mul(:,iom), dlnA_mul(:,iom))

        enddo ! iom

        !----------------------
        err_dtau(1:NPT)   = 0.
        err_dlnA(1:NPT)   = 0.

        do i = i_fstart,i_fend

        edt_ave   = 0.
        edA_ave   = 0. 

        do iom = 1, ntaper
        edt_iom = ntaper*dtau_mtm(i) - (ntaper-1)*dtau_mul(i,iom)
        edt_ave = edt_ave + edt_iom

        edA_iom = ntaper*dlnA_mtm(i) - (ntaper-1)*dlnA_mul(i,iom)
        edA_ave = edA_ave + edA_iom

        enddo

        edt_ave   = edt_ave   / (ntaper)
        edA_ave   = edA_ave   / (ntaper)

        do iom = 1, ntaper
        err_dlnA(i)  = err_dlnA(i) + ( dlnA_mul(i,iom) - edA_ave)**2
        err_dtau(i)   = err_dtau(i)  + (dtau_mul(i,iom) - edt_ave)**2
        enddo

        err_dlnA(i)  =  sqrt( err_dlnA(i) / (ntaper * (ntaper-1) ) )
        err_dtau(i)   =  sqrt( err_dtau(i) / (ntaper * (ntaper-1) ) )
        ! set the error bar for the first point corresponding to
        ! static offset to be large, which makes no contribution to
        ! the adjoint source
        !  if (i == 1) err_dt(i) = LARGE_VAL

        enddo ! i_fstart

        deallocate(phi_mul)
        deallocate(dtau_mul)
        deallocate(abs_mul)
        deallocate(dlnA_mul)

        if(DISPLAY_DETAILS) then
            open(1,file=trim(output_dir)//'/err_dtau_dlnA',status='unknown')
            do  i =  i_fstart,i_fend
            write(1,'(3E15.5)') wvec(i)/TWOPI,err_dtau(i),err_dlnA(i)
            !print *,'err_dtau(i)=',err_dtau(i)  
            enddo
            close(1)
        endif

    endif  ! if use_error_MT

    !     ------------------------------------------------------------------
    !     End error calculation loop
    !     ------------------------------------------------------------------

end subroutine mt_measure
! ------------------------------------------------------------------------------------
subroutine mt_adj(syn,npts,deltat,nlen,df,i_fstart,i_fend,dtau_w,dlnA_w,w_taper,wp_taper,wq_taper,&
        err_dt_cc,err_dlnA_cc, &
        err_dtau_mt,err_dlnA_mt, &
        compute_adjoint, &
        fp,fq, misfit_p, misfit_q)
    use constants
    implicit none

    integer, intent(in) :: npts,nlen
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: syn
    real(kind=CUSTOM_REAL), intent(in) ::  deltat,df,err_dt_cc,err_dlnA_cc
    integer, intent(in) :: i_fstart,i_fend
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dtau_w,dlnA_w,err_dtau_mt,err_dlnA_mt
    logical, intent(in) :: compute_adjoint
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: fp, fq 
    real(kind=CUSTOM_REAL), intent(out) :: misfit_p, misfit_q

    ! frequency-domain taper
    integer :: nfrange, nflen 
    real(kind=CUSTOM_REAL):: taper_percentage=1.0
    character(len=4) :: taper_type="cosp"
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: tas
    real(kind=CUSTOM_REAL), dimension(NPT) :: w_taper,wp_taper,wq_taper
    real(kind=CUSTOM_REAL) :: ffac, dtau_wtr, dlnA_wtr, err_t, err_A

    !! fft -- double size
    ! FFT parameters 
    real(kind=SIZE_DOUBLE) :: ampmax,wtr_use
    integer :: i,ictaper

    !! fft to evaluate adj
    ! tapered data in time domain
    real(kind=CUSTOM_REAL), dimension(NPT) :: syn_dtw_h, syn_vtw_h
    ! fft and ifft (forward and backward)
    complex (SIZE_DOUBLE), dimension(:,:),allocatable :: syn_dtw_ho_all,syn_vtw_ho_all
    complex (SIZE_DOUBLE), dimension(NPT) :: p_bot_mtm, q_bot_mtm, &
        pwc_adj,qwc_adj
    real(kind=SIZE_DOUBLE), dimension(NPT) ::  dtau_pj_t,dlnA_qj_t
    real(kind=CUSTOM_REAL), dimension(NPT) :: fp_adj, fq_adj

    ! frequency-domain tapers
    ! THIS CHOICE WILL HAVE AN EFFECT ON THE ADJOINT SOURCES
    nfrange = i_fend -i_fstart 
    w_taper(:) = 0.
    nflen=nfrange+1
    allocate(tas(nflen))
    call window_taper(nflen,taper_percentage,taper_type,tas)
    w_taper(i_fstart:i_fend)=tas(1:nflen)
    deallocate(tas)

    ! compute normalization factor for w_taper
    ! note: 2 is needed for the integration from -inf to inf
    ffac = 2.0 * df * sum(w_taper(i_fstart:i_fend) )   
    if (DISPLAY_DETAILS) then
        print* 
        print*, ' normalize frequency-domain taper' 
        print*, ' df = ', df 
        print *, 'Taper normalization factor, ffac = ', ffac
    endif
    ! no error estimate
    ! only adds normalization factor
    wp_taper = w_taper / ffac
    wq_taper = w_taper / ffac

    ! add error estimated 
    if (USE_ERROR_CC) then
        ! CC error estimate
        do i = i_fstart,i_fend
        wp_taper(i) = wp_taper(i) / (err_dt_cc ** 2)
        wq_taper(i) = wq_taper(i) / (err_dlnA_cc ** 2)
        !print *,'err_dt_cc=',err_dt_cc
        enddo 
    endif
    if (USE_ERROR_MT) then
        ! MT jack-knife error estimate
        dtau_wtr = WTR * sum(abs(dtau_w(i_fstart:i_fend)))/ nfrange
        dlnA_wtr = WTR * sum(abs(dlnA_w(i_fstart:i_fend)))/ nfrange
        print*
        print*,'to add mt error :'
        print*,'water level for err_dtau is ',dtau_wtr
        print*,'water level for err_dlnA is ',dlnA_wtr

        do i = i_fstart,i_fend
        err_t = err_dtau_mt(i)
        err_A = err_dlnA_mt(i)
        if (err_t < dtau_wtr)  err_t = err_t + dtau_wtr
        if (err_A < dlnA_wtr)  err_A = err_A + dlnA_wtr
        print *,' err_t=', err_t
        wp_taper(i) = wp_taper(i) / (err_t ** 2)
        wq_taper(i) = wq_taper(i) / (err_A ** 2)
        enddo
    endif

    if( DISPLAY_DETAILS) then
        open(1,file=trim(output_dir)//'/frequency_taper',status='unknown')
        do  i =  i_fstart,i_fend
        write(1,'(I5,3e15.5)') i,w_taper(i),wp_taper(i),wq_taper(i)
        enddo
        close(1)
    endif

    !! misfit
    !! add 2.0 to consider negative and positive freq in [-inf inf] intergration 
    !misfit_p=0.5*2.0*sum(dtau_w(i_fstart:i_fend)**2*wp_taper(i_fstart:i_fend)*df)
    !misfit_q=0.5*2.0*sum(dlnA_w(i_fstart:i_fend)**2*wq_taper(i_fstart:i_fend)*df)
    !print *,'misfit_p=',misfit_p
    !misfit_p=2.0*sum(dtau_w(i_fstart:i_fend)**2*wp_taper(i_fstart:i_fend)*df)
    !misfit_q=2.0*sum(dlnA_w(i_fstart:i_fend)**2*wq_taper(i_fstart:i_fend)*df)


    if(compute_adjoint) then
        ! allocate MT variables
        allocate(syn_dtw_ho_all(NPT,ntaper))
        allocate(syn_vtw_ho_all(NPT,ntaper))

        p_bot_mtm = 0.
        q_bot_mtm = 0.

        do ictaper = 1,ntaper

        ! tapered synthetic displacement
        syn_dtw_h(1:nlen) = syn(1:nlen) * real(tapers(1:nlen,ictaper),CUSTOM_REAL)

        ! compute velocity of tapered syn
        call compute_vel(syn_dtw_h,npts,deltat,nlen,syn_vtw_h)

        ! single-tapered complex synthetic displacement and velocity
        syn_dtw_ho_all(:,ictaper) = 0.
        syn_vtw_ho_all(:,ictaper) = 0.
        syn_dtw_ho_all(1:nlen,ictaper) = dcmplx(syn_dtw_h(1:nlen),0.)
        syn_vtw_ho_all(1:nlen,ictaper) = dcmplx(syn_vtw_h(1:nlen),0.)

        ! apply FFT get complex spectra
        call fft(LNPT,syn_dtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,syn_vtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))

        p_bot_mtm(:) = p_bot_mtm(:) + syn_vtw_ho_all(:,ictaper) &
            * conjg(syn_vtw_ho_all(:,ictaper))
        q_bot_mtm(:) = q_bot_mtm(:) + syn_dtw_ho_all(:,ictaper) &
            * conjg(syn_dtw_ho_all(:,ictaper))

        enddo ! ictaper

        if( DISPLAY_DETAILS) then
            open(2,file=trim(output_dir)//'/adj_bot_pq',status='unknown')
            do  i = i_fstart,i_fend 
            write(2,*) i,abs(p_bot_mtm(i)), abs(q_bot_mtm(i))
            enddo
            close(2)
        endif

        ampmax = maxval(abs(p_bot_mtm))
        !    wtr_use = ampmax * wtr_mtm**2
        wtr_use = ampmax * 0.01

        ! compute p_j, q_j, P_j, Q_j and adjoint source fp, fq
        fp(1:npts) = 0.
        fq(1:npts) = 0.
        do ictaper = 1,ntaper
        fp_adj=0.0
        fq_adj=0.0
        ! compute p_j(w) and q_j(w)
        pwc_adj(:) = cmplx(0.,0.)
        qwc_adj(:) = cmplx(0.,0.)

        do i = i_fstart,i_fend
        ! if(abs(p_bot_mtm(i)) > abs(wtr_use)) pwc_adj(i) = &
        !        syn_vtw_ho_all(i,ictaper) / p_bot_mtm(i)
        ! if(abs(p_bot_mtm(i)) > abs(wtr_use)) pwc_adj(i) = &
        !        syn_vtw_ho_all(i,ictaper) / (p_bot_mtm(i) + wtr_use)
        pwc_adj(i) =  syn_vtw_ho_all(i,ictaper) / p_bot_mtm(i)
        qwc_adj(i) = -syn_dtw_ho_all(i,ictaper) / q_bot_mtm(i)
        enddo

        ! compute P_j(w) and Q_j(w)
        ! NOTE: the MT measurement is incorporated here
        !             also note that wp_taper and wq_taper can contain
        !             uncertainty estimations
        ! adds misfit measurement dtau, dlnA
        pwc_adj(i_fstart: i_fend) = pwc_adj(i_fstart: i_fend) &
            * dcmplx(dtau_w(i_fstart: i_fend),0.) &
            * dcmplx(wp_taper(i_fstart: i_fend),0.)
        qwc_adj(i_fstart: i_fend) = qwc_adj(i_fstart: i_fend) &
            * dcmplx(dlnA_w(i_fstart: i_fend),0.) &
            * dcmplx(wq_taper(i_fstart: i_fend),0.)

        ! IFFT into the time domain
        call fftinv(LNPT,pwc_adj,dble(REVERSE_FFT),dble(deltat),dtau_pj_t)
        call fftinv(LNPT,qwc_adj,dble(REVERSE_FFT),dble(deltat),dlnA_qj_t)

        ! create adjoint source
        fp_adj(1:nlen)=real(tapers(1:nlen,ictaper) * dtau_pj_t(1:nlen),CUSTOM_REAL)
        fq_adj(1:nlen)=real(tapers(1:nlen,ictaper) * dlnA_qj_t(1:nlen),CUSTOM_REAL)
        ! applies taper to time signal
        fp(1:nlen) = fp(1:nlen) + fp_adj(1:nlen)
        fq(1:nlen) = fq(1:nlen) + fq_adj(1:nlen)

        enddo  ! ictaper

        deallocate(syn_dtw_ho_all)
        deallocate(syn_vtw_ho_all)
    endif ! compute_adjoint

end subroutine mt_adj
! ------------------------------------------------------------------------------------
subroutine mt_adj_DD(s1,s2,npts,deltat,nlen,df,i_fstart,i_fend,ddtau_w,ddlnA_w,&
        err_dt_cc_obs,err_dt_cc_syn,err_dlnA_cc_obs,err_dlnA_cc_syn, &
        err_dtau_mt_obs,err_dtau_mt_syn,err_dlnA_mt_obs,err_dlnA_mt_syn, &
        compute_adjoint,fp1,fp2,fq1,fq2,misfit_p,misfit_q)
    use constants
    implicit none

    integer, intent(in) :: npts,nlen
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: s1,s2
    real(kind=CUSTOM_REAL), intent(in) :: deltat,df
    real(kind=CUSTOM_REAL), intent(in) :: err_dt_cc_obs,err_dt_cc_syn,err_dlnA_cc_obs,err_dlnA_cc_syn
    integer, intent(in) :: i_fstart,i_fend
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: ddtau_w,ddlnA_w
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: err_dtau_mt_obs,err_dtau_mt_syn,err_dlnA_mt_obs,err_dlnA_mt_syn
    logical, intent(in) :: compute_adjoint
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: fp1,fp2,fq1,fq2
    real(kind=CUSTOM_REAL), intent(out) :: misfit_p, misfit_q

    ! frequency-domain taper
    integer :: nfrange, nflen
    real(kind=CUSTOM_REAL):: taper_percentage=1.0
    character(len=4) :: taper_type="cosp"
    real(kind=CUSTOM_REAL), dimension(:),allocatable :: tas
    real(kind=CUSTOM_REAL), dimension(NPT) :: w_taper, wp_taper, wq_taper
    real(kind=CUSTOM_REAL) :: ffac, ddtau_wtr, ddlnA_wtr, err_t, err_A

    integer :: i,ictaper

    !! fft to evaluate adj
    ! tapered data in time domain
    real(kind=CUSTOM_REAL), dimension(NPT) :: s1_dtw_h, s1_vtw_h
    real(kind=CUSTOM_REAL), dimension(NPT) :: s2_dtw_h, s2_vtw_h
    ! fft and ifft (forward and backward)
    complex (SIZE_DOUBLE), dimension(:,:),allocatable :: s1_dtw_ho_all,s1_vtw_ho_all
    complex (SIZE_DOUBLE), dimension(:,:),allocatable :: s2_dtw_ho_all,s2_vtw_ho_all
    complex (SIZE_DOUBLE), dimension(NPT) :: Mtr1_mtm,Mtr2_mtm,Mtr3_mtm,Mtr4_mtm,Mtr5_mtm
    complex (SIZE_DOUBLE), dimension(NPT) :: p1_adj,pw1_adj,p2_adj,pw2_adj 
    complex (SIZE_DOUBLE), dimension(NPT) :: q1_adj,qw1_adj,q2_adj,qw2_adj
    complex (SIZE_DOUBLE), dimension(NPT) :: fp1_adj,fp2_adj,fq1_adj,fq2_adj
    real(kind=SIZE_DOUBLE), dimension(NPT) :: fp1_adj_t,fp2_adj_t,fq1_adj_t,fq2_adj_t

    ! frequency-domain tapers
    ! THIS CHOICE WILL HAVE AN EFFECT ON THE ADJOINT SOURCES
    nfrange = i_fend -i_fstart
    w_taper(:) = 0.
    nflen=nfrange+1
    allocate(tas(nflen))
    call window_taper(nflen,taper_percentage,taper_type,tas)
    w_taper(i_fstart:i_fend)=tas(1:nflen)
    deallocate(tas)

    ! compute normalization factor for w_taper
    ! note: 2 is needed for the integration from -inf to inf
    ffac = 2.0 * df * sum(w_taper(i_fstart:i_fend) )
    if (DISPLAY_DETAILS) then
        print*
        print*, ' normalize frequency-domain taper'
        print*, ' df = ', df
        print *, 'Taper normalization factor, ffac = ', ffac
    endif
    ! no error estimate
    ! only adds normalization factor
    wp_taper = w_taper / ffac
    wq_taper = w_taper / ffac

    ! add error estimated 
    if (USE_ERROR_CC) then
        !          ! CC error estimate
        do i = i_fstart,i_fend
        wp_taper(i) = wp_taper(i) / (err_dt_cc_obs * err_dt_cc_syn)
        wq_taper(i) = wq_taper(i) / (err_dlnA_cc_obs * err_dlnA_cc_syn)
        enddo
    endif
    if (USE_ERROR_MT) then
        ! MT jack-knife error estimate
        ddtau_wtr = (WTR * sum(abs(ddtau_w(i_fstart:i_fend)))/ nfrange)**2
        ddlnA_wtr = (WTR * sum(abs(ddlnA_w(i_fstart:i_fend)))/ nfrange)**2
        print*
        print*,'to add mt error :'
        print*,'water level for err_dtau is ',ddtau_wtr
        print*,'water level for err_dlnA is ',ddlnA_wtr

        do i = i_fstart,i_fend
        err_t = err_dtau_mt_obs(i) * err_dtau_mt_syn(i)
        err_A = err_dlnA_mt_obs(i) * err_dlnA_mt_syn(i)
        if (err_t < ddtau_wtr)  err_t = err_t + ddtau_wtr
        if (err_A < ddlnA_wtr)  err_A = err_A + ddlnA_wtr
        wp_taper(i) = wp_taper(i) / err_t 
        wq_taper(i) = wq_taper(i) / err_A 
        enddo
    endif

    if( DISPLAY_DETAILS) then
        open(1,file=trim(output_dir)//'/frequency_taper',status='unknown')
        do  i =  i_fstart,i_fend
        write(1,'(I5,3e15.5)') i,w_taper(i),wp_taper(i),wq_taper(i)
        enddo
        close(1)
    endif
    !! misfit
    !! add 2.0 to consider negative and positive freq in [-inf inf] intergration
    misfit_p=0.5*2.0*sum(ddtau_w(i_fstart:i_fend)**2*wp_taper(i_fstart:i_fend)*df)
    misfit_q=0.5*2.0*sum(ddlnA_w(i_fstart:i_fend)**2*wq_taper(i_fstart:i_fend)*df)

    if(compute_adjoint) then 
        ! allocate MT variables
        allocate(s1_dtw_ho_all(NPT,ntaper))
        allocate(s1_vtw_ho_all(NPT,ntaper))
        allocate(s2_dtw_ho_all(NPT,ntaper))
        allocate(s2_vtw_ho_all(NPT,ntaper))

        !! constant terms for adj 
        Mtr1_mtm = 0.
        Mtr2_mtm = 0.
        Mtr3_mtm = 0.
        Mtr4_mtm = 0.
        Mtr5_mtm = 0.

        do ictaper = 1,ntaper

        ! tapered synthetic displacement
        s1_dtw_h(1:nlen) = s1(1:nlen) * real(tapers(1:nlen,ictaper),CUSTOM_REAL)
        s2_dtw_h(1:nlen) = s2(1:nlen) * real(tapers(1:nlen,ictaper),CUSTOM_REAL)

        ! compute velocity of tapered syn
        call compute_vel(s1_dtw_h,npts,deltat,nlen,s1_vtw_h)
        call compute_vel(s2_dtw_h,npts,deltat,nlen,s2_vtw_h)


        ! single-tapered complex synthetic displacement and velocity
        s1_dtw_ho_all(:,ictaper) = 0.
        s1_vtw_ho_all(:,ictaper) = 0.
        s2_dtw_ho_all(:,ictaper) = 0.
        s2_vtw_ho_all(:,ictaper) = 0.
        s1_dtw_ho_all(1:nlen,ictaper) = dcmplx(s1_dtw_h(1:nlen),0.)
        s1_vtw_ho_all(1:nlen,ictaper) = dcmplx(s1_vtw_h(1:nlen),0.)
        s2_dtw_ho_all(1:nlen,ictaper) = dcmplx(s2_dtw_h(1:nlen),0.)
        s2_vtw_ho_all(1:nlen,ictaper) = dcmplx(s2_vtw_h(1:nlen),0.)

        ! apply FFT get complex spectra
        call fft(LNPT,s1_dtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,s1_vtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,s2_dtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))
        call fft(LNPT,s2_vtw_ho_all(:,ictaper),dble(FORWARD_FFT),dble(deltat))

        Mtr1_mtm(:) = Mtr1_mtm(:) + s1_vtw_ho_all(:,ictaper) &
            * conjg(s2_vtw_ho_all(:,ictaper))
        Mtr2_mtm(:) = Mtr2_mtm(:) + s2_vtw_ho_all(:,ictaper) &
            * conjg(s1_vtw_ho_all(:,ictaper))

        Mtr3_mtm(:) = Mtr3_mtm(:) + s1_dtw_ho_all(:,ictaper) &
            * conjg(s2_dtw_ho_all(:,ictaper))
        Mtr4_mtm(:) = Mtr4_mtm(:) + s2_dtw_ho_all(:,ictaper) &
            * conjg(s1_dtw_ho_all(:,ictaper))
        Mtr5_mtm(:) = Mtr5_mtm(:) + s2_dtw_ho_all(:,ictaper) &
            * conjg(s2_dtw_ho_all(:,ictaper))
        enddo ! ictaper


        ! compute p_j, q_j, P_j, Q_j and adjoint source fp, fq
        fp1(1:npts) = 0.
        fq1(1:npts) = 0.
        fp2(1:npts) = 0.
        fq2(1:npts) = 0.

        do ictaper = 1,ntaper

        ! compute p_j(w) and q_j(w)
        p1_adj(:) = cmplx(0.,0.)
        pw1_adj(:) = cmplx(0.,0.)
        p2_adj(:) = cmplx(0.,0.)
        pw2_adj(:) = cmplx(0.,0.)
        q1_adj(:) = cmplx(0.,0.)
        qw1_adj(:) = cmplx(0.,0.)
        q2_adj(:) = cmplx(0.,0.)
        qw2_adj(:) = cmplx(0.,0.)

        fp1_adj(:) = cmplx(0.,0.)
        fp2_adj(:) = cmplx(0.,0.)
        fq1_adj(:) = cmplx(0.,0.)
        fq2_adj(:) = cmplx(0.,0.)

        do i = i_fstart,i_fend
        !p1_adj(i)  =  -0.5* conjg(s2_vtw_ho_all(i,ictaper)) / Mtr1_mtm(i)
        pw1_adj(i) = - 0.5 * s2_vtw_ho_all(i,ictaper) / Mtr2_mtm(i)

        !p2_adj(i)  =  0.5 * conjg(s1_vtw_ho_all(i,ictaper)) / Mtr2_mtm(i)
        pw2_adj(i) =  0.5 * s1_vtw_ho_all(i,ictaper) / Mtr1_mtm(i)

        q1_adj(i)  =  0.5 * conjg(s2_dtw_ho_all(i,ictaper)) / Mtr3_mtm(i)
        !   qw1_adj(i) =  0.5 * s2_dtw_ho_all(i,ictaper) / Mtr4_mtm(i)
        ! 
        q2_adj(i)  =  0.5 * conjg(s1_dtw_ho_all(i,ictaper)) / Mtr4_mtm(i) &
            - conjg(s2_dtw_ho_all(i,ictaper)) / Mtr5_mtm(i)
        !   qw2_adj(i) =  0.5 * s1_dtw_ho_all(i,ictaper) / Mtr3_mtm(i) &
        !                 - s2_dtw_ho_all(i,ictaper) / Mtr5_mtm(i)
        enddo

        ! compute P_j(w) and Q_j(w)
        ! NOTE: the MT measurement is incorporated here
        !             also note that wp_taper and wq_taper can contain
        !             uncertainty estimations
        ! adds misfit measurement dtau, dlnA
        fp1_adj(i_fstart: i_fend) &
            ! = (conjg(p1_adj(i_fstart: i_fend)) +
        =  2.0*(pw1_adj(i_fstart:i_fend)) &
            * dcmplx(ddtau_w(i_fstart: i_fend),0.) &
            * dcmplx(wp_taper(i_fstart: i_fend),0.)
        fp2_adj(i_fstart: i_fend) &
            ! = (conjg(p2_adj(i_fstart: i_fend)) +
        = 2.0*(pw2_adj(i_fstart: i_fend)) &
            * dcmplx(ddtau_w(i_fstart: i_fend),0.) &
            * dcmplx(wp_taper(i_fstart: i_fend),0.)
        fq1_adj(i_fstart: i_fend) &
            = (q1_adj(i_fstart: i_fend) + conjg(q1_adj(i_fstart: i_fend)))&
            * dcmplx(ddlnA_w(i_fstart: i_fend),0.) &
            * dcmplx(wq_taper(i_fstart: i_fend),0.)
        fq2_adj(i_fstart: i_fend) &
            = (q2_adj(i_fstart: i_fend) + conjg(q2_adj(i_fstart: i_fend)))&
            * dcmplx(ddlnA_w(i_fstart: i_fend),0.) &
            * dcmplx(wq_taper(i_fstart: i_fend),0.)


        ! IFFT into the time domain
        call fftinv(LNPT,fp1_adj,dble(REVERSE_FFT),dble(deltat),fp1_adj_t)
        call fftinv(LNPT,fp2_adj,dble(REVERSE_FFT),dble(deltat),fp2_adj_t)
        call fftinv(LNPT,fq1_adj,dble(REVERSE_FFT),dble(deltat),fq1_adj_t)
        call fftinv(LNPT,fq2_adj,dble(REVERSE_FFT),dble(deltat),fq2_adj_t)

        ! create adjoint source
        ! applies taper to time signal
        fp1(1:nlen) = fp1(1:nlen) + real(tapers(1:nlen,ictaper) * &
            fp1_adj_t(1:nlen),CUSTOM_REAL)
        fp2(1:nlen) = fp2(1:nlen) + real(tapers(1:nlen,ictaper) * &
            fp2_adj_t(1:nlen),CUSTOM_REAL)
        fq1(1:nlen) = fq1(1:nlen) + real(tapers(1:nlen,ictaper) * &
            fq1_adj_t(1:nlen),CUSTOM_REAL)
        fq2(1:nlen) = fq2(1:nlen) + real(tapers(1:nlen,ictaper) * &
            fq2_adj_t(1:nlen),CUSTOM_REAL)
        enddo  ! ictaper
        deallocate(s1_dtw_ho_all)
        deallocate(s1_vtw_ho_all)
        deallocate(s2_dtw_ho_all)
        deallocate(s2_vtw_ho_all)
    endif ! compute_adjoint 

end subroutine mt_adj_DD

!==============================================================================
!-----------------------------------------------------------------------
subroutine CC_similarity(d1,d2,npts,deltat,&
        tstart1,tend1,tstart2,tend2,&
        taper_percentage,taper_type,&
        cc_max)
    !! measure the similarity of two waveforms based on cross-correlatiobs

    use constants
    implicit none

    ! inputs & outputs 
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: d1,d2
    integer, intent(in) :: npts
    real(kind=CUSTOM_REAL), intent(in) :: deltat
    real(kind=CUSTOM_REAL), intent(in) :: tstart1,tend1,tstart2,tend2
    real(kind=CUSTOM_REAL), intent(in) :: taper_percentage
    character(len=4), intent(in) :: taper_type
    real(kind=CUSTOM_REAL), intent(out) :: cc_max

    ! window
    integer :: nlen1,nlen2,nlen
    integer :: i_tstart1,i_tend1,i_tstart2,i_tend2
    real(kind=CUSTOM_REAL), dimension(:), allocatable :: tas1, tas2
    real(kind=CUSTOM_REAL), dimension(npts) :: d1_tw,d2_tw
    ! cc 
    integer :: ishift
    real(kind=CUSTOM_REAL) :: dlnA

    !! initialization
    d1_tw(1:npts)=0.0
    d2_tw(1:npts)=0.0
    cc_max=0.0

    !! window
    nlen1=floor((tend1-tstart1)/deltat)+1
    if(nlen1<1 .or. nlen1>npts) then 
        print*,'check nlen1 ',nlen1
        stop
    endif
    i_tstart1=max(floor(tstart1 / deltat), 1)
    i_tend1 = min(i_tstart1+nlen1-1, npts)
    nlen1=i_tend1-i_tstart1+1
    allocate(tas1(nlen1))
    call window_taper(nlen1,taper_percentage,taper_type,tas1)
    d1_tw(1:nlen1)=d1(i_tstart1:i_tend1)
    d1_tw(1:nlen1)=tas1(1:nlen1)*d1_tw(1:nlen1)

    nlen2=floor((tend2-tstart2)/deltat)+1
    if(nlen2<1 .or. nlen2>npts) then
        print*,'check nlen2 ',nlen2
        stop
    endif
    i_tstart2=max(floor(tstart2 / deltat), 1)
    i_tend2 = min(i_tstart2+nlen2-1, npts)
    nlen2=i_tend2-i_tstart2+1
    allocate(tas2(nlen2))
    call window_taper(nlen2,taper_percentage,taper_type,tas2)
    d2_tw(1:nlen2)=d2(i_tstart2:i_tend2)
    d2_tw(1:nlen2)=tas2(1:nlen2)*d2_tw(1:nlen2)

    nlen = max(nlen1,nlen2)

    !! cc
    call xcorr_calc(d1_tw,d2_tw,npts,1,nlen,ishift,dlnA,cc_max) !T(d1-d2)
    deallocate(tas1,tas2)
end subroutine cc_similarity
! ---------------------------------------------------------------------------
subroutine frequency_limit (dat,nlen,deltat,min_freq,max_freq,i_fstart,i_fend)
    use constants
    implicit none

    integer, intent(in) :: nlen
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dat
    real(kind=CUSTOM_REAL), intent(in) ::  deltat
    real(kind=CUSTOM_REAL), intent(in) :: min_freq,max_freq
    integer, intent(out) :: i_fstart,i_fend

    integer :: i,fnum 
    real(kind=CUSTOM_REAL) :: df,ampmax, wtr_use
    integer :: i_ampmax, i_fstart_stop, i_fend_stop
    complex (SIZE_DOUBLE), dimension(NPT) :: dat_dtwo     

    !! find reliable frequency limits 
    ! fft of untapered inputs
 
    df = 1./(NPT*deltat)
    dat_dtwo = cmplx(0.,0.)
    dat_dtwo(1:nlen) = cmplx(real(dat(1:nlen),SIZE_DOUBLE),0.0_SIZE_DOUBLE)
    call fft(LNPT,dat_dtwo,dble(FORWARD_FFT),dble(deltat))

    fnum = NPT/2+1
    ! water level based untapered dat1
    ampmax = 0.
    i_ampmax = 1
    do i = 1, fnum   ! loop over frequencies
    if( abs(dat_dtwo(i)) > ampmax) then
        ampmax =  abs(dat_dtwo(i))
        i_ampmax = i
    endif
    enddo
    wtr_use = ampmax * WTR

    ! i_fend 
    i_fend = fnum
    i_fend_stop = 0
    do i = 1,fnum
    if( abs(dat_dtwo(i)) <= abs(wtr_use) .and. i_fend_stop==0 .and. i > i_ampmax ) then
        i_fend_stop = 1
        i_fend = i
    endif
    if( abs(dat_dtwo(i)) >= 10.*abs(wtr_use) .and. i_fend_stop==1 .and. i > i_ampmax) then
        i_fend_stop = 0
        i_fend = i
    endif
    enddo
    !Don't go beyond the Nyquist frequency
    !df = 1./(NPT*deltat)

    if( DISPLAY_DETAILS) then
    print *,'df =1./(NPT*deltat)',  'NPT=',NPT,'max_freq=',max_freq,'min_freq=',min_freq   
    print *,'i_fend=',i_fend,'floor(1.0/(2*deltat)/df)+1=',floor(1.0/(2*deltat)/df)+1,'floor(max_freq/df))',floor(max_freq/df)
    endif

    i_fend = min(i_fend,&
        floor(1.0/(2*deltat)/df)+1,&
        floor(max_freq/df))

    ! i_fstart 
    i_fstart = 1
    i_fstart_stop = 0
    do i = fnum,1,-1
    if( abs(dat_dtwo(i)) <= abs(wtr_use) .and. i_fstart_stop==0 .and. i < i_ampmax ) then
        i_fstart_stop = 1
        i_fstart = i
    endif
    if( abs(dat_dtwo(i)) >= 10.*abs(wtr_use) .and. i_fstart_stop==1 .and. i < i_ampmax) then
        i_fstart_stop = 0
        i_fstart = i
    endif
    enddo
    !assume there are at least N cycles within the window
    if( DISPLAY_DETAILS) then
    print *,'i_fstart=',i_fstart
    print *,'ceiling(ncycle_in_window/(nlen*deltat)/df)=',ceiling(ncycle_in_window/(nlen*deltat)/df),'ncycle_in_window=',ncycle_in_window
    print *,'ceiling(min_freq/df)=',ceiling(min_freq/df),' min_freq=',min_freq,'df=',df
    endif

    i_fstart = max(i_fstart,&
        ceiling(ncycle_in_window/(nlen*deltat)/df),&
        ceiling(min_freq/df))

    if( DISPLAY_DETAILS) then
        open(1,file=trim(output_dir)//'/spectrum',status='unknown')
        do  i =  1,2*i_fend
        write(1,'(f15.5,2e15.5)') (i-1)*df,abs(dat_dtwo(i)),wtr_use
        enddo
        close(1)
    endif

end subroutine frequency_limit
! ---------------------------------------------------------------------------
subroutine cc_error(data_dtw,syn_dtw,npts,deltat,nlen,ishift,dlnA,sigma_dt,sigma_dlnA)
    ! CHT: Estimate the uncertainty in the CC measurement
    !      based on the integrated waveform difference between the data
    !      and the reconstructed synthetics.
    ! NOTE: We implement the exact equations that are in the Latex notes.

    use constants
    implicit none
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: data_dtw, syn_dtw
    integer, intent(in) :: npts,nlen, ishift
    real(kind=CUSTOM_REAL), intent(in) :: deltat, dlnA
    real(kind=CUSTOM_REAL), intent(inout) :: sigma_dt, sigma_dlnA

    real(kind=CUSTOM_REAL), dimension(npts) :: syn_dtw_cc, syn_vtw_cc
    real(kind=CUSTOM_REAL) :: sigma_dt_top, sigma_dlnA_top, sigma_dt_bot,sigma_dlnA_bot
    integer :: i,j

    ! fixed window for d, correct the window for s
    call cc_window(syn_dtw,npts,1,nlen,ishift,dlnA,syn_dtw_cc)

    ! compute cc-corrected synthetic velocity
    call compute_vel(syn_dtw_cc,npts,deltat,nlen,syn_vtw_cc)

    ! estimated uncertainty in cross-correlation travltime and amplitude
    sigma_dt_top   = sum( (data_dtw(1:nlen) - syn_dtw_cc(1:nlen) )**2 )
    sigma_dt_bot   = sum( syn_vtw_cc(1:nlen)**2 )
    sigma_dlnA_top = sigma_dt_top
    sigma_dlnA_bot = sum( (syn_dtw_cc(1:nlen))**2 )/(dlnA * dlnA)
    sigma_dt       = sqrt( sigma_dt_top / sigma_dt_bot )
    sigma_dlnA     = sqrt( sigma_dlnA_top / sigma_dlnA_bot )

    ! make sure that the uncertainty estimates are not below the water level;
    ! otherwise, the adjoint sources will blow up unreasonably
    if( sigma_dt < DT_SIGMA_MIN) sigma_dt = DT_SIGMA_MIN
    if( sigma_dlnA < DLNA_SIGMA_MIN) sigma_dlnA = DLNA_SIGMA_MIN
    if (isnan(sigma_dt)) sigma_dt=1.0


    if(DISPLAY_DETAILS) then
        print*
        print*,'error estimation based on cc'
        print*,'sigma_dt top = ',sigma_dt_top, 'bot =', sigma_dt_bot, &
            ' ratio =', sqrt( sigma_dt_top / sigma_dt_bot )
        print*,'sigma_dlnA top = ',sigma_dlnA_top, 'bot =', sigma_dlnA_bot,&
            ' ratio =', sqrt( sigma_dlnA_top / sigma_dlnA_bot )
        print *, 'estimate sigma_dt   : ', sigma_dt
        print *, 'estimate sigma_dlnA : ', sigma_dlnA
        open(1,file=trim(output_dir)//'/cc_error.dat',status='unknown')
        do i = 1,nlen
        write(1,'(I5,4e15.5)') i, data_dtw(i), syn_dtw(i), syn_dtw_cc(i),syn_vtw_cc(i)
        enddo
        close(1)
    endif
end subroutine cc_error

! ---------------------------------------------------------------------------
subroutine write_phase(phase_func, wvec, i_right, tshift, phi_wt, dtau_wt)
    use constants
    implicit none

    ! The transfer function maps the data2 to the CC-corrected data1;

    complex (CUSTOM_REAL), dimension(*), intent(in) :: phase_func
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: wvec
    real(kind=CUSTOM_REAL), intent(in) ::  tshift
    integer, intent(in) ::  i_right
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: phi_wt, dtau_wt

    integer :: i, j
    real(kind=CUSTOM_REAL) :: smth, smth1, smth2 ! f0

    phi_wt(1:NPT) = 0.

    ! loop to calculate phase and amplitude
    do i = 1, i_right
    phi_wt(i) = atan2( aimag(phase_func(i)) , real(phase_func(i)) )
    enddo

    ! NOTE: the CC measurements dT (tshift) and dlnA are BOTH included
    dtau_wt(1) = tshift
    do i = 1, i_right

    if (i > 1 .and. i < i_right) then
        ! check the smoothness (2nd-order derivative) by 2*pi changes
        smth  =  phi_wt(i+1) + phi_wt(i-1) - 2.0 * phi_wt(i)
        smth1 = (phi_wt(i+1) + TWOPI) + phi_wt(i-1) - 2.0 * phi_wt(i)
        smth2 = (phi_wt(i+1) - TWOPI) + phi_wt(i-1) - 2.0 * phi_wt(i)
        if(abs(smth1).lt.abs(smth).and.abs(smth1).lt.abs(smth2).and. abs(phi_wt(i) - phi_wt(i+1)) > PHASE_STEP)then
            if (DISPLAY_DETAILS) print *, 'phase correction : 2 pi', i, phi_wt(i) - phi_wt(i+1)
            do j = i+1, i_right
            phi_wt(j) = phi_wt(j) + TWOPI
            enddo
        endif
        if(abs(smth2).lt.abs(smth).and.abs(smth2).lt.abs(smth1).and. abs(phi_wt(i) - phi_wt(i+1)) > PHASE_STEP)then
            if (DISPLAY_DETAILS) print *, 'phase correction : - 2 pi', i, phi_wt(i) - phi_wt(i+1)
            do j = i+1, i_right
            phi_wt(j) = phi_wt(j) - TWOPI
            enddo
        endif
    endif

    ! add the CC measurements to the transfer function
    if (i > 1) dtau_wt(i) = (0.5/wvec(i)) * phi_wt(i) + tshift

    enddo

end subroutine write_phase
! ---------------------------------------------------------------------------
subroutine write_trans(trans, wvec,i_left, i_right, tshift, dlnA,phi_wt, abs_wt, dtau_wt, dlnA_wt)
    ! The transfer function maps the synthetics to the CC-deconstructed data;
    ! the CC measurements then need to be applied to match the original data.

    use constants
    implicit none
    complex (CUSTOM_REAL), dimension(*),intent(in) :: trans
    real(kind=CUSTOM_REAL), dimension(*),intent(in) :: wvec
    real(kind=CUSTOM_REAL), intent(in) :: tshift, dlnA
    integer, intent(in) :: i_left, i_right
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: phi_wt, abs_wt,dtau_wt,dlnA_wt

    integer :: i

    ! initialization 
    phi_wt(1:NPT) = 0.d0 
    abs_wt(1:NPT) = 0.d0
    dtau_wt(1:NPT) = 0.d0
    dlnA_wt(1:NPT) = 0.d0 

    ! loop to calculate phase and amplitude
    do i = i_left, i_right
    phi_wt(i) = atan2( aimag(trans(i)) , real(trans(i)) )
    abs_wt(i) = abs(trans(i))
    enddo

    ! check the smoothenss of phi 
    call smooth_phase(phi_wt,i_left,i_right)

    ! NOTE: the CC measurements dT (tshift) and dlnA are BOTH included 
    ! print*,' add the CC measurements to the transfer function'
    if (i_left > 1) dtau_wt(i_left:i_right) = (-1./wvec(i_left:i_right)) * phi_wt(i_left:i_right) + tshift
    if (i_left == 1) dtau_wt(i_left+1:i_right) = (-1./wvec(i_left+1:i_right)) * phi_wt(i_left+1:i_right) + tshift
    dtau_wt(1) = tshift 
    dlnA_wt(i_left:i_right) = log(abs_wt(i_left:i_right)) + dlnA

end subroutine write_trans
! --------------------------------------------------------------------
subroutine smooth_phase(phi_wt,i_left,i_right)
    ! check the smoothness of phi
    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*),intent(out) :: phi_wt
    integer, intent(in) :: i_left, i_right

    integer :: i, j
    real(kind=CUSTOM_REAL) :: smth, smth1, smth2 ! f0

    do i = i_left, i_right
    if (i > i_left .and. i < i_right) then
        !  print*,' check the smoothness (2nd-order derivative) by 2*pi changes'
        smth  =  phi_wt(i+1) + phi_wt(i-1) - 2.0 * phi_wt(i)
        smth1 = (phi_wt(i+1) + TWOPI) + phi_wt(i-1) - 2.0 * phi_wt(i)
        smth2 = (phi_wt(i+1) - TWOPI) + phi_wt(i-1) - 2.0 * phi_wt(i)
        if(abs(smth1).lt.abs(smth).and.abs(smth1).lt.abs(smth2).and. abs(phi_wt(i) - phi_wt(i+1)) > PHASE_STEP)then
            if (DISPLAY_DETAILS) print *, 'phase correction : 2pi',i,phi_wt(i) - phi_wt(i+1)
            do j = i+1, i_right
            phi_wt(j) = phi_wt(j) + TWOPI
            enddo
        endif

        if(abs(smth2).lt.abs(smth).and.abs(smth2).lt.abs(smth1).and. abs(phi_wt(i) - phi_wt(i+1)) > PHASE_STEP)then
            if (DISPLAY_DETAILS) print *, 'phase correction : - 2 pi ', i, phi_wt(i) - phi_wt(i+1)
            do j = i+1, i_right
            phi_wt(j) = phi_wt(j) - TWOPI
            enddo
        endif

    endif 
    enddo

end subroutine smooth_phase
! --------------------------------------------------------------------
subroutine unwrap_phase(phi_wt,i_left,i_right)
    ! unwrap phi
    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*),intent(out) :: phi_wt
    integer, intent(in) :: i_left, i_right

    integer :: i
    real(kind=CUSTOM_REAL) :: dp, dps, dp_corr

    do i = i_left, i_right
    dp_corr = 0.0
    if (i > i_left .and. i <= i_right) then
        !% Incremental phase variations 
        dp = phi_wt(i)-phi_wt(i-1)
        !% Equivalent phase variations in [-pi,pi)
        dps = mod(dp+PI,TWOPI)-PI
        !% Preserve variation sign for pi vs. -pi
        if(dps==-PI .and. dp>0.0) dps=PI
        !% Incremental phase corrections
        dp_corr = dps-dp
        !% Ignore correction when incr. variation is < CUTOFF
        if(abs(dp)<CUTOFF) dp_corr=0.0
        !% Integrate corrections and add to P to produce smoothed phase values
        phi_wt(i:i_right) = phi_wt(i:i_right) + dp_corr
    endif
    enddo

end subroutine unwrap_phase
! ---------------------------------------------------------------------------
subroutine cc_window(dat,npts,istart,iend,ishift,dlnA,dat_win)
    ! delay by ishift and scale by exp(dlnA)
    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dat
    real(kind=CUSTOM_REAL), intent(in) :: dlnA
    integer, intent(in) :: npts, istart, iend, ishift
    integer :: nlen
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: dat_win

    integer ::  i, j

    nlen = iend - istart+1

    ! initialization
    dat_win(1:npts) = 0.d0

    do i = 1, nlen

    ! index 
    j = i + (istart-1) - ishift

    if(j>=1 .and. j <=npts) then
        dat_win(i) = dat(j) * exp(dlnA)  ! shift and scale
    endif

    enddo

end subroutine cc_window
! ---------------------------------------------------------------------------
subroutine cc_window_inverse(dat_win,npts,istart,iend,ishift,dlnA,dat)
    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dat_win
    real(kind=CUSTOM_REAL), intent(in) :: dlnA
    integer, intent(in) :: npts, istart,iend, ishift
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: dat

    integer ::  i, j, nlen

    nlen = iend - istart+1

    ! initialization
    dat(1:npts) = 0.d0

    do i = 1, nlen

    ! index
    j = i + (istart-1) - ishift

    ! window dat2 using exact window info 
    if(j>=1 .and. j <=npts) then
        dat(j) = dat_win(i) * exp(-dlnA) 
    endif
    enddo

end subroutine cc_window_inverse
!-----------------------------------------------------------------------------
subroutine compute_vel(syn,npts,deltat,nlen,syn_vel)
    use constants
    implicit none 
    real(kind=CUSTOM_REAL), dimension(*),intent(in) :: syn 
    real(kind=CUSTOM_REAL), intent(in) :: deltat 
    integer, intent(in) :: npts, nlen
    real(kind=CUSTOM_REAL), dimension(*),intent(out) :: syn_vel

    ! index
    integer :: itime 

    ! initialization 
    syn_vel(1:npts)=0.0

    do itime=2,nlen-1
    syn_vel(itime)=(syn(itime+1)-syn(itime-1))/(2*deltat)
    enddo
    ! boundaries
    syn_vel(1)=(syn(2)-syn(1))/deltat
    syn_vel(nlen)=(syn(nlen)-syn(nlen-1))/deltat

end subroutine compute_vel

!-----------------------------------------------------------------------------
subroutine compute_acc(syn,npts,deltat,nlen,syn_acc)
    use constants
    implicit none
    real(kind=CUSTOM_REAL), dimension(*),intent(in) :: syn
    real(kind=CUSTOM_REAL), intent(in) :: deltat
    integer, intent(in) :: npts, nlen
    real(kind=CUSTOM_REAL), dimension(*),intent(out) :: syn_acc
    real(kind=CUSTOM_REAL), dimension(npts) :: syn_vel

    ! velocity 
    call compute_vel(syn,npts,deltat,nlen,syn_vel)

    ! acceleration 
    call compute_vel(syn_vel,npts,deltat,nlen,syn_acc)

end subroutine compute_acc


subroutine window(npts,istart,iend,window_type,win)
    use constants
    implicit none

    integer, intent(in) :: npts, istart,iend, window_type
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: win

    integer ::  i, nlen
    real(kind=CUSTOM_REAL) :: sfac1
    real(kind=CUSTOM_REAL), dimension(npts) :: fac

    nlen = iend - istart+1

    ! some constants
    sfac1 = (2./real(nlen))**2   ! for Welch taper

    ! initialization
    win(1:npts) = 0.d0
    fac(1:npts) = 0.d0

    do i = 1, nlen
    if(window_type ==2) then
        fac(i) = 1 - sfac1*((i-1) - real(nlen)/2.)**2
    elseif(window_type ==3) then
        fac(i) = 1. - cos(PI*(i-1)/(nlen-1))**ipwr_t
    elseif(window_type ==4) then
        fac(i) = 0.5 - 0.5*cos(TWOPI*(i-1)/(nlen-1))
    else
        fac(i) = 1. ! boxcar window
    endif
    enddo

    !! return 
    win(istart:iend)=fac(1:nlen)

end subroutine window

subroutine window_mute(npts,istart,iend,taper_percentage,taper_type,win)
    use constants
    implicit none

    ! input parameters
    integer, intent(in) ::  npts,istart,iend    
    real, intent(in) :: taper_percentage
    character(len=4) :: taper_type
    real, dimension(*), intent(out) :: win
    integer :: taper_len
    integer :: i
  
    real(kind=CUSTOM_REAL), dimension(npts) :: fac

    ! initialization
    !npts = iend - istart+1
    win(1:npts)=0.0
    fac(1:npts)=1.0

    if (taper_percentage <= 0.0 .or. taper_percentage >= 1.0) then
        taper_len = int(npts*taper_percentage / 2.0)
    else
        taper_len = int(npts*taper_percentage / 2.0 + 0.5)
    endif


    !print *,'npst=',npts,'taper_percentage=',taper_percentage,'taper_len=',taper_len



!    do i=1, taper_len
!    if (trim(taper_type) == 'boxc') then
!        tas(i)=1.0
!    elseif (trim(taper_type) == 'hann') then
!        tas(i)=0.5 - 0.5 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
!    elseif (trim(taper_type) == 'hamm') then
!        tas(i)=0.54 - 0.46 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
!    elseif (trim(taper_type) == 'cose') then
!        tas(i)=cos(PI * (i-1) / (2 * taper_len - 1) - PI / 2.0) ** ipwr_t
!    elseif (trim(taper_type) == 'cosp') then
!        tas(i)=1.0 - cos(PI * (i-1) / (2 * taper_len - 1)) ** ipwr_t
!    else
!        print*,'taper_type must be among "boxc"/"hann"/"hamm"/"cose"/"cosp"!'
!    endif
!    tas(npts-i+1)=tas(i)
    do i=1, taper_len
    if (trim(taper_type) == 'boxc') then
        fac(i)=1.0
    elseif (trim(taper_type) == 'hann') then
        fac(i)=0.5 - 0.5 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
    elseif (trim(taper_type) == 'hamm') then
        fac(i)=0.54 - 0.46 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
    elseif (trim(taper_type) == 'cose') then
        fac(i)=cos(PI * (i-1) / (2 * taper_len - 1) - PI / 2.0) ** ipwr_t
    elseif (trim(taper_type) == 'cosp') then
        fac(i)=1.0 - cos(PI * (i-1) / (2 * taper_len - 1)) ** ipwr_t
    else
        print*,'taper_type must be among "boxc"/"hann"/"hamm"/"cose"/"cosp"!'
    endif
    fac(npts-i+1)=fac(i)
    enddo    
    win(istart:iend)=fac(1:taper_len)

    print *,'win(istart+20)=',win(istart+30)

end subroutine window_mute


!-----------------------------------------------------------------------------
subroutine window_taper(npts,taper_percentage,taper_type,tas)
    use constants
    implicit none

    ! input parameters
    integer, intent(in) :: npts
    real, intent(in) :: taper_percentage
    character(len=4) :: taper_type
    real, dimension(*), intent(out) :: tas

    integer :: taper_len
    integer :: i

    ! initialization
    tas(1:npts)=1.0

    if (taper_percentage <= 0.0 .or. taper_percentage >= 1.0) then
        taper_len = int(npts*taper_percentage / 2.0)
    else
        taper_len = int(npts*taper_percentage / 2.0 + 0.5)
    endif
    
    !print *,'taper_len=',taper_len    
    do i=1, taper_len
    if (trim(taper_type) == 'boxc') then
        tas(i)=1.0
    elseif (trim(taper_type) == 'hann') then
        tas(i)=0.5 - 0.5 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
    elseif (trim(taper_type) == 'hamm') then
        tas(i)=0.54 - 0.46 * cos(2.0 * PI * (i-1) / (2 * taper_len - 1))
    elseif (trim(taper_type) == 'cose') then
        tas(i)=cos(PI * (i-1) / (2 * taper_len - 1) - PI / 2.0) ** ipwr_t
    elseif (trim(taper_type) == 'cosp') then
        tas(i)=1.0 - cos(PI * (i-1) / (2 * taper_len - 1)) ** ipwr_t
    else
        print*,'taper_type must be among "boxc"/"hann"/"hamm"/"cose"/"cosp"!'
    endif
    tas(npts-i+1)=tas(i)
    enddo

end subroutine window_taper
!----------------------------------------------------------------------
subroutine xcorr_calc(dat1,dat2,npts,i1,i2,ishift,dlnA,cc_max,cc_func)
    ! SAC libarary -- to get optimal shift (not scaled by time interval) 
    ! corresponding to maximal cross-correlation coefficients
    ! cross correlation time T(dat1-dat2) within window i1 and i2
    ! finxed the window for dat2, shift the window for dat1

    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: dat1,dat2
    integer, intent(in) :: npts, i1, i2
    real(kind=CUSTOM_REAL), dimension(npts) :: cc_func
    ! outputs:
    ! ishift = index lag (d-s) for max cross correlation
    ! cc_max = maximum of cross correlation (normalised by sqrt(synthetic*data))
    integer, intent(out) :: ishift
    real(kind=CUSTOM_REAL), intent(out) :: cc_max,dlnA

    ! local variables
    integer :: nlen,k
    integer :: i_left, i_right, i, j, id_left, id_right
    real(kind=CUSTOM_REAL) :: cc, norm, norm_s

    ! initialise shift and cross correlation to zero
    ishift = 0
    dlnA = 0.0
    cc_max = 0.0
    cc_func=0.0_CUSTOM_REAL
    ! print*,'ishift,dlnA,cc_max :',ishift,dlnA,cc_max

    if (i1.lt.1 .or. i1.gt.i2 .or. i2.gt.npts) then
        write(*,*) 'Error with window limits: i1, i2, npts ', i1, i2, npts
        return
    endif

    ! length of window (number of points, including ends)
    nlen = i2 - i1 + 1
    ! power of synthetic signal in window
    norm_s = sqrt(sum(dat2(i1:i2)*dat2(i1:i2)))

    ! left and right limits of index (time) shift search
    ! NOTE: This looks OUTSIDE the time window of interest to compute TSHIFT and CC.
    !       How far to look outside, in theory, should be another parameter.
    !       However, it does not matter as much if the data and synthetics are
    !          zeroed outside the windows, as currently done in calc_criteria.
    i_left = -1*int(nlen/8.0)
    i_right = int(nlen/8.0)
    !!  i_left = -nlen
    !!  i_right = nlen
    k=0
    ! i is the index to shift to be applied to DATA (d)
    do i = i_left, i_right
    k=k+1
    ! normalization factor varies as you take different windows of d
    id_left = max(1,i1+i)      ! left index for data window
    id_right = min(npts,i2+i)  ! right index for data window
    norm = norm_s * sqrt(sum(dat1(id_left:id_right)*(dat1(id_left:id_right))))

    ! cc as a function of i
    cc = 0.0
    do j = i1, i2   ! loop over full window length
    if((j+i).ge.1 .and. (j+i).le.npts) cc = cc + dat2(j)*dat1(j+i)  ! d is shifted by i
    enddo
    !print*,'norm,norm_s :',norm,norm_s
    !open(1,file=trim(output_dir)//'/adj_CC_function',status='unknown')
    !  write(1,'(F8.2)') cc
    !close(1)

    ! normalized by norm of data

    if(norm > SMALL_VAL ) cc = cc/norm
    ! keeping cc-max only
    if (cc .gt. cc_max) then
        cc_max = cc
        ishift = i
        !print*,'i,cc,cc_max :',i,cc,cc_max
    endif

    cc_func(int(nlen*3/8.0)+k)=cc
    !print *,'int(nlen*3/8.0)+k',int(nlen*3/8.0)+k,'cc_func(int(nlen*3/8.0)+k)=',cc_func(int(nlen*3/8.0)+k),'cc=',cc
    !if( DISPLAY_DETAILS) then 
    !open(2,file=trim(output_dir)//'/adj_CC_function',status='unknown')
    !  write(2,'(I5,F8.2)') i,cc
    !endif 


    enddo
    !open(2,file=trim(output_dir)//'/adj_CC_function',status='unknown')
    !write(2,'(I5,F8.2)') i,cc


    dlnA = 0.5 * log( sum(dat1(i1:i2) * dat1(i1:i2)) / sum(dat2(i1:i2) * dat2(i1:i2)) )

    !print*,'ishift,dlnA,cc_max :',ishift,dlnA,cc_max
    ! EXAMPLE: consider the following indexing:
    ! Two records are from 1 to 100, window is i1=20 to i2=41.
    !    --> nlen = 22, i_left = -11, i_right = 11
    !    i   i1+i   i2+i  id_left  id_right
    !  -11     9     30      9        30
    !   -5    15     36     15        36
    !    0    20     41     20        41
    !    5    25     46     25        46
    !   10    31     52     31        52

end subroutine xcorr_calc
!------------------------------------------------------------------------
subroutine xconv(d,s,npts1,npts2,c_array,npts3)
    ! Yanhua -- convolution of d with npts1 points and s with npts2 points 
    ! output is c_array with length npts3
    use constants
    implicit none
    ! inputs
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: s,d
    integer, intent(in) :: npts1,npts2,npts3

    ! outputs:
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: c_array

    ! local variables
    integer :: i_left, i_right, i,j,k
    real(kind=CUSTOM_REAL),dimension(npts1+npts2-1) :: cc_array
    real(kind=CUSTOM_REAL) :: cc

    if (npts3 .lt. 1 .or. npts3 .gt. npts1+npts2-1) then
        write(*,*) 'output length is improper: ', npts3
        return
    endif

    i_left = 2
    i_right = npts1+npts2
    ! i is the index to shift to be applied to DATA (d)
    k=0
    do i = i_left, i_right
    k=k+1
    ! cc as a function of i or k
    cc = 0.
    do j = 1,npts1    ! loop over full window length
    if((i-j) .ge. 1 .and. (i-j) .le. npts2) then
        cc = cc + d(j)*s(i-j)  ! d is shifted by i
    endif
    enddo
    cc_array(k)=cc
    enddo
    i_left=1+ceiling((npts1+npts2-1-npts3)*0.5)
    i_right=npts3+ceiling((npts1+npts2-1-npts3)*0.5)
    c_array(1:npts3)=cc_array(i_left:i_right)

end subroutine xconv
!-----------------------------------------------------------------------
subroutine gaussmf(x,sig,c,npts,f)
    ! Gaussian curve membership function
    ! by Yanhua -- see matlab y = gaussmf(x,[sig c])
    use constants
    implicit none
    ! inputs
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: x
    real(kind=CUSTOM_REAL), intent(in) :: sig,c
    integer, intent(in) :: npts

    ! outputs:
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: f

    integer :: i

    do i=1,npts
    f(i)=exp(-(x(i)-c)**2/(2*sig**2))
    enddo
end subroutine gaussmf
!---------------------------------------------------------------------------
subroutine gauspuls(x,npts,fc,sig,c,fe,f )
    ! Gaussian curve membership function
    use constants
    implicit none
    ! inputs
    real(kind=CUSTOM_REAL), dimension(*), intent(in) :: x
    real(kind=CUSTOM_REAL), intent(in) :: fc,sig,c
    integer, intent(in) :: npts

    ! outputs:
    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: fe, f

    ! compute gaussian envelope
    call gaussmf(x,sig,c,npts,fe)
    ! gaussian modulated sine/cosine function
    f(1:npts) = fe(1:npts) * cos(TWOPI*fc*x(1:npts));

end subroutine gauspuls
! ---------------------------------------------------------------------------
subroutine WT(seism,NSTEP,level,NA)
    use constants
    implicit none

    real(kind=CUSTOM_REAL), dimension(*), intent(out) :: seism
    integer, intent(in) :: NSTEP,level
    integer :: iend,istart,i,j,st
    integer, intent(out) :: NA
    real(kind=CUSTOM_REAL),dimension(:,:),allocatable :: basis, seism_WT
    real(kind=CUSTOM_REAL) :: wc
    real(kind=CUSTOM_REAL),dimension(:), allocatable :: nonzero_basis
    character(len=500) :: fname

    NA=0
    if(level .gt. 0) then
        if(level==12) NA=45046
        if(level==11) NA=22518
        if(level==10) NA=11254
        if(level==9) NA=5622
        if(level==8) NA=2806
        if(level==7) NA=1398
        if(level==6) NA=694
        if(level==5) NA=342
        if(level==4) NA=166
        if(level==3) NA=78
        if(level==2) NA=34
        if(level==1) NA=12
    endif !! level

    if(level .gt. 0 .and. NA .gt. 0) then
        allocate(nonzero_basis(NA))
        allocate(basis(NSTEP,1))
        allocate(seism_WT(NSTEP,1))
        seism_WT(1:NSTEP,1)=0.0

        ! print*,' load basis functions for wavelet Daubechies',nvm,' scale',level
        write(fname,"(a,i0,a,i0)") 'scale_basis_Daubechies',nvm,'_scale',level
        filename='WT_basis/'//trim(fname)//'.dat'
        ! print*,filename
        OPEN (UNIT=20,FILE=filename,STATUS='OLD',action='read',iostat=st)
        if(st>0) then
            print*,'Error opening file: ',filename
            stop
        else
            read(20,*) nonzero_basis
        end if
        close(20)
        ! initialization
        iend=1
        istart=2-NA
        ! find shifted basis 
        do while (istart<=NSTEP)
        basis(1:NSTEP,1)=0.0
        j=0
        do i=istart,iend
        j=j+1
        if(i>=1 .and. i<=NSTEP) then
            basis(i,1)=nonzero_basis(j)
        endif
        enddo
        !! WT
        wc=dot_product(seism(1:NSTEP),basis(1:NSTEP,1))
        !   print*, 'istart=',istart,' wc=',wc
        ! inverse wavelet transform to construct data
        seism_WT(1:NSTEP,1)=seism_WT(1:NSTEP,1)+wc*basis(1:NSTEP,1)
        !    print*, ' prepare for next basis '
        iend=iend+2**level
        istart=istart+2**level
        !    print*, 'istart=',istart,' iend=',iend
        enddo
        seism(1:NSTEP)=seism_WT(1:NSTEP,1)

        deallocate(nonzero_basis)
        deallocate(basis)
        deallocate(seism_WT)
    endif  !! level

end subroutine WT



! ---------------------------------------------------------------------------

subroutine bandpass(x, n,FSTART,FEND,delta_t)
    use constants

    implicit none

    real(kind=4), dimension(*), intent(out) :: x
    real(kind=4),dimension(:), allocatable :: x_sngl
    !!all kind=8
    real(kind=8), intent(in) ::FSTART,FEND,delta_t
    real(kind=8), parameter :: TRBDNDW=0.3
    real(kind=8), parameter :: APARM=30.0
    !real(kind=8), parameter :: APARM=0.0

    integer, parameter :: IORD=2
    integer, parameter :: PASSES=2
    integer, intent(in) :: n


    allocate(x_sngl(n))
    x_sngl(1:n) = x(1:n)
  ! old version - uses old SacLib
  ! does band-pass filter
  !call
  !xapiir(x_sngl,n,'BU',sngl(TRBDNDW),sngl(APARM),IORD,'BP',sngl(FSTART),sngl(FEND),delta_t_sngl,PASSES)
  ! new version, uses subroutines in libsac.a
  ! does band-pass filter
  !print*,'n=',n,'TRBDNDW=',TRBDNDW,'APARM=',APARM,'IORD=',IORD,'FSTART=',FSTART,'FEND=',FEND,'delta_t=',delta_t,'PASSES=',PASSES
  !print*,'before,x_sngl=',x_sngl
    call xapiir(x_sngl,n,'BU',TRBDNDW,APARM,IORD,'BP',FSTART,FEND,delta_t,PASSES)
    !print*,'bandpass finish'
    x(1:n) = x_sngl(1:n)
    deallocate(x_sngl)

end subroutine bandpass

!subroutine bandpass(x,n,f1,f2,delta_t)
!    ! a double-precision wrapper around sac xapiir()
!    ! modified from FLEXWIN subroutines on 26-July-2009
!
!    implicit none
!    integer, intent(in) :: n
!    double precision, intent(inout),  dimension(*) :: x
!    double precision, intent(in) :: f1,f2
!    real, dimension(:), allocatable :: x_sngl
!    double precision, parameter :: TRBDNDW = 0.3
!    double precision, parameter :: APARM = 30.
!    integer, parameter :: IORD = 4
!    integer, parameter :: PASSES = 2
!    real(kind=8), intent(in) ::delta_t   
!    allocate(x_sngl(n))
!
!    x_sngl(1:n) = sngl(x(1:n))
!    !  delta_t_sngl = sngl(delta_t)
!
!    ! old version - uses old SacLib
!    ! does band-pass filter
!    !call
!    !xapiir(x_sngl,n,'BU',sngl(TRBDNDW),sngl(APARM),IORD,'BP',sngl(FSTART),sngl(FEND),delta_t_sngl,PASSES)
!
!    ! new version, uses subroutines in libsac.a
!    ! does band-pass filter
!    ! BU - butterworth
!    ! BP - bandpass
!    ! LQY: Shouldn't  delta_t_sngl = sngl(delta_t) still be done? same for
!    ! f1,f2?
!    print *,'n=',n,'TRBDNDW,APARM,IORD=',TRBDNDW,APARM,IORD
!    print *,'f1,f2,delta_t,PASSES=',f1,f2,delta_t,PASSES
!    call xapiir(x_sngl,n,'BU',TRBDNDW,APARM,IORD,'BP',f1,f2,delta_t,PASSES)
!
!    x(1:n) = dble(x_sngl(1:n))
!
!    deallocate(x_sngl)
!
!  end subroutine bandpass








subroutine split_string(instring,delimiter,outstring,nstring)
    use constants,only: MAX_STRING_LEN, MAX_KERNEL_NUM
    implicit none

    character(len=MAX_STRING_LEN),intent(in) :: instring
    character(len=MAX_STRING_LEN),intent(inout) :: outstring(MAX_KERNEL_NUM)
    integer,intent(out) :: nstring
    character,intent(in) :: delimiter

    ! local parameters
    integer :: index,istring
    character(len=MAX_STRING_LEN) :: scan_string, remaining_string

    ! intialization 
    scan_string = TRIM(instring)
    remaining_string=TRIM(instring)

    ! try 
    index = SCAN(scan_string,delimiter)
    istring=0

    ! loop
    do while (len(trim(remaining_string))>0 .and. index>0)

    istring=istring+1
    outstring(istring) = scan_string(1:index-1)
    remaining_string = scan_string(index+1:)

    scan_string = trim(remaining_string)
    index = SCAN(scan_string,delimiter)

    enddo

    nstring=istring+1
    ! the last string 
    outstring(nstring)= remaining_string
end subroutine split_string
! ---------------------------------------------------------------------------
