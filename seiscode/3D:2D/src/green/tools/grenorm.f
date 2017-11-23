c this is <grenorm.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c GREens function will be NORMalized
c
c REVISIONS and CHANGES
c    28/06/99   V1.0   Thomas Forbriger
c    29/06/99   V1.1   allow no_norm and response coefficients
c    02/07/99   V1.2   apply taper file to greens function
c    30/09/99   V1.3   allow maximum amplitude value to be set
c    01/06/00   V1.4   - moving average on scaling factors
c                      - do maxamp scaling always (with default value 1)
c                        in one part (not every part)
c               V1.5   improved cosine moving average again
c    23/08/01   V1.6   allow amplitude normalize
c    27/08/01   V1.7   allow amplitude norm with taper file
c    27/03/02   V1.8   support calculation of modulus
c    05/04/02   V1.8a  tell more about what we are doing
c    05/12/02   V1.9   added slowness limit to boxcar
c
c==============================================================================
c
      program grenorm
c
      character*79 version
      parameter(version=
     &  'GRENORM   V1.9   GREens function will be NORMalized')
c
c data
      integer maxslo, maxfreq
      parameter (maxslo=900, maxfreq=600)
c 
      complex green(maxfreq, maxslo)
      real weight(maxfreq, maxslo)
      real frefac(maxfreq), ofrefac(maxfreq)
      real om(maxfreq), slo(maxslo)
      real tom, realpart, imagpart, maxamp
      integer nom, nslo
      character*80 infile, outfile
c magic number for binary file identification
      integer inmagic, cpu, match
      character*4 cmagic, incmagic, wcmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      parameter(wcmagic='123S')
      integer magic
c file
      integer lu
      parameter(lu=20)
c 
      integer i, j, k, l
      real lowlim, intval, absval, slow, value, coslen, pi
      parameter(lowlim=1.e-20,pi=3.14159265358979311599796)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=14)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c options 
      logical opt_gauss, opt_weight, opt_clip, opt_none, opt_resp, opt_tap
      logical opt_mova, opt_amp, opt_modulus
      character*80 weight_file, resp_file, tap_file
      real gauss_center, gauss_width, gauss_limit, maxamp_val, slo_limit
      real clip_level
      integer mova_len
c here are the keys to our commandline options
      data optid/'-d', '-v', '-g', '-w', '-c', '-N', '-r', '-t', '-n', 
     &           '-l','-C','-m','-a','-A'/
      data opthasarg/2*.FALSE.,2*.TRUE.,2*.FALSE.,6*.TRUE.,2*.false./
      data optarg/2*'-','5.,5.,0.',5*'-','1.','0.','1.','10',2*'-'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: grenorm infile outfile [-v] [-d] [-c]'
        print *,'       [-g c,w,l | -w file | -N] [-r file] [-t file]'
        print *,'       [-n val] [-l slo] [-C val] [-m l] [-a] [-A]'
        print *,'   or: grenorm -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'GREens function will be NORMalized'
        print *,' '
        print *,'-v           verbose'
        print *,'-d           debug'
        print *,'-g c,w,l     use gaussian taper with'
        print *,'             center: c (s/km)'
        print *,'             width: w (s/km)'
        print *,'             limit: l (s/km)'
        print *,'-w file      use weights from ''file'' to define taper'
        print *,'-c           clip coefficients at absolute value of 1.'
        print *,'-C val       set clip level to ''val'''
        print *,'             (default: ',optarg(11)(1:4),')'
        print *,'-N           do NOT normalize at all'
        print *,'-r file      read response coefficients from ''file'' and'
        print *,'             convolve green with response.'
        print *,'-t file      apply taper from ''file'''
        print *,'             the greens coefficients are simply'
        print *,'             scaled by the taper factors (multiplied'
        print *,'             with them)'
        print *,'-n val       normalize maximum amplitude to val'
        print *,'             (default: ',optarg(9)(1:4),')'
        print *,'             this has nothing to do with the -a option'
        print *,'             in any case the result of any of the methods'
        print *,'             will be scaled to a maximum amplitude'
        print *,'             except when -N is selected'
        print *,'-l slo       set slowness limit for finding maximum amplitude'
        print *,'             value - only slowness values greater than ''slo'''
        print *,'             will be used (units: s/km)'
        print *,'-m l         when normalizing frequencies: apply moveing'
        print *,'             of length ''l'' to scaling factors'
        print *,'-a           normalize to maximum amplitude not to integral'
        print *,'-A           take real absolute value (modulus) of'
        print *,'             coefficients and return them.'
        print *,' '
        print *,'Default method is to use a boxcar taper with a value of'
        print *,'1. for each coefficient.'
        print *,' '
        print *,'Notice: There is a hierarchy in method selection'
        print *,' '
        print *,'1. priority:'
        print *,'   -N        do not normalize'
        print *,'       -w, -g, -n, -m, -l, -a become meaningless'
        print *,' '
        print *,'2. priority:'
        print *,'   -g        use gaussian taper'
        print *,'       -a will break the program'
        print *,' '
        print *,'3. priority:'
        print *,'   -w        use weights from file'
        print *,'       -a will change scaling policy'
        print *,' '
        print *,'4. priority:'
        print *,'   default action: scale with boxcar'
        print *,'       -a will change scaling policy'
        print *,' '
        print *,'All these methods scale per frequency! A moving'
        print *,'average may be defined by the -m option and is'
        print *,'used for all of them.'
        print *,' '
        print *,'Next the following switches are used:'
        print *,' '
        print *,'If -r is selected, the coefficients are multiplied'
        print *,'by the response coefficients.'
        print *,' '
        print *,'If -t is selected, the coefficients are multiplied'
        print *,'by the taper factors.'
        print *,' '
        print *,'If -N ist NOT selected, the total maximum amplitude in'
        print *,'range for slownesses larger than the one defined by -l'
        print *,'is scaled to the value selected with -n.'
        print *,' '
        print *,'If -c or -C is selected, the values in the total file' 
        print *,'(all slowness values) are clipped at the level defined'
        print *,'by -C.'
        print *,' '
        print *,'If -A is selected, the modulus is taken from all'
        print *,'coefficients.'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      opt_gauss=optset(3)
      read(optarg(3), *) gauss_center,gauss_width,gauss_limit
      opt_weight=optset(4)
      weight_file=optarg(4)
      opt_clip=optset(5)
      opt_none=optset(6)
      opt_resp=optset(7)
      resp_file=optarg(7)
      opt_tap=optset(8)
      tap_file=optarg(8)
      read(optarg(9), *) maxamp_val
      read(optarg(10), *) slo_limit
      read(optarg(11), *) clip_level
      if (optset(11)) opt_clip=.true.
      opt_mova=optset(12)
      read(optarg(12), *) mova_len
      opt_amp=optset(13)
      opt_modulus=optset(14)
c we use s/m
      slo_limit=slo_limit*1.e-3
c 
      call getarg(1, infile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
c
      print *,'read green file ',infile(1:index(infile, ' '))
      open(lu, file=infile, form='unformatted', status='old', err=89)
c check byte sex
      read(lu, err=88, end=87) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=86)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=88, end=87) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=86)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=88, end=87) (om(i), i=1,nom), (slo(i), i=1,nslo)
      read(lu, err=88, end=87) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=86)
      print *,'green file read and closed'
c----------------------------------------------------------------------
c 
c going to normailze data
c
      if (opt_none) then
        print *,'  do NOT normalize...'
      elseif (opt_gauss) then
        print *,'  using gaussian taper...'
        if (opt_amp) 
     &    stop 'ERROR: amplitude normalize not supported in this mode'
        if (verbose) then
          print *,'    center: ',gauss_center,'s/km'
          print *,'    width:  ',gauss_width,'s/km'
          print *,'    limit:  ',gauss_limit,'s/km'
          print *,'    normalize to integral...'
        endif
        do i=1,nom
          intval=0.
          do j=1,nslo
            slow=slo(j)*1.e3
            absval=abs(green(i,j))
            if (slow.ge.gauss_limit) then
              intval=intval+
     &          absval*exp(-log(10.)*
     &          ((gauss_center-slow)/gauss_width)**2)
            endif
          enddo
          intval=max(intval,lowlim)
          ofrefac(i)=intval
        enddo
c----------------------------------------------------------------------
c 
      elseif (opt_weight) then
        print *,'  using taper defined by weights read from ',
     &    weight_file(1:index(weight_file, ' ')),'...'
        if (verbose) print *,'  read file...'
        open(lu, file=weight_file, form='unformatted', status='old', err=79)

c check byte sex
        read(lu, err=78, end=77) inmagic
        call tf_bytesex(wcmagic, inmagic, cpu, match)
        if (cpu.eq.1) then
          print *,'  running on Intel...'
        elseif (cpu.eq.2) then
          print *,'  running on Motorola...'
        else
          stop '  unknown processor type...'
        endif
        if (match.eq.1) then
          print *,'  matching bytesex - good...'
        elseif (match.eq.2) then 
          print *,
     &     '  bytesex not matching - we will have to swap!'
          stop 'ERROR: do not know how to do that...'
        else
          close(lu, err=76)
          print *,'  bytesex read is ',incmagic
          stop 'ERROR (dat_rweight): bytesex is unkown - oh oh...'
        endif
c 
        read(lu, err=78, end=77) i, j
        if (i.ne.nom) stop 'ERROR: file dimensions (frequency) do not fit'
        if (j.ne.nslo) stop 'ERROR: file dimensions (slowness) do not fit'
        read(lu, err=78, end=77) ((weight(i,j), i=1,nom), j=1,nslo)
        close(lu, err=76)
c 
        if (verbose) print *,'  file read and closed...'
c 
        if (verbose) print *,'    normalize to integral...'
c 
        if (opt_amp) then
          if (verbose) print *,'    normalize to amplitude...'
          do i=1,nom
            intval=0.
            do j=1,nslo
              intval=max(intval,(abs(green(i,j))*weight(i,j)))
            enddo
            intval=max(intval,lowlim)
            ofrefac(i)=intval
          enddo
        else
          if (verbose) print *,'    normalize to integral...'
          do i=1,nom
            intval=0.
            do j=1,nslo
              intval=intval+abs(green(i,j))*weight(i,j)
            enddo
            intval=max(intval,lowlim)
            ofrefac(i)=intval
          enddo
        endif
c 
c----------------------------------------------------------------------
c 
      else
        print *,'  using default taper (boxcar)...'
        print *,'    slowness limit: ',slo_limit*1.e3
        if (opt_amp) then
          if (verbose) then
            print *,'    normalize to amplitude...'
          endif
          do i=1,nom
            intval=0.
            do j=1,nslo
              if (slo(j).ge.slo_limit) 
     &          intval=max(intval,abs(green(i,j)))
            enddo
            intval=max(intval,lowlim)
            ofrefac(i)=intval
          enddo
        else
          if (verbose) then
            print *,'    normalize to integral...'
          endif
          do i=1,nom
            intval=0.
            do j=1,nslo
              if (slo(j).ge.slo_limit) intval=intval+abs(green(i,j))
            enddo
            intval=max(intval,lowlim)
            ofrefac(i)=intval
          enddo
        endif
      endif
c
c----------------------------------------------------------------------
c
      if (.not.(opt_none)) then
        if (opt_mova) then
          if (verbose) print *,'    apply moving average to factors...'
          do i=1,nom
            k=i-int(mova_len/2)
            l=i+int(mova_len/2)
c            if (k.lt.1) then
c              l=l+1-k
c              k=1
c            endif
c            if (l.gt.nom) then
c              k=k-l+nom
c              l=nom
c            endif
            k=max(1,k)
            l=min(nom,l)
            value=0.
            intval=0.
c            coslen=float(max(i-k,l-i)+1)
            coslen=float(k-l+1)
            do j=k,l
              value=value+ofrefac(i)*(1.+cos(2.*pi*float(j-i)/coslen))
              intval=intval+(1.+cos(2.*pi*float(j-i)/coslen))
            enddo
            frefac(i)=value/intval
          enddo
        else
          do i=1,nom
            frefac(i)=ofrefac(i)
          enddo
        endif
        if (verbose) print *,'    rescale amplitudes...'
        do i=1,nom
          if (debug) print *,'  i,fac',i,1./frefac(i)
          do j=1,nslo
            green(i,j)=green(i,j)/frefac(i)
          enddo
        enddo
      endif
c
c----------------------------------------------------------------------
c 
c convolve with response if desired
c  
      if (opt_resp) then
        print *,'  convolve with response from ',
     &    resp_file(1:index(resp_file,' ')),'...'
        if (verbose) print *,'    read file...'
        open(lu, file=resp_file, status='old', err=69)
        read(lu, '(/////)', err=68, end=67) 
        read(lu, *, err=68, end=67) i
        if (i.ne.nom) stop 'ERROR: number of frequencies does not match'
        if (verbose) print *,'    read and convolve...'
        do i=1,nom
          read(lu, *, err=68, end=67) 
     &      tom, realpart, imagpart
          if ((abs(tom-om(i))/tom).gt.1.e-4) 
     &      stop 'ERROR: frequencies do not match'
          if (debug) print *,'omega: ',tom,' real, imag ', realpart, imagpart
          do j=1,nslo
            green(i,j)=green(i,j)*cmplx(realpart,imagpart)
          enddo
        enddo
        close(lu, err=66)
        if (verbose) print *,'    file read and closed...'
      endif
c----------------------------------------------------------------------
c 
c apply taper defined by weights read from file
c  
      if (opt_tap) then
        print *,'  apply taper defined by weights read from ',
     &    tap_file(1:index(tap_file, ' ')),'...'
        if (verbose) print *,'  read file...'
        open(lu, file=tap_file, form='unformatted', status='old', err=79)

c check byte sex
        read(lu, err=78, end=77) inmagic
        call tf_bytesex(wcmagic, inmagic, cpu, match)
        if (cpu.eq.1) then
          print *,'  running on Intel...'
        elseif (cpu.eq.2) then
          print *,'  running on Motorola...'
        else
          stop '  unknown processor type...'
        endif
        if (match.eq.1) then
          print *,'  matching bytesex - good...'
        elseif (match.eq.2) then 
          print *,
     &     '  bytesex not matching - we will have to swap!'
          stop 'ERROR: do not know how to do that...'
        else
          close(lu, err=76)
          print *,'  bytesex read is ',incmagic
          stop 'ERROR (dat_rweight): bytesex is unkown - oh oh...'
        endif
c 
        read(lu, err=78, end=77) i, j
        if (i.ne.nom) stop 'ERROR: file dimensions (frequency) do not fit'
        if (j.ne.nslo) stop 'ERROR: file dimensions (slowness) do not fit'
        read(lu, err=78, end=77) ((weight(i,j), i=1,nom), j=1,nslo)
        close(lu, err=76)
c 
        if (verbose) print *,'  file read and closed...'
c 
        do i=1,nom
          do j=1,nslo
            green(i,j)=green(i,j)*weight(i,j)
          enddo
        enddo
      endif
c----------------------------------------------------------------------
c
c set maximum amplitude value
c
      if (.not.(opt_none)) then
        print *,'  set maximum amplitude to ',maxamp_val
        maxamp=abs(green(nom,nslo))
        do i=1,nom
          do j=1,nslo
            if (slo(j).ge.slo_limit) maxamp=max(maxamp,abs(green(i,j)))
          enddo
        enddo
        maxamp=maxamp/maxamp_val
        do i=1,nom
          do j=1,nslo
            green(i,j)=green(i,j)/maxamp
          enddo
        enddo
      endif
c----------------------------------------------------------------------
c 
c clip if desired
c  
      if (opt_clip) then
        print *,'  clip data...'
        do i=1,nom
          do j=1,nslo
            absval=abs(green(i,j))
            if (absval.gt.clip_level) green(i,j)=clip_level*green(i,j)/absval
          enddo
        enddo
      endif

c----------------------------------------------------------------------
c 
c calculate modulus
c
      if (opt_modulus) then
        print *,'take modulus...'
        do i=1,nom
          do j=1,nslo
            absval=abs(green(i,j))
            green(i,j)=cmplx(absval,0.)
          enddo
        enddo
      endif

c
c write green code (easy to use)
c 
      print *,' '
      print *,'opening green file ',outfile(1:index(outfile,' ')),
     &    ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, nslo
      write(lu, err=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      write(lu, err=97) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=96)
      print *,'green file written and closed'
c 
      stop
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
   89 stop 'ERROR: opening green file'
   88 stop 'ERROR: reading green file'
   87 stop 'ERROR: reading green file - unexpected end'
   86 stop 'ERROR: closing green file'
   79 stop 'ERROR: opening green weight file'
   78 stop 'ERROR: reading green weight file'
   77 stop 'ERROR: reading green weight file - unexpected end'
   76 stop 'ERROR: closing green weight file'
   69 stop 'ERROR: opening response file'
   68 stop 'ERROR: reading response file'
   67 stop 'ERROR: reading response file - unexpected end'
   66 stop 'ERROR: closing response file'
      end
c
c ----- END OF grenorm.f -----
