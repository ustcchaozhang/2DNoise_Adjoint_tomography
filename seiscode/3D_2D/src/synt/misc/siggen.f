c this is <siggen.f>
c------------------------------------------------------------------------------
c Copyright 2001, 2010 by Thomas Forbriger (IMGF Frankfurt)
c
c SIGnal GENerator
c
c ----
c SIGGEN is free software; you can redistribute it and/or modify
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
c
c REVISIONS and CHANGES
c    24/10/2001   V1.0   Thomas Forbriger
c    02/11/2001   V1.1   boxcar
c    20/11/2001   V1.2   Gerhards weight-drop source signal
c    21/11/2001   V1.3   added first sine half-period
c    28/08/2002   V1.4   added noise
c    01/07/2002   V1.5   added test triangle
c    12/09/2007   V1.6   provide GSL random numbers
c    05/04/2011   V1.7   provide logarithmic sweep
c    20/04/2011   V1.8   provide better control for sweep
c    16/02/2012   V1.9   added cosine function (damb)
c    25/05/2012   V2.0   Support different output file type (libfapidxx
c                        interface in use) (damb)
c    03/06/2012   V2.1   Use new function from libsffu to support
c                        arbitrary many samples
c    12/06/2014   V2.2   document properties of Mueller-Bruestle
c                        amplitude spectrum
c    14/04/2015   V2.3   provide square wave
c    21/09/2015   V2.4   provide damped cosine
c
c==============================================================================
c
      program siggen
c
      character*(*) version
      parameter(version='SIGGEN   V2.4   SIGnal GENerator')
c
c parameters
      integer nsig,ncyc,s
      double precision f,t,a,d,ta,te,td,tm,f1,f2,expo,b,c
      logical overwrite
      character*80 filename
c functions
      real tf_rand
c internal parameters
      integer nsamples,i
      double precision ti,tend,bx,tx,dx,h,g
      double precision myexpo
      real pi
      parameter(pi= 3.1415926535897)
      integer lu
      parameter(lu=10)
c data space
      integer maxsamples
      parameter(maxsamples=10000000)
      real series(maxsamples)
      double precision dseries(maxsamples)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument, outformat
      parameter(maxopt=19)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/'-D', '-v', '-o', '-f', '-T', '-a', '-d',
     &           '-Ta','-Te','-n','-Td','-Tm','-f1','-f2','-e',
     &           '-b','-c','-s','-ot'/
      data opthasarg/3*.FALSE.,16*.TRUE./
      data optarg/3*'-','20.',2*'1.','.001','0.','.02',
     &           '5','1.e20','.005',2*'20.',4*'1','sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: siggen type file [-v] [-o] [-f f] [-T T] [-a a] [-d d]'
        print *,'                     [-Ta Ta] [-Te Te] [-Td Td] [-Tm Tm]'
        print *,'                     [-n n] [-f1 f1] [-f2 f2] [-b b] [-c c]'
        print *,'                     [-s s] [-ot f]'
        print *,'   or: siggen -help'
        print *,' '
        print *,'SIGnal GENerator'
        print *,' '
        if (argument(1:5).ne.'-help') then
          print *,'provided signals: '
          print *,'type   signal'
          print *,'-------------------------'
          print *,'1      sine wave'
          print *,'2      Mueller-Bruestle function'
          print *,'3      first order derivative of Mueller-Bruestle function'
          print *,'4      second order derivative of Mueller-Bruestle function'
          print *,'5      damped modulated sine'
          print *,'6      damped asymmetric sine pulse'
          print *,'7      damped asymmetric cosine pulse'
          print *,'8      single boxcar'
          print *,'9      Gerhard''s weight drop'
          print *,'10     Gerhard''s weight drop (spike version)'
          print *,'11     sine, first half-period'
          print *,'12     white noise'
          print *,'13     triangle test signal'
          print *,'14     white noise (GSL random number generator)'
          print *,'15     decadic sweep'
          print *,'16     cosine wave'
          print *,'17     square wave'
          print *,'18     damped cosine wave'
          stop 'ERROR: wrong number of arguments'
        endif 
c 
        print *,'type         select signal type ''type'' (see list below)'
        print *,'file         output file'
        print *,' '
        print *,'-o           overwrite existing output file'
        print *,'-v           be verbose'
        print *,'-T T         set time parameter to ''T''s'
        print *,'             this is the length of the signal'
        print *,'             (default: ',optarg(5)(1:3),')'
        print *,'-d d         set time parameter to ''d''s'
        print *,'             this is the sampling interval'
        print *,'             (default: ',optarg(7)(1:3),')'
        print *,'-a a         set amplitude parameter to ''a'' '
        print *,'             this is the signal amplitude'
        print *,'             (default: ',optarg(6)(1:3),')'
        print *,'-b b         set parameter ''b'' '
        print *,'-c c         set parameter ''c'' '
        print *,'-s s         set parameter ''s'' '
        print *,' '
        print *,'-f f         set frequency parameter to ''f''Hz'
        print *,'             this is the fundamental signal frequency'
        print *,'             (default: ',optarg(4)(1:3),')'
        print *,' '
        print *,'-Ta Ta       set time parameter Ta to ''Ta''s'
        print *,'             (default: ',optarg(8)(1:3),')'
        print *,'-Te Te       set time parameter Te to ''Te''s'
        print *,'             (default: ',optarg(9)(1:3),')'
        print *,'-Td Td       set time parameter Td to ''Td''s'
        print *,'             (default: ',optarg(11)(1:3),')'
        print *,'-Tm Tm       set time parameter Tm to ''Tm''s'
        print *,'             (default: ',optarg(12)(1:3),')'
        print *,'-n n         set cycle parameter n to ''n'' cycles'
        print *,'             (default: ',optarg(10)(1:3),')'
        print *,'-e e         set exponent parameter e to ''e'' '
        print *,'             (default: ',optarg(15)(1:3),')'
        print *,'-f1 f1       set frequency parameter f1 to ''f1''Hz'
        print *,'             (default: ',optarg(13)(1:3),')'
        print *,'-f2 f2       set frequency parameter f2 to ''f2''Hz'
        print *,'             (default: ',optarg(14)(1:3),')'
        print *,'-ot f        select output data file format f'
        print *,' '
        print *,'type   signal'
        print *,'-------------------------'
        print *,' '
        print *,'1      sine wave'
        print *,'       f(t)=a*sin(2*pi*f*t)'
        print *,' '
        print *,'2      Mueller-Bruestle function'
        print *,'       f(t)=0                                 t <= Ta'
        print *,'       f(t)=a*(-0.75*cos(pi*(t-Ta)/(Te-Ta))+'
        print *,'            0.25*cos(pi*(t-Ta)/(Te-Ta))**3)   Ta < t < Te'
        print *,'       f(t)=a                                 t >= Te'
        print *,' '
        print *,'       It is a smooth step starting at Ta at value 0'
        print *,'       and leading to value a at Te.'
        print *,' '
        print *,'3      first order derivative of Mueller-Bruestle function'
        print *,'       f(t)=0                                 t <= Ta'
        print *,'       f(t)=a*(0.75*pi/(Te-Ta)*'
        print *,'            dsin(pi*(t-Ta)/(Te-Ta))**3)       Ta < t < Te'
        print *,'       f(t)=0                                 t >= Te'
        print *,' '
        print *,'       It is a smooth one-sided impulse starting at Ta'
        print *,'       and ending at Te.'
        print *,' '
        print *,'       If the duration Te-Ta of the signal is dT, then'
        print *,'       the amplitude spectrum of the signal has zeroes at'
        print *,'         f1=(1.5*n)/dT'
        print *,'       with n being a positive integer. Half-width is'
        print *,'       approximately at f=1.16/dT.'
        print *,' '
        print *,'4      second order derivative of Mueller-Bruestle function'
        print *,'       f(t)=0                                 t <= Ta'
        print *,'       f(t)=a*(9*pi**2/(4*(Te-Ta)**2)*'
        print *,'            sin(pi*(t-Ta)/(Te-Ta))**2*'
        print *,'            cos(pi*(t-Ta)/(Te-Ta)))           Ta < t < Te'
        print *,'       f(t)=0                                 t >= Te'
        print *,' '
        print *,'       It is a smooth two-sided impulse starting at Ta'
        print *,'       and ending at Te with a zero-crossing at (Ta+Te)/2.'
        print *,' '
        print *,'5      damped modulated sine'
        print *,' '
        print *,'       f(t)=0                                 t <= Ta'
        print *,'       f(t)=a*(sin(2*pi*(f1+b*(t-Ta))*(t-Ta))*'
        print *,'            exp(-(t-Ta)/Td)))                 Ta < t < Tend'
        print *,'       f(t)=0                                 t >= Tend'
        print *,' '
        print *,'       Tend=Ta+2.*n/(f1+f2)'
        print *,'       b=(f2**2-f1**2)/(4.*n)'
        print *,' '
        print *,'       The signal starts at Ta with value 0 and'
        print *,'       terminates at Tend after n cycles with value 0.'
        print *,'       The frequency varies from f1 at Ta to f2 at Tend.'
        print *,' '
        print *,'6      damped asymmetric sine pulse'
        print *,' '
        print *,'       f(t)=0                                 t <= Ta'
        print *,'       f(t)=a*(sin(2*pi*(1.-(1.-(t-Ta)/(Te-Ta))**b))*'
        print *,'            exp(-(t-Ta)/Td)))                 Ta < t < Te'
        print *,'       f(t)=0                                 t >= Te'
        print *,' '
        print *,'       b=log(0.5)/log(1.-(Tm-Ta)/(Te-Ta))'
        print *,' '
        print *,'       The signal starts at Ta with value 0 has a single'
        print *,'       zero-crossing at Tm and terminates at Te with'
        print *,'       frequency 0Hz and value 0.'
        print *,' '
        print *,'7      damped asymmetric cosine pulse'
        print *,' '
        print *,'       f(t)=a*(cos(1.5*pi*(1.-(1.-ti/te)**b))*'
        print *,'            exp(-t/Td)))                      t < Te'
        print *,'       f(t)=0                                 t >= Te'
        print *,' '
        print *,'       b=log(2./3.)/log(1.-Tm/Te)'
        print *,' '
        print *,'       The signal starts at time 0s with value a, has a'
        print *,'       single zero-crossing at Tm and terminates at Te'
        print *,'       with value 0 and frequency 0Hz.'
        print *,' '
        print *,'8      single boxcar'
        print *,' '
        print *,'       All samples are zero. Only ''n'' samples after'
        print *,'       ''Ta'' seconds will be ''a''.'
        print *,' '
        print *,'9      Gerhard''s weight drop'
        print *,' '
        print *,'       f(t)=0                                t < Ta'
        print *,'       f(t)=-a                               Ta <= t <= Tm'
        print *,'       f(t)=2*a*(Tm-Ta)/(sqrt(pi)*(Te-Tm))*'
        print *,'            sqrt((t-Tm)/(Te-Tm))*'
        print *,'             exp(-(t-Tm)/(Te-Tm))            Tm < t'
        print *,' '
        print *,'10     Gerhard''s weight drop (spike version)'
        print *,' '
        print *,'       f(t)=0                                t < Ta'
        print *,'       f(t)=-a                               Ta <= t < Tm'
        print *,'       f(t)=a*(Tm-Ta)/d                      t = Tm'
        print *,'       f(t)=0                                t > Tm'
        print *,' '
        print *,'11     sine, first half-period'
        print *,'       f(t)=0                                t <= Ta'
        print *,'       f(t)=a*sin(pi*(t-Ta)/(Te-Ta))         Ta < t < Te'
        print *,'       f(t)=0                                t >= Te'
        print *,' '
        print *,'       It is a one-sided impulse starting at Ta'
        print *,'       and ending at Te.'
        print *,' '
        print *,'12     white noise'
        print *,'       n: read n values from random stream'
        print *,'       s: use s to initialize random number generator'
        print *,' '
        print *,'13     triangle test signal'
        print *,'       If D(t) is a triangle signal with 2*a peak'
        print *,'       to peak amplitude and period 1/f, the output'
        print *,'       signal will be'
        print *,' '
        print *,'       f(t)=D(t)+b+c*t'
        print *,' '
        print *,'       Choosing a=0 will result in a linear ramp.'
        print *,' '
        print *,'14     white noise (GSL random number generator)'
        print *,'       gaussian noise from GSL generator'
        print *,'       a: specifies the rms value.'
        print *,' '
        print *,'15     decadic sweep'
        print *,'       a: amplitude'
        print *,'       f: initial frequency'
        print *,'       n: number of cycles per frequency decade'
        print *,' '
        print *,'16     cosine wave'
        print *,'       f(t)=a*cos(2*pi*f*t)'
        print *,' '
        print *,'17     square wave'
        print *,'       f(t)=+a if sin(2*pi*f*(t-Ta)) > 0'
        print *,'       f(t)=-a if sin(2*pi*f*(t-Ta)) < 0'
        print *,' '
        print *,'18     damped cosine wave'
        print *,'       f(t)=a*cos(2*pi*f*t)*exp(-t/Td)'
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(1, argument)
      read(argument, *) nsig
      call getarg(2, filename)

      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      read(optarg(4), *) f
      read(optarg(5), *) t
      read(optarg(6), *) a
      read(optarg(7), *) d
      read(optarg(8), *) ta
      read(optarg(9), *) te
      read(optarg(10), *) ncyc
      read(optarg(11), *) td
      read(optarg(12), *) tm
      read(optarg(13), *) f1
      read(optarg(14), *) f2
      read(optarg(15), *) expo
      read(optarg(16), *) b
      read(optarg(17), *) c
      read(optarg(18), *) s
      outformat=optarg(19)

c perform some checks
      nsamples=int(t/d)
      if (nsamples.gt.maxsamples) stop 'ERROR: too many samples'

c
c------------------------------------------------------------------------------
c go
c
      if (nsig.eq.1) then
        if (verbose) then
          print *,'sine wave'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'frequency','f',f,'Hz'
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          series(i)=sngl(a*sin(2*pi*(i-1)*d*f))
        enddo
c------------
      elseif (nsig.eq.2) then
        if (verbose) then
          print *,'Mueller-Bruestle function'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'end of wavelet','Te',te,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.te) then
            series(i)=sngl(a*(0.5-0.75*cos(pi*(ti-ta)/(te-ta))+
     &                    0.25*cos(pi*(ti-ta)/(te-ta))**3))
          else
            series(i)=sngl(a)
          endif
        enddo
c------------
      elseif (nsig.eq.3) then
        if (verbose) then
          print *,'first order derivative of Mueller-Bruestle function'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'end of wavelet','Te',te,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.te) then
            series(i)=sngl(a*(3.*pi/(4.*(te-ta))*
     &                     sin(pi*(ti-ta)/(te-ta))**3))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.4) then
        if (verbose) then
          print *,'second order derivative of Mueller-Bruestle function'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'end of wavelet','Te',te,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.te) then
            series(i)=sngl(a*(9*pi**2/(4.*(te-ta)**2)*
     &           sin(pi*(ti-ta)/(te-ta))**2*
     &           cos(pi*(ti-ta)/(te-ta))))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.5) then
        if (verbose) then
          print *,'modulated and damped sine wave'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'damping time constant','Td',td,'s'
          print 51,'number of cycles','n',ncyc,' '
          print 50,'initial frequency','f1',f1,'Hz'
          print 50,'final frequency','f2',f2,'Hz'
        endif
        tend=ta+2.*ncyc/(f1+f2)
        bx=(f2**2-f1**2)/(4.*ncyc)
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
          print 53,'end of signal',tend,'s'
          print 53,'frequency modulation slope',bx,'Hz/s'
          print 53,'initial frequency',f1,'Hz'
          print 53,'final frequency',f1+2.*bx*(tend-ta),'Hz'
          print 53,'total cycles',(f1+bx*(tend-ta))*(tend-ta),' '
        endif
        do i=1,nsamples
          ti=d*float(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.tend) then
            series(i)=sngl(a*(sin(2*pi*(f1+bx*(ti-ta))*(ti-ta))*
     &                 exp(-(ti-Ta)/Td)))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.6) then
        if (verbose) then
          print *,'asymmetric sine pulse'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'zero of wavelet','Tm',tm,'s'
          print 50,'end of wavelet','Te',te,'s'
          print 50,'damping time constant','Td',td,'s'
        endif
        bx=log(0.5)/log(1.-(Tm-ta)/(te-ta))
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
          print 53,'frequency modulation exponent',bx,' '
          print 53,'phase at ta',(1.-myexpo(dble(1.-(ta-ta)/(te-ta)),bx)),'2pi'
          print 53,'phase at tm',(1.-myexpo(dble(1.-(tm-ta)/(te-ta)),bx)),'2pi'
          print 53,'phase at te',(1.-myexpo(dble(1.-(te-ta)/(te-ta)),bx)),'2pi'
          print 53,'frequency at te',
     &       bx/(te-ta)*myexpo(dble(1.-(te-ta)/(te-ta)),(bx-1.)),'Hz'
        endif
        do i=1,nsamples
          ti=d*float(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.te) then
            series(i)=sngl(a*(sin(2*pi*
     &                 (1.-myexpo(dble(1.-(ti-ta)/(te-ta)),bx)))*
     &                 exp(-(ti-Ta)/Td)))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.7) then
        if (verbose) then
          print *,'asymmetric cosine pulse'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'zero of wavelet','Tm',tm,'s'
          print 50,'end of wavelet','Te',te,'s'
          print 50,'damping time constant','Td',td,'s'
        endif
        bx=log(2./3.)/log(1.-Tm/te)
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
          print 53,'frequency modulation exponent',bx,' '
          print 53,'phase at tm',1.5*(1.-myexpo(dble(1.-tm/te),bx)),'pi'
          print 53,'phase at te',1.5*(1.-myexpo(dble(1.-te/te),bx)),'pi'
          print 53,'frequency at te',
     &       (bx*3./(4.*te))*myexpo(dble(1.-te/te),(bx-1.)),'Hz'
        endif
        do i=1,nsamples
          ti=d*float(i-1)
          if(ti.le.te) then
            series(i)=sngl(a*(cos(1.5*pi*
     &                 (1.-myexpo(dble(1.-ti/te),bx)))*
     &                 exp(-ti/Td)))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.8) then
        if (verbose) then
          print *,'boxcar'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 51,'number of non-zero samples','n',ncyc,' '
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*float(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif (ncyc.gt.0) then
            series(i)=sngl(a)
            ncyc=ncyc-1
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.9) then
        if (verbose) then
          print *,'Gerhards weight drop'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'weight release','Ta',ta,'s'
          print 50,'weight impact','Tm',tm,'s'
          print 50,'impulse transfer e-time','Te',te,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*float(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif (ti.lt.tm) then
            series(i)=sngl(-a)
          else
            series(i)=sngl(2*a*(Tm-Ta)/(sqrt(pi)*(Te-Tm))*
     &              sqrt((ti-Tm)/(Te-Tm))*
     &              exp(-(ti-Tm)/(Te-Tm)))
          endif
        enddo
c------------
      elseif (nsig.eq.10) then
        if (verbose) then
          print *,'Gerhards weight drop (spike version)'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'weight release','Ta',ta,'s'
          print 50,'weight impact','Tm',tm,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        ncyc=1
        do i=1,nsamples
          ti=d*float(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif (ti.lt.tm) then
            series(i)=sngl(-a)
          elseif (ncyc.gt.0) then
            ncyc=0
            series(i)=sngl(a*(tm-ta)/d)
          else
            series(i)=0
          endif
        enddo
c------------
      elseif (nsig.eq.11) then
        if (verbose) then
          print *,'sine, first half-period'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'onset of wavelet','Ta',ta,'s'
          print 50,'end of wavelet','Te',te,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          ti=d*(i-1)
          if (ti.lt.ta) then
            series(i)=0.
          elseif(ti.le.te) then
            series(i)=sngl(a*sin(pi*(ti-ta)/(te-ta)))
          else
            series(i)=0.
          endif
        enddo
c------------
      elseif (nsig.eq.12) then
        if (verbose) then
          print *,'white noise'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 51,'random seed','s',s,' '
          print 51,'drop','n',ncyc,' '
        endif
        call tf_srand(s)
        do i=1,ncyc
          bx=dble(tf_rand())
          if (debug) print *,'DEBUG: tf_rand: ',bx
        enddo
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          bx=dble(tf_rand())
          series(i)=sngl(a*(2.*bx-1.))
        enddo
c------------
      elseif (nsig.eq.13) then
        if (verbose) then
          print *,'triangle with trend and offset superimposed'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'frequency','f',f,'Hz'
          print 50,'offset','b',b,' '
          print 50,'trend','c',c,'1/s'
        endif
c half period
        bx=1/(2.*f)
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
          print 53,'half period',bx,' '
        endif
        do i=1,nsamples
c time of sample
          ti=(i-1)*d
c time of next root
          tx=bx*nint(ti/bx)
          if (cos(ti*pi*2.*f).gt.0.) then
c slope
            dx=2.*a/bx
          else
            dx=-2.*a/bx
          endif
          series(i)=sngl(dx*(ti-tx)+b+c*ti)
        enddo
c------------
      elseif (nsig.eq.14) then
        if (verbose) then
          print *,'white gaussian noise noise (GSL generator)'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series length','T',t,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        call tf_gsl_rng_ugaussian(dseries, nsamples)
        do i=1,nsamples
          series(i)=sngl(a*dseries(i))
        enddo
c------------
      elseif (nsig.eq.15) then
        if (verbose) then
          print *,'sweep with constant number of cycles per ',
     &            'frequency decade'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'sampling interval','d',d,'s'
          print 50,'time series length','T',t,'s'
          print 50,'initial frequency','f',f,'Hz'
          print 51,'number of cycle per decade','n',ncyc,' '
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        h=(ncyc*log10(exp(1.d0))/f)-d
        g=-log10(d+h)
        do i=1,nsamples
          series(i)=sngl(a*sin((g+log10(i*d+h))*2*pi*ncyc))
        enddo
c------------
      elseif (nsig.eq.16) then
        if (verbose) then
          print *,'cosine wave'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'frequency','f',f,'Hz'
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          series(i)=sngl(a*cos(2*pi*(i-1)*d*f))
        enddo
c------------
      elseif (nsig.eq.17) then
        if (verbose) then
          print *,'square wave'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'frequency','f',f,'Hz'
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'rising edge at ','Ta',ta,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          if (sin(2*pi*((i-1)*d-ta)*f).gt.0.) then
            series(i)=sngl(a)
          else
            series(i)=sngl(-a)
          endif
        enddo
c------------
      elseif (nsig.eq.18) then
        if (verbose) then
          print *,'damped cosine wave'
          print *,'parameters:'
          print 50,'amplitude','a',a,' '
          print 50,'frequency','f',f,'Hz'
          print 50,'sampling interval','d',d,'s'
          print 50,'time series lentgh','T',t,'s'
          print 50,'decay time','Td',t,'s'
        endif
        if (verbose) then
          print *,'derived parameters:'
          print 52,'number of samples',nsamples,' '
        endif
        do i=1,nsamples
          series(i)=sngl(a*cos(2*pi*(i-1)*d*f)*exp(-1.*(i-1)*d/td))
        enddo
c------------
      else
        stop 'ERROR: unknown signal'
      endif

      if (debug) then
        print *,'DEBUG: display samples'
        do i=1,nsamples
          print *, i, series(i)
        enddo
      endif

      if (overwrite) then
        call sff_new(lu,filename,i)
        if (i.ne.0) stop 'ERROR: deleting output file'
      endif
c open output file
      call sff_select_output_format(outformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting output file format'
c      call sff_wopens(lu,filename,'NSP',)
c      call sff_close(lu,i)
c      if (i.ne.0) stop 'ERROR: closing output file'
      call sffu_simpleopen(lu,filename)
      call sffu_simplewrite_external_ws(lu, .true., series, nsamples,
     &  sngl(d), 0., series, maxsamples)

      stop
   50 format(3x,a30,1x,a3,1x,f10.3,a4)
   51 format(3x,a30,1x,a3,1x,i10,a4)
   52 format(3x,a30,1x,3x,1x,i10,a4)
   53 format(3x,a30,1x,3x,1x,f10.3,a4)
      end
c 
c----------------------------------------------------------------------
      double precision function myexpo(x,y)
      double precision x,y,r
      if (x.lt.0.d0) stop 'ERROR: argument of myexpo'
      if (x.lt.1.d-2) then
        r=0.d0
      else
        r=exp(y*log(x))
      endif
      myexpo=r
      return
      end

c
c ----- END OF siggen.f -----
