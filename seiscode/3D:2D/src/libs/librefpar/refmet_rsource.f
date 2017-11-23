c this is <sub/refmet_rsource.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c read source file
c
c ============================================================================
c
c this is part of the REFMET reflectivity program
c (for comments and revisions see also the main source refmet.f)
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
c REVISIONS and CHANGES
c 09/02/97   changed interpretation of Thd to be also dominant period 
c            in case of impulse source
c 15/10/00   be more specific about time parameters
c 20/10/01   explain the source amplitude definition in detail
c 24/10/01   incomplete source description
c 12/06/14   zeroes of MB-function
c
c======================================================================
c
c read source file
c
c outsig now holds former typ:
c   outsig=1  source signal will be once integrated
c   outsig=2  source signal will be taken as is
c   outsig=3  source signal will be once differentiated
c
c typ now selects different source model types
c   typ=1  source is given by moment tensor
c   typ=2  source is given as a vertical single force
c   typ=3  source is given by force unit vector
c
c srcsig selects input for source time signal
c   srcsig=1  source signal is compiled source function
c   srcsig=2  source signal is delta pulse
c   srcsig=3  source signal will be read from file
c
      subroutine refmet_rsource(sourcefile, sourcetext, outunits,
     &  typ, outsig, srcsig, The, Thd, ZQ, M0, Mxx, Myy, Mzz, Mxy, Mxz, Myz,
     &  cl_vlevel, lev2, cl_debug)
c 
      integer typ, outsig, srcsig
      character sourcefile*(*), sourcetext*(*), outunits*(*)
      real*8 The, Thd, ZQ, M0, Mxx, Myy, Mzz, Mxy, Mxz, Myz
      integer cl_vlevel, lev2
      logical cl_debug
c 
      integer srclu, sourcelen
      parameter(srclu=11)
c 
      sourcelen=index(sourcefile, ' ')-1
      if (cl_vlevel.gt.lev2) print 52,'opening ',sourcefile(1:sourcelen)
      open(srclu, file=sourcefile(1:sourcelen), status='old', err=99)
      read(srclu, '(a72)', err=98, end=97) sourcetext
      read(srclu, '(/)', err=98, end=97)
      read(srclu, *, err=98, end=97) typ,outsig,srcsig,The,Thd,ZQ
      read(srclu, '(/)', err=98, end=97)
c read different types
      if (typ.eq.1) then
         read(srclu, *, err=98, end=97) M0,Mxx,Myy,Mzz,Mxy,Mxz,Myz
         if (cl_debug) print *,'DEBUG: tensor: ',M0,Mxx,Myy,Mzz,Mxy,Mxz,Myz
      elseif (typ.eq.2) then
         read(srclu, *, err=98, end=97) M0
      elseif (typ.eq.3) then
         stop 'ERROR: no single force available yet'
      else
         print *,'ERROR: you did select typ=',typ
         stop 'ERROR: unknown source model type'
      endif
c get seismogram units
      read(srclu, '(/)', err=98, end=97)
      read(srclu, '(a)', err=98, end=97) outunits
c close file
      if (cl_vlevel.gt.lev2) print 52,'closing ',sourcefile(1:sourcelen)
      close(srclu, err=96)

c check other selections
      if ((typ.eq.2).and.(The.ne.0.d0)) then
        The=0.d0
        print *,'NOTICE: source onset is set to zero when using delta function'
      endif
      if ((srcsig.lt.1).or.(srcsig.gt.3)) then
        print *,'ERROR: you did select srcsig=',srcsig
        stop 'ERROR: unknown source signal selector'
      endif
      if ((Thd.eq.0.).and.(srcsig.ne.2))
     &  stop 'ERROR: source duration set to zero!'
      if ((outsig.lt.1).or.(outsig.gt.3)) then
        print *,'ERROR: you did select outsig=',outsig
        stop 'ERROR: unknown output signal selector'
      endif
      if (srcsig.eq.3) stop 'ERROR: no source signal reading available yet'
      return
   52 format(/a,1x,a)
   99 stop 'ERROR opening source file'
   98 stop 'ERROR reading source file'
   97 stop 'ERROR reading source file - unexpected end of file'
   96 stop 'ERROR closing source file'
      end

c----------------------------------------------------------------------
c
c give some information on source-file structure
c
      subroutine refmet_sourinf(hdfktstr)
      character hdfktstr*(*)
c 
      print 50,' '
      print 50,'How to build a source configuration file'
      print 50,'========================================'
      print 50,' '
      print 50,'compiled source function is:'
      print 50,hdfktstr
      print 50,' '
      print 51,'line','contents'

      print 52,1,'text'
      print 53,'text','a72','any comment on source configuration'

      print 52,4,'typ, sig, src, Ts, Td, Zs'
      print 53,'typ','i','=1: source given by moment tensor'
      print 55,'=2: source given as single vertical force'
      print 55,'=3: source given as single force (not supported)'
      print 54,'sig','i','=1: source signal wil be once integrated'
      print 55,'=2: source signal will be taken as is'
      print 55,'=3: source signal will be once differentiated'
      print 54,'src','i','=1: take compiled source function'
      print 55,'=2: use delta pulse as source'
      print 55,'=3: read time series from file (not supported)'
      print 54,'Ts','f','time of source onset in seconds'
      print 55,'is meaningless when using delta pulse'
      print 54,'Td','f','source signal duration in seconds'
      print 55,'is meaningless when using delta pulse'
      print 55,'The source-function parameters T1 and T2 are:'
      print 55,'T1=Ts   and   T2=Ts+Td'
      print 54,'Zs','f','depth of source in km'

      print 50,' '
      print 50,'moment tensor source:'
      print 52,7,'M0, Mxx, Myy, Mzz, Mxy, Mxz, Myz'
      print 53,'M0','f','scalar momentum in Nm'
      print 54,'Mxx,...','f','cartesian components of moment tensor'
      print 50,' '
      print 50,'single vertical force:'
      print 52,7,'F0'
      print 53,'F0','f','scalar force in N'

      print 52,10,'units'
      print 53,'units','a8','string will be written to output file'

      print 50,' '
      print 50,'Comments on the Mueller-Bruestle-function'
      print 50,'-----------------------------------------'
      print 50,' '
      print 50,'  The Mueller-Bruestle-function is defined as:'
      print 50,' '
      print 50,'           0                                 if t <= T1'
      print 50,'    f(t) = 0.5-0.75*cos(pi*(t-T1)/(T2-T1))+'
      print 50,'              0.25*cos(pi*(t-T1)/(T2-T1))**3 if T1 < t < T2'
      print 50,'           1                                 if t >= T2'
      print 50,' '
      print 50,'  It is f(T1)=0 and f(T2)=1 and the function varies'
      print 50,'  smoothly from 0 to 1 in the time interval [T1,T2].'
      print 50,'  It has vanishing first and second order derivatives'
      print 50,'  at t=T1 and t=T2. It is therefore ideal to describe'
      print 50,'  a (stress-drop) moment-function with a function that'
      print 50,'  holds the nyquist condition of limited bandwidth.'
      print 50,' '
      print 50,'  Notice: The program contains only the first derivative'
      print 50,'  f''(t) in explicit from. The form f(t) is derived by'
      print 50,'  numerical integration in the frequency domain. It is'
      print 50,'  non-unique due to an arbitrary integration constant.'
      print 50,'  In the program the frequency 0Hz (DC) will be missing,'
      print 50,'  resulting in f(t)-0.5 after integration.'
      print 50,' '
      print 50,'  Its first order derivative is'
      print 50,' '
      print 50,'    f''(t)= 0.75*pi/(T2-T1)*dsin(pi*(tvar-T1)/(T2-T1))**3'
      print 50,' '
      print 50,'  in the time interval [T1,T2] and else f''(t)=0. This'
      print 50,'  is a one-sided positive impulse. It has its maximum'
      print 50,'  at t=T1+0.5*(T2-T1). The value at the maximum is'
      print 50,'  f''max= 0.75*pi/(T2-T1).'
      print 50,' '
      print 50,'  Its second order derivative is'
      print 50,' '
      print 50,'    f''''(t)=9*pi**2/(4*(T2-T1)**2)*'
      print 50,'               sin(pi*(tvar-T1)/(T2-T1))**2*'
      print 50,'               cos(pi*(tvar-T1)/(T2-T1))'
      print 50,' '
      print 50,'  in the time interval [T1,T2] and else f''(t)=0. This'
      print 50,'  is a two-sided pulse of vanishing mean value. It is'
      print 50,'  f''''=0 at t=T1+0.5*(T2-T1). It has a maximum at'
      print 50,'  tmax=arctan(sqrt(2))*(T2-T1)/pi+T1 and a minimum at'
      print 50,'  tmin=T2-arctan(sqrt(2))*(T2-T1)/pi. The extreme'
      print 50,'  values are f''''(tmax)=8.550/(T2-T1)**2 and'
      print 50,'  f''''(tmin)=-8.550/(T2-T1)**2 and'
      print 50,' '
      print 50,'  If the duration T2-T1 of the signal is dT, then'
      print 50,'  the amplitude spectrum of the signal has zeroes at'
      print 50,'    f1=(1.5*n)/dT'
      print 50,'  with n being a positive integer.'
      print 50,' '
      print 50,'  Half-width of the amplitude spectrum of the first'
      print 50,'  derivative is approximately at f=1.16/dT.'

      print 50,' '
      print 50,'General comments on the application of the function'
      print 50,'---------------------------------------------------'
      print 50,' '
      print 50,'  The source time function is given in the form of its'
      print 50,'  first order derivative. If it is taken as it is'
      print 50,'  (sig=2) the seismograms will be velocity waveforms'
      print 50,'  as response to a unit moment step. If the scalar'
      print 50,'  moment is given in N*m the seismograms are in m/s.'
      print 50,'  To obtain displacement waveforms in m you have to'
      print 50,'  select source function integration (sig=1), which will be'
      print 50,'  performed in the Fourier domain. If you want to obtain'
      print 50,'  acceleration waveforms in m/s**2 you have to select'
      print 50,'  source function differentiation (sig=3), which also will'
      print 50,'  be done in the Fourier domain. Notice that the scalar'
      print 50,'  momentum M0 always refers to the step-function f(t).'
      print 50,' '
      print 50,'  If you want to receive displacement waveforms as a'
      print 50,'  response to a one-sided force-impulse you may use'
      print 50,'  f''(t) (sig=2) as source function. However, then you'
      print 50,'  have to scale the force value with (T2-T1)/(0.75*pi)'
      print 50,'  to receive the correct displacement in meters according'
      print 50,'  to the specified maximum force value. In other words:'
      print 50,'  If your force-pulse should have an amplitude of Fp Newton'
      print 50,'  you have to specify F0=Fp*(T2-T1)/(0.75*pi). '
      print 50,' '
      print 50,'  NOTICE: Like with for seismic moment source, the scalar'
      print 50,'  source amplitude (here F0) is given with respect to'
      print 50,'  f(t). If you select sig=2 and typ=2, you will obtain'
      print 50,'  velocity seismograms for force step with amplitude F0'
      print 50,'  given in Newton.'
      print 50,'  On the other hand, if you select sig=2 and typ=2, you'
      print 50,'  obtain displacement seismograms for an impulse force'
      print 50,'  of shape f''(t), for which you specify the impulse'
      print 50,'  transfer in Ns (Newton*Seconds) by the scalar'
      print 50,'  amplitude factor F0.'
      print 50,' '
      print 50,'  In the same way you may calculate the displacement'
      print 50,'  response to the two-sided pulse f''''(t) of maximum force'
      print 50,'  Fp when specifying sig=3 and F0=Fp*(T2-T1)**2/8.550.'
c 
      return
   50 format(1x,a)
   51 format(1x,a4,1x,a)
   52 format(/1x,i4,1x,a)
   53 format(/6x,a8,1x,1h(,a3,1h),1x,a)
   54 format(6x,a8,1x,1h(,a3,1h),1x,a)
   55 format(21x,a)
      end
c
c ----- END OF sub/refmet_rsource.f ----- 
