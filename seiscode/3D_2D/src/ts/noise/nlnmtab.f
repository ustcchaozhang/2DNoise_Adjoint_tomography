c this is <nlnmtab.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
c data table and function copied from noisecon.f 
c Copyright (c) 2005 by Erhard Wielandt
c
c original code is provided at http://www.software-for-seismometry.de/
c
c write table of NLNM values
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
c definition of power spectral density (PSD):
c Peterson (1993) displays average peak-to-peak amplitude in 1/3 octave
c in Fig. 18. Average peak amplitude in 1/3 octave essentially equals
c twice the rms amplitude in 1/6 decade. In Fig. 15 he displays power
c spectral density as defined in Tab. 3 and as defined in this program.
c I obtain the values displayed in Fig. 18 when assuming that total
c power is obtained by integration of power spectral density over
c positive frequencies only. The PSD values provided by this program are as
c defined by the enigeering convention not as defined by the
c mathematical definition, i.e. they are twice the Fourier transform of
c the normalized autocorrelation function at the given (positive)
c frequency.
c 
c Conversion factor from rms amplitude in 1/6 decade to average peak
c amplitude in 1/3 octave as defined by Peterson:
c
c a_peak(1/3 octave) = sqrt(pi/2) * sqrt(0.232/0.386) * a_rms(1/6 c decade)
c                    = 0.972 * a_rms(1/6 decade)
c
c Similarly for average peak-to-peak:
c a_peak-to-peak(1/3 octave) = 2.506 * a_rms(1/3 octave)
c                            = 1.943 * a_rms(1/6 decade)
c
c REVISIONS and CHANGES
c    06/06/2005   V1.0   Thomas Forbriger
c    26/06/2013   V1.1   check definition of PSD values
c    04/12/2014   V1.2   fix: declare return type of function fnlnm
c
c ============================================================================
c
      program nlnmtab
c
      character*(*) version
      parameter(version=
     &  'NLNMTAB   V1.2   write table of NLNM values')
      character*(*) NLNMTAB_CVS_ID
      parameter(NLNMTAB_CVS_ID=
     &  '$Id$')
c
c 
      logical useperiod, uselog
      double precision f,p,f1,f2,df,fnlnm
      integer k,n
      character*80 outstring
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=7)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-p, 2h-s, 2h-e, 2h-n, 2h-l/
      data opthasarg/3*.FALSE.,3*.TRUE.,.FALSE./
      data optarg/3*1h-,5h1.e-4,3h10.,3h100,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.0)) then
        print *,version
        print *,'Usage: nlnmtab [-p] [-s s] [-e e] [-n n] [-l]'
        print *,'   or: nlnmtab -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'-p     specify signal period rather than frequency'
        print *,'-s s   start table at frequency/period ''s'' '
        print *,'-e e   end table at frequency/period ''e'' '
        print *,'-n n   subdivide frequency/period range into'
        print *,'       ''n'' intervals of equal size.'
        print *,'-l     use equal intervals on a logarithmic scale'
        print *,' '
        print *,NLNMTAB_CVS_ID
        print *,' '
        print *,'New Low Noise Model (Peterson 1993, USGS Open File Report 93-322)'
        print *,'(parameters are given in Tab. 3 on page 36).'
        print *,'http://earthquake.usgs.gov/regional/asl/pubs/'
        print *,' '
        print *,'Power spectral density is given in dB referred to 1 (m/s**2)**2/Hz.'
        print *,'Total power in a given bandwidth is obtained by integration over'
        print *,'positive values of frequency only (engineering convention).'
        print *,' '
        print *,'Definition of power spectral density (PSD):'
        print *,'Peterson (1993) displays average peak-to-peak amplitude in 1/3 octave'
        print *,'in Fig. 18. Average peak amplitude in 1/3 octave essentially equals'
        print *,'twice the rms amplitude in 1/6 decade. In Fig. 15 he displays power'
        print *,'spectral density as defined in Tab. 3 and as defined in this program.'
        print *,'I obtain the values displayed in Fig. 18 when assuming that total'
        print *,'power is obtained by integration of power spectral density over'
        print *,'positive frequencies only. The PSD values provided by this program are as'
        print *,'defined by the enigeering convention not as defined by the'
        print *,'mathematical definition, i.e. they are twice the Fourier transform of'
        print *,'the normalized autocorrelation function at the given (positive)'
        print *,'frequency.'
        print *,''
        print *,'Conversion factor from rms amplitude in 1/6 decade to average peak'
        print *,'amplitude in 1/3 octave (Peterson, 1993, page 36):'
        print *,''
        print *,'a_peak(1/3 octave) = sqrt(pi/2) * sqrt(0.232/0.386) * a_rms(1/6 c decade)'
        print *,'                   = 0.972 * a_rms(1/6 decade)'
        print *,''
        print *,'Similarly for average peak-to-peak:'
        print *,'a_peak-to-peak(1/3 octave) = 2.506 * a_rms(1/3 octave)'
        print *,'                           = 1.943 * a_rms(1/6 decade)'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
c 
      useperiod=optset(3)
      read(optarg(4), *) f1
      read(optarg(5), *) f2
      read(optarg(6), *) n
      uselog=optset(7)
c
c------------------------------------------------------------------------------
c go
c
      print 50,'# New Low Noise Model ',
     &  '(Peterson 1993, USGS Open File Report 93-322)'
      print 50,'# (parameters are given in Tab. 3 on page 36)'
      if (useperiod) then
        outstring='period (s)'
      else
        outstring='frequency (Hz)'
      endif
      print 50,'# first column: ',outstring(1:index(outstring, ')'))
      print 50,'# second column: power spectral density in dB'
      print 50,'#                referred to 1 (m/s**2)**2/Hz'
c 
      if (uselog) then
        f1=log10(f1)
        f2=log10(f2)
      endif
      df=(f2-f1)/n
      do k=0,n
        f=f1+k*df
        if (uselog) then
          f=10.d0**f
        endif
        if (useperiod) then
          p=f
        else
          p=1./f
        endif
        print 51,f,fnlnm(p)
      enddo
c
      stop
   50 format(a,a)
   51 format(g10.4,2x,f10.3)
      end
c
c----------------------------------------------------------------------
c evaluate NLNM 
c
      double precision function fnlnm(p)
c
      double precision p
c
c data table and function copied from noisecon.f by Erhard Wielandt
c 
c the function returns the power spectral density in dB 
c referred to 1 (m/s**2)**2/Hz
c
      double precision per(21),a(21),b(21)
      integer k
c  
c   New Low Noise Model (Peterson 1993, USGS Open File Report 93-322)
c   (parameters are given in Tab. 3 on page 36)
c  
      data per(1),a(1),b(1)    / 0.1,-162.36,5.64 /
      data per(2),a(2),b(2)    / 0.17,-166.7,0 /
      data per(3),a(3),b(3)    / 0.4,-170,-8.3 /
      data per(4),a(4),b(4)    / 0.8,-166.4,28.9 /
      data per(5),a(5),b(5)    / 1.24,-168.6,52.48 /
      data per(6),a(6),b(6)    / 2.4,-159.98,29.81 /
      data per(7),a(7),b(7)    / 4.3,-141.1,0 /
      data per(8),a(8),b(8)    / 5,-71.36,-99.77 /
      data per(9),a(9),b(9)    / 6,-97.26,-66.49 /
      data per(10),a(10),b(10) / 10,-132.18,-31.57 /
      data per(11),a(11),b(11) / 12,-205.27,36.16 /
      data per(12),a(12),b(12) / 15.6,-37.65,-104.33 /
      data per(13),a(13),b(13) / 21.9,-114.37,-47.1 /
      data per(14),a(14),b(14) / 31.6,-160.58,-16.28 /
      data per(15),a(15),b(15) / 45,-187.5,0 /
      data per(16),a(16),b(16) / 70,-216.47,15.7 /
      data per(17),a(17),b(17) / 101,-185,0 /
      data per(18),a(18),b(18) / 154,-168.34,-7.61 /
      data per(19),a(19),b(19) / 328,-217.43,11.9 /
      data per(20),a(20),b(20) / 600,-258.28,26.6 /
      data per(21),a(21),b(21) / 10000,-346.88,48.75 /
      IF (p.lt.0.1) THEN
c        write(6,*) ' NLNM undefined, OLNM used'
        fnlnm=-168.
      ELSE IF (p.le.100000.) then
        do k=1,20
          IF (p.lt.per(k+1)) goto 20
        enddo
   20   fnlnm=a(k)+b(k)*LOG10(p)
      ELSE
        write(6,*) ' NLNM undefined'
        fnlnm=0.
      ENDIF
      END
c
c
c ----- END OF nlnmtab.f ----- 
