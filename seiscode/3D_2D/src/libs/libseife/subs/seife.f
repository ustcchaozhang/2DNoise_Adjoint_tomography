c this is <seife.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of the program seife.f. A current version of seife.f can
c be obtained from http://www.software-for-seismometry.de/
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
c extracted from libseife.f
c
c REVISIONS and CHANGES
c    25/10/2000   V1.0   Thomas Forbriger
c    18/01/2008   V1.1   added function seife_mim
c    09/03/2010   V1.2   added function seife_normalize
c
c==============================================================================
c
c----------------------------------------------------------------------
c this is file <libseife.f>
c
c======================================================================
c 
c this is a library with time-domain filter commands from the
c original seife by E. Wielandt
c
c REVISIONS
c V1.0   11/11/96   first version with help output
c V1.1   25/11/96   changed all parameter reading to list input
c V1.2   05/12/96   added polynominal trend removal
c V1.3   07/01/97   added seife_sqr and seife_abs  
c V1.4   24/06/98   explain lpb and hpb on help page
c V1.5   09/07/98   changed twi to be more tolerant
c V1.6   14/06/02   added command fbl
c V1.7   11/07/05   support debug mode
c V1.8   01/09/05   - support handling of first value (fir, rfi)
c                   - added cob window
c
c======================================================================
c  Wolfgang Friederich changed this to be a library (4/96)
c----------------------------------------------------------------------
c 11/4/96   tf   added filter 'int' 
c----------------------------------------------------------------------
c   .    1    .    2    .    3    .    4    .    5    .    6    .    7..
c     program seife
c
c   original version: e. wielandt, eth zurich, 1984
c
c   time-domain signal processing (filtering, windowing, decimation, etc.)
c
c
c   Each command
c   consists of a three-letter code followed by the necessary
c   number of parameters (such as corner period, damping, window limits).
c   numerical parameters must be separated by blanks.
c   parameters in brackets [] are optional. the following codes
c   are accepted:
c
c
c   lim, n: limit the length of the input series to n samples
c
c   rem  remark - comment, ignored by this program
c
c   int, [tau]:  integrate the signal [with time constant tau]
c   dif, [tau]:  differentiate the signal [with time constant tau]
c   fac, f: multiply each sample by f
c   add, a: add a to each sample
c   skp, n: skip n-1 samples
c   cos, n: cosine-taper over n points at both ends
c   win, n1, n2: time window from sample n1 to n2 (n1 becomes first sample)
c   sin, n1, n2: the same, with a sine taper
c   sis, n1, n2: the same, with a squared-sine taper
c   twi, tmin1, tsec1, tmin2, tsec2: window defined by time after midnight
c   pad, n Nullen ergaenzen bis zur Gesamtlaenge n
c   tap, n1, n2: signal is cosine- tapered between samples n1 and n2
c        (signal is unchanged between samples 1 and n1)
c   nul, n1, n2: nulls the signal between samples n1 and n1 (inclusive).
c   dec, n: decimate to every n'th sample
c   del, t: delay the signal by time t (using linear interpolation)
c   avg, n: remove average (determined from first n samples)
c   tre, n: remove linear trend (determined from first n samples)
c   pol, n: remove polynomial trend of degree n
c   csi, dt: change sampling interval to dt by linear interpolation
c   tid, [n]: remove tides. tides are interpolated over n samples.
c        default (n=0 or blank) is interpolation over 5 minutes.
c   spl, clp,n1,n2,apex:  interpolate clipped samples (value >= clp) with
c        cubic spline from sample n1 to n2. clp=0 is replaced by a default
c        value of slightly less than 2**18. if apex is specified other than
c        zero, interplation is done with a parabola with specified height of
c        apex.
c   clp, clp: clip signal at level clp.
c   sqr: square time series
c   abs: replace each sample by its absolute value
c   rev: reverse signal in time (useful for backward filtering)
c   DBG: switch on debug mode
c
c   recursive filters are specified by type and 1 to 4 parameters.
c        possible types are: lp1, hp1, le1, he1, lp2, hp2, bp2, le2, he2,
c                            lpb, hpb
c        lp = low pass, hp= high pass, bp = band pass
c             parameters: corner period [,damping if second-order]
c        le = inverse low pass, he = inverse high pass (equalizers)
c             parameters: old corner period, [old damping], new corner
c             period, [new damping]
c        1 or 2 = number of poles, i.e. order of denominator polynomial
c        b = Butterworth, specify order in place of damping
c
c
c-------------------------------------------------------------
c  subroutine seife_seife(typ,par,n,dt,tmin,tsec,x,msg)
c  character typ*3, par*(*), msg*(*)
c  integer n
c  real dt, tmin, tsec
c  real*8 x
c
c  All input
c  Seife main routine converted to a subroutine
c-------------------------------------------------------------
	subroutine seife(typ,par,n,dt,tmin,tsec,x,msg)
      include 'seife_common.inc'
      character version*80
	double precision x(n)
	character typ*3, par*(*), msg*(*)
	logical nil
c
      version='LIBSEIFE   V1.8   time domain signal processing'
      nil=.true.
      msg=' '
	if(typ.eq.'lim') then
		read(par,*) npts
		n=min(n,npts)
		nil=.false.
	endif
      if(typ.eq.'fac') call seife_factor(nil,par,x,n,msg)
      if(typ.eq.'nrm') call seife_normalize(nil,par,x,n,msg)
      if(typ.eq.'mim') call seife_mim(nil,par,x,n,msg)
      if(typ.eq.'add') call seife_add(nil,par,x,n,msg)
      if(typ.eq.'dif') call seife_deriv(nil,par,x,n,dt,msg)
      if(typ.eq.'spl') call seife_interp(nil,par,x,n,msg)
      if(typ.eq.'csi') call seife_intpo (nil,par,dt,x,n,msg)
      if(typ.eq.'win'.or.typ.eq.'sin'.or.typ.eq.'skp'.or.typ.eq.'tap'
     &		.or.typ.eq.'sis'.or.typ.eq.'cos'.or.typ.eq.'cob') 
     & 	call seife_window(nil,typ,par,x,n,dt,tmin,tsec,msg)
      if(typ.eq.'twi') call seife_timwin(nil,par,x,n,dt,tmin,tsec,msg)
      if(typ.eq.'nul') call seife_null(nil,par,x,n,msg)
      if(typ.eq.'clp') call seife_clip(nil,par,x,n,msg)
      if(typ.eq.'pol') call seife_polytrend(nil,par,x,n,msg)
      if(typ.eq.'dec') call seife_decim(nil,par,x,n,dt,msg)
      if(typ.eq.'del') call seife_delay(nil,par,x,n,dt,tsec,msg)
      if(typ.eq.'avg') call seife_mean(nil,par,x,n,msg)
      if(typ.eq.'tre') call seife_trend(nil,par,x,n,msg)
      if(typ.eq.'rev') call seife_reverse(nil,par,x,n,msg)
      if(typ.eq.'tid') call seife_tides(nil,par,x,n,dt,msg)
      if(typ.eq.'pad') call seife_pad(nil,par,x,n,msg)
      if(typ.eq.'sqr') call seife_sqr(nil,par,x,n,msg)
      if(typ.eq.'mra') call seife_mra(nil,par,x,n,dt,msg)
      if(typ.eq.'rms') call seife_rms(nil,par,x,n,dt,msg)
      if(typ.eq.'fbl') call seife_baseline(nil,par,x,n,msg)
      if(typ.eq.'abs') call seife_abs(nil,par,x,n,msg)
      if(typ.eq.'fir') call seife_resetfirst(nil,par,x,n,.true.,msg)
      if(typ.eq.'rfi') call seife_resetfirst(nil,par,x,n,.false.,msg)
	if(typ.eq.'lp1'.or.
     1            typ.eq.'int'.or.
     1		typ.eq.'hp1'.or.
     1		typ.eq.'le1'.or.
     1		typ.eq.'he1'.or.
     1		typ.eq.'lp2'.or.
     1		typ.eq.'hp2'.or.
     1		typ.eq.'bp2'.or.
     1		typ.eq.'le2'.or.
     1		typ.eq.'he2'.or.
     1		typ.eq.'lpb'.or.
     1		typ.eq.'hpb') 
     1                call seife_filter(nil,typ,par,x,n,dt,msg)
      if(typ.eq.'hel') call seife_help(nil,version,msg)
      if(typ.eq.'rem') call seife_remark(nil, typ, par, msg)
      if(typ.eq.'DBG') then
        ldebug=.true.
        nil=.false.
      endif
c
c  produce error message if code unrecognized
c
      if(nil) then
        if (msg.eq.' ') write(msg,3) typ
    3   format("ERROR: prozedur ",a3," ist nicht definiert --- return")
        return
      endif
      return
      end
c
c ----- END OF seife.f -----
