c this is <seife_help.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of seife.f. A current version of seife.f can be obtained
c from http://www.software-for-seismometry.de/
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
c    14/06/2002   V1.1   added command fbl
c    11/07/2005   V1.2   introduced debug mode
c    01/09/2005   V1.3   explain cob, fir and rfi
c    09/03/2010   V1.3   added function seife_normalize
c
c==============================================================================
c
c
c======================================================================
      subroutine seife_help(nil,par,msg)
c
c print out help message
c
      logical nil
      character msg*(*),par*(*)
      nil=.false.
      write(msg,'(a)') 'help message'
      print *,'this is: ',par
      print *,''
c----------------------------------------------------------------------
c some comments that were included in the original
  999   format(/'   e. wielandt eth zurich 1984',
     &/' ',
     &/'   time-domain signal processing (filtering, ',
     &'windowing, decimation, etc.)',
     &/' ',
     &/'   the desired time-domain processes are defined in ',
     &'a command file whose',
     &/'   name is entered as a runstring. ',
     &/'   all processes apply to all input files. each line of ',
     &'the command file',
     &/'   consists of a three-letter code followed by two ',
     &'blanks and the necessary')
  996 format(
     &'   number of parameters (such as corner period, ',
     &'damping, window limits).',
     &/'   numerical parameters must be separated by commas ',
     &'and fit into columns',
     &/'   6 to 40. parameters in brackets [] are optional. ',
     &'the following codes',
     &/'   are accepted:',
     &/' ',
     &/'   lim, n: limit the length of the output series ',
     &'to n samples',
     &/' ',
     &/'   rem  remark - ignored by this program')
  995 format( ' ',
     &/'   int, [tau]:  integrate the signal [with time ',
     &'constant tau]',
     &/'   dif, [tau]:  differentiate the signal [with time ',
     &'constant tau]',
     &/'   fac, f: multiply each sample by f',
     &/'   add, a: add a to each sample',
     &/'   con, f, [comment]: same as fac. used for ',
     &'transducer responsivities.',
     &/'   skp, n: skip n-1 samples')
  994 format(
     &'   win, n1, n2: time window from sample n1 to ',
     &'n2 (n1 becomes first sample)',
     &/'   hut, n: cosine-taper over n points at both ends',
     &/'   sin, n1, n2: the same, with a sine taper',
     &/'   sis, n1, n2: the same, with a squared-sine taper',
     &/'   twi, tmin1, tsec1, tmin2, tsec2: window defined ',
     &'by time after midnight',
     &/'   pad, n Nullen ergaenzen bis zur Gesamtlaenge n',
     &/'   tap, n1, n2: signal is cosine- tapered between ',
     &'samples n1 and n2',
     &/'        (signal is unchanged between samples 1 and n1)',
     &/'   ext  determine extremal samples (signal is unchanged)',
     &/'   rms  determine rms amplitude (signal is unchanged)',
     &/'   nul, n1, n2: nulls the signal between samples n1 ',
     & 'and n1 (inclusive).')
  997 format(
     &'   cob, n: apply cos-taper to first n samples',
     &/'   fir, n: remove value of average over n first samples',
     &/'           from whole series',
     &/'   rfi: restore value of first sample to whole series',
     &/'   nrm, f: normalize time series to an absolute maximum of f')
  993 format(
     &'   dec, n: decimate to every n''th sample',
     &/'   del, t: delay the signal by time t (using linear ',
     &'interpolation)',
     &/'   avg, n: remove average (determined from first n samples)',
     &/'   fbl, n,n2: force signal ends to baseline zeor',
     &/'           (determined from first n and n samples to n2)',
     &/'   tre, n: remove linear trend (determined from first ',
     &/'   pol, n: remove polynomial trend of degree n',
     &'n samples)',
     &/'   csi, dt: change sampling interval to dt by ',
     &'linear interpolation',
     &/'   tid, [n]: remove tides. tides are interpolated ',
     &'over n samples.',
     &/'        default (n=0 or blank) is interpolation ',
     &'over 5 minutes.',
     &/'   spl, clp,n1,n2,apex:  interpolate clipped samples ',
     &'(value >= clp) with')
  992 format(
     &'        cubic spline from sample n1 to n2. clp=0 is ',
     &'replaced by a default',
     &/'        value of slightly less than 2**18. if apex is ',
     &'specified other than',
     &/'        zero, interplation is done with a parabola with ',
     &'specified height of',
     &/'        apex.',
     &/'   clp, clp: clip signal at level clp.')
  989 format(
     &'   sqr: square each sample',
     &/'   mra, fac: multiply with ramp t*fac',
     &/'   mim: report minimum and maximum value',
     &/'   abs: replace each sample by its absolute value',
     &/'   rev: reverse signal in time (useful for ',
     &'backward filtering)',
     &/'   DBG: switch on debug mode',
     &/' ',
     &/'   recursive filters are specified by type and 1 ',
     &'to 4 parameters.')
  991 format(
     &'        possible types are: lp1, hp1, le1, he1, lp2, ',
     &'hp2, bp2, le2, he2',
     &/'        lp = low pass, hp= high pass, bp = band pass',
     &/'             parameters: corner period [,damping if ',
     &'second-order]',
     &/'        le = inverse low pass, he = inverse high pass ',
     &'(equalizers)',
     &/'             parameters: old corner period, ',
     &'[old damping], new corner',
     &/'             period, [new damping]',
     &/'        1 or 2 = number of poles, i.e. order of ',
     &'denominator polynomial',
     &/' ',
     &/'        example: to specify a 30 sec, fifth-order ',
     &'butterworth lp, say')
  990 format(
     &'        lp1  30.',
     &/'        lp2  30., 0.309',
     &/'        lp2  30., 0.809',
     &/' ',
     &/'       arbitrary butterworth filters may be specified:',
     &/' ',
     &/'       hpb period,order        butterworth high pass',
     &/'       lpb period,order        butterworth low pass')
  988 format(
     &' ',
     &/'   end  indicates the end of the controlfile',
     &/' ')
        write(6,999)
        write(6,996)
        write(6,995)
        write(6,994)
        write(6,997)
        write(6,993)
        write(6,992)
        write(6,989)
        write(6,991)
        write(6,990)
        write(6,988)
c----------------------------------------------------------------------
      return
      end
c
c ----- END OF seife_help.f -----
