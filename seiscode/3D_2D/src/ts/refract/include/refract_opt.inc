c this is <refract_opt.inc>
c------------------------------------------------------------------------------
c
c Copyright 1998 by Thomas Forbriger (IfG Stuttgart)
c
c options common block for refract
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    17/03/98   V1.0   Thomas Forbriger (thof)
c    24/05/00   V1.1   activated
c    29/07/00   V1.2   introduced opt_Tannotate
c    26/11/10   V1.3   support additional file formats
c    12/11/2012 V1.4   optionally do not apply additional travel time
c                      reduction to offset shifted traces
c    24/10/2013 V1.5   - added ordinate switch
c                      - added parameters to refer scaling mode 3 amplitude to
c                        an offset range
c    18/11/2013 V1.6   added option -TF
c    21/03/2014 thof:  add option -TR
c
c==============================================================================
c
c titles, labels, legends
      character*120 opt_Ttitle
      character*120 opt_Txlabel
      character*120 opt_Tylabel
      character*120 opt_Tannotate
      logical opt_Tmode, opt_Tlegend, opt_Tmodel, opt_Tfilename
      logical opt_Treverselegend
      real opt_Tscale, opt_Tmodx, opt_Tmodt
c
c line and color options
      integer opt_Lwidth, opt_Lmax, opt_Cmax, opt_Lttwidth
      real opt_Cbgrgb(3), opt_Cfgrgb(3)
      logical opt_Lcycle, opt_Ccycle, opt_Cswap
c 
c elements and style
      logical opt_Egrid, opt_Ewiggle, opt_Ebubble
      logical opt_Epicks, opt_Ewave, opt_Ett
      logical opt_ECpicks, opt_ECwave, opt_ECtt
c
c file reading
      character*120 opt_Fpicks
      character*120 opt_Farrival
      character*120 opt_Ftaper
      character*120 opt_Fmodel
      character*120 opt_Fformat
c 
c scaling
      real opt_Sxmin, opt_Sxmax, opt_Stmin, opt_Stmax
      real opt_Sexp, opt_Samp, opt_Sclip, opt_Svel, opt_Sminoff, opt_Sradius
      real opt_Savgrefxmin, opt_Savgrefxmax
      integer opt_Smode, opt_Sordinate
      logical opt_Sinv, opt_Savg, opt_Sxrange, opt_Strange, opt_Sreduce
      logical opt_Sosnoreduce, opt_Savgref
c
c common blocks
      common /refract_optS/ opt_Ttitle, opt_Txlabel, opt_Tylabel,
     &                      opt_Tannotate,
     &                      opt_Fpicks, opt_Farrival, opt_Ftaper, 
     &                      opt_Fmodel, opt_Fformat
      common /refract_optI/ opt_Lwidth, opt_Lmax, opt_Cmax, opt_Smode
      common /refract_optR/ opt_Tscale, opt_Cbgrgb, opt_Cfgrgb,
     &                      opt_Sxmin, opt_Sxmax, opt_Stmin, opt_Stmax,
     &                      opt_Sexp, opt_Samp, opt_Sclip, opt_Svel,
     &                      opt_Sminoff, opt_Sradius, opt_Tmodx, opt_Tmodt,
     &                      opt_Lttwidth, opt_Sordinate, opt_Savgrefxmin,
     &                      opt_Savgrefxmax
      common /refract_optL/ opt_Tmode, opt_Tlegend, opt_Tmodel, opt_Lcycle,
     &                      opt_Ccycle, opt_Cswap, opt_Egrid, opt_Ewiggle,
     &                      opt_Ebubble, opt_Epicks, opt_Ewave, opt_Ett,
     &                      opt_ECpicks, opt_ECwave, opt_ECtt, opt_Sinv,
     &                      opt_Savg, opt_Sxrange, opt_Strange, opt_Sreduce,
     &                      opt_Sosnoreduce, opt_Savgref, opt_Tfilename,
     &                      opt_Treverselegend
c
c ----- END OF refract_opt.inc -----
