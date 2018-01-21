c this is <refract_para.inc>
c------------------------------------------------------------------------------
c
c 20/01/98 by Thomas Forbriger (IfG Stuttgart)
c
c some parameters for refract.f
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
c    20/01/98   V1.0   Thomas Forbriger
c    11/01/2001 V1.1   had to introduce plfplag_hypoffset
c    17/06/2003 V1.2   introduced plflag_tracenum
c    09/09/2004 V1.3   introduced plflag_tracename
c    14/11/2011 V1.4   remember whether minoff is forced on command line
c    12/11/2012 V1.5   optionally do not time shift offset shifted traces
c    12/11/2012 V1.6   added frame parameter
c    24/10/2013 V1.7   added parameters for scaling mode -S3
c    07/12/2015 V1.8   refering amplitude scaling to average in offest range
c                      becomes the standard (without alternative)
c
c==============================================================================
c
c plot scaling parameters
c -----------------------
c
c clipping in meters
      real plpar_clip
c remove average from traces?
      logical plpar_remav
c offset dependend scaling
      real plpar_expo
c scaling mode
c   1: scale traces individually
c   2: scale all traces to first trace as reference
c   3: scale all traces to first trace of input dataset as reference
      integer plpar_mode
c amplitude scaling for reference trace (meter)
      real plpar_amp
c minimum offset step to be taken as two different positions
      real plpar_minoff
c true if plpar_minoff is forced by command line
      logical plpar_forceminoff
c
c offset range to calculate reference amplitude for amplitude scaling
      real plpar_avgrefxmin, plpar_avgrefxmax
c 
c control modes
c -------------
c
c scale mode (which parameter is affected by <+> and <->)
c 1: amplitude
c 2: clipping
c 3: exponent
      integer plpar_pmmode
c 
c pick modes
c 1: travel times
c 2: taper 1
c 3: taper 1
c 4: taper 1
c 5: taper 1
      integer plpar_pickmode
c
c look and feel parameters
c ------------------------
c
c use colors
      logical plflag_color, plflag_picol
c color cycle to use
      integer plpar_colcyc
c use different line styles
      logical plflag_linestyle
c style cycle to use
      integer plpar_lscyc 
c plot grid
      logical plflag_grid
c 
c use different styles for seismograms
      logical plflag_seistyle
c use different styles for travel times
      logical plflag_ttstyle
c
c use variable area
      logical plflag_vara
c invert trace sign
      logical plflag_invers
c plot these nice bubbles
      logical plflag_bubbles
c 
c use offset scales according to earth radius 
      real plpar_radius
c use spatial offset rather than radial offset
c (spatial offset: distance to hypocenter)
c (radial offset: distance to epicenter)
      logical plflag_hypoffset
c 
c misc flags
c ----------
c
c verbosity flags
      logical debug, verbose
c 
c traveltime reduction
c --------------------
c
      logical plflag_reduce
c reduction velocity in km/s
      real plpar_vred
c
c do not apply time shift to offset shifted traces
      logical plflag_osnoreduce
c
c switch different modes
c ----------------------
c
c mode flags
      logical flag_pick, flag_replot
c 
c switch elements on/off
c ----------------------
c
      logical elem_modbox, elem_filenames, elem_version
      logical elem_annot, elem_scales, elem_data, elem_syntt
      logical elem_picks
c plot subscales
      logical plflag_subscale
c annotate each trace with its number
      logical plflag_tracenum
c annotate each trace with its station name
      logical plflag_tracename
c frame at viewport bounds
      logical elem_vpframe
c 
c strings to hold
c ---------------
c
c last warning message
      character*100 plstring_lastwarn
c
c common blocks
      common /refract_plpar/ plpar_amp, plpar_expo, plpar_minoff,
     &                       plpar_clip, plpar_mode, plpar_remav,
     &                       plflag_color, plflag_linestyle,
     &                       plpar_colcyc, plpar_lscyc, 
     &                       plflag_vara, plflag_invers, plflag_bubbles,
     &                       plpar_pmmode, plflag_grid, plflag_picol,
     &                       plflag_seistyle, plflag_ttstyle, plpar_radius,
     &                       plflag_hypoffset, plflag_tracenum,
     &                       plflag_tracename, plpar_forceminoff,
     &                       plpar_avgrefxmin, plpar_avgrefxmax
      common /refract_elem/ elem_modbox, elem_filenames, elem_version,
     &                      elem_annot, elem_scales, elem_data,
     &                      elem_syntt, elem_picks, plflag_subscale,
     &                      elem_vpframe
      common /refract_verb/ debug, verbose
      common /refract_flags/ flag_pick, flag_replot
      common /refract_reduce/ plflag_reduce, plpar_vred, plflag_osnoreduce
      common /refract_pick/ plpar_pickmode
      common /refract_holdstring/ plstring_lastwarn
c
c ----- END OF refract_para.inc -----
