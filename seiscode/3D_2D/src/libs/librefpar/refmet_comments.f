c this is <sub/refmet_comments.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c some comments supplementing usage information in online documentation
c
c ============================================================================
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
c
      subroutine refmet_comments()
      print *,'For complete information on the structure of the four'
      print *,'input files look at the source code of refmet.f or at'
      print *,'the provided example files.'
      print *,' '
      print *,'* A model with earth radius negative will be interpreted'
      print *,'  as a flat model (no amplitude correcting will be applied).'
      print *,'* The model must contain a top halfspace and a bottom.'
      print *,'  halfspace.'
      print *,' '
      print *,'The physical units are:'
      print *,' '
      print *,'     layer velocity:  km/s'
      print *,'            density:  g/cm^3'
      print *,'   depth & distance:  km'
      print *,'              angle:  degrees'
      print *,'               time:  seconds'
      print *,'          frequency:  Hz'
      print *,'           slowness:  s/km'
      print *,'       displacement:  m'
      print *,'  particle velocity:  m/s'
      print *,'       acceleration:  m/s^2'
      print *,'              force:  N'
      print *,'             moment:  N*m'
      print *,' '
      print *,'Here are some essential comments on the source function,'
      print *,'the source units and the corresponding seismogram units:'
      print *,' '
      print *,'  REFMET expects a source function in the form A*s(t)'
      print *,'  with s(t) defining the time dependence of the source'
      print *,'  and A being the amplitude. A is equal to M0 [N*m] for a'
      print *,'  moment tensor source and equal to F0 [N] for a single'
      print *,'  force source. '
      print *,'    The function hfkt in the source code (or the read in'
      print *,'  time series) is expected to be s''(t) which is the'
      print *,'  first time derivative of s(t). In this case the'
      print *,'  seismograms (without any further source integration or'
      print *,'  derivation) will be particle velocity in meters per'
      print *,'  second. Integration will lead to displacement in meters.'
      print *,'  Derivation will lead to accelaration in meters per'
      print *,'  squaresecond.'
      print *,'    If you give s(t) as the time dependence hfkt the'
      print *,'  seismogram units will be m*s (once integrated),'
      print *,'  m (unchanged source) or m/s (once derived).'
      print *,'    The terms "derivation" and "integration" apply to'
      print *,'  your selection in the source configuration file.'
      print *,' '
      print *,'Information on coordinate system:'
      print *,'  The positive z-axis points downwards into the halfspace.'
      print *,'  The r-axis points away from the epicenter. The transverse'
      print *,'  component points to the right when looking in positive'
      print *,'  r-direction. Azimuthal angles are counted clockwise'
      print *,'  positive, when looking in positive z-direction. The'
      print *,'  r-direction falls onto the x-direction for azimuthal'
      print *,'  angle zero and falls onto the y-direction for azimuthal'
      print *,'  angle 90degrees.'
      print *,' '
      print *,'  All vector components point into the direction of the'
      print *,'  corresponding coordinate axis:'
      print *,'    Positive forces point downward into the halfspace.'
      print *,'    Z-component seismograms are positive for downward'
      print *,'    movement. R-component seismograms are positive for'
      print *,'    outward movement. Transverse component seismograms'
      print *,'    are positive for rightward movements when looking'
      print *,'    outwards.'
      print *,' '
      print *,'Information on velocity dispersion:'
      print *,'  This program version is capable of velocity dispersion.'
      print *,'  This means that the complex layer velocities depend on'
      print *,'  frequency relative to a model given reference frequency.'
      print *,'  As one of the main advantages of the reflectivity method is'
      print *,'  that the interface coefficients are frequency independent'
      print *,'  and have to be calculated only once for each slowness.'
      print *,'  You will loose this advantage when using velocity'
      print *,'  dispersion.'
      print *,'  Whether dispersion is calculated or not depends on your'
      print *,'  earth model. If there is a positive reference frequency set'
      print *,'  velocity dispersion will be used.'
      return
      end
c
c ----- END OF sub/refmet_comments.f ----- 
