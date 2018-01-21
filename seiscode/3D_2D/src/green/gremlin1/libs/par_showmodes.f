c this is <par_showmodes.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c explain different mode settings
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
c    20/08/98   V1.0   Thomas Forbriger
c    07/04/00   V1.1   introduced new data modification modes
c    09/04/00   V1.2   new cons (conord1) and cono (conord2) are used 
c    02/06/00   V1.3   explain vptrackfactor and mweightcondition
c    04/07/02   V1.4   explain mseisfk offset correction
c
c==============================================================================
c
      subroutine par_showmodes
c 
      print *,'mode settings'
      print *,'============='
      print *,' '
      print *,'green data mode'
      print *,'  1: use greens function coefficients as they are'
      print *,'  2: keep phase of coefficients but reduce dynamic range'
      print *,'     of amplitude by log10'
      print *,'  3: use real absolute values of coefficients'
      print *,'  4: use real absolute values of coefficients with dynamic'
      print *,'     range reduced by log10'
      print *,' '
      print *,'if the data mode number is greater than 4, it is assumed to be'
      print *,'a sum of the following flag values plus 4:'
      print *,'  1: take absolute amplitude of complex values'
      print *,'  2: increase contrast by log-function defined through'
      print *,'     ''cont'', ''cons'', and ''cono'' as explained below'
      print *,'  4: remove polynomial trend using legendre polynomials up to'
      print *,'     order ''poly'' (''spa'' submenu) from real part of'
      print *,'     synthetics'
      print *,'  8: remove polynomial trend rather from absolute amplitude'
      print *,' 16: remove polynomial trend from read data values too'
      print *,' '
      print *,'  The dynamic range is reduced using the formula'
      print *,' '
      print *,'    A'' = log10(1.+((10**R-1.)/M*abs(A))/R'
      print *,' '
      print *,'  where A is the original coefficient and A'' is the reduced'
      print *,'  amplitude. M is the maximum amplitude in the input dataset'
      print *,'  and R is the range you specified. The resulting values of'
      print *,'  A'' vary from 0. to B but any value of A is related to a'
      print *,'  value of A''. There is no limiting watertable.'
      print *,' '
      print *,'  R is the spa-parameter called logstretch (logs).'
      print *,' '
      print *,'  The contrast incresing function is defined by a symmetric'
      print *,'  log10-function as given above. The symmetry point is defined'
      print *,'  by the threshold (relative to the maximum amplitude within'
      print *,'  one slowness-trace (fixed frequency). The parameters are'
      print *,'  (in the ''spa'' submenu):'
      print *,'  cont:  gives the threshold value'
      print *,'  cons:  order of the log10-relation for suppressing values'
      print *,'         smaller than the threshold'
      print *,'  cono:  order of the log10-relation for amplifying values'
      print *,'         larger than the threshold'
      print *,' '
      print *,'  Green data errors refer to the modified dataset!'
      print *,' '
      print *,'green data prefit mode'
      print *,'  0: a complex value for the transfer function is found '
      print *,'     using a linear regression'
      print *,'  1: a real value for the transfer function is found using'
      print *,'     a linear regression'
      print *,'  2: the calculated value for the transfer function ensures'
      print *,'     that the synthetic and the real datasets both have the'
      print *,'     same mean amplitude'
      print *,'  3: make maximum amplitude per slowness-trace (fixed'
      print *,'     frequency) equal to data.'
      print *,' '
      print *,'  If the modes in the synthetic dataset do not match at all,'
      print *,'  the linear regression will find a value of zero which'
      print *,'  obviously minimizes the residual. Use prefit mode 2 in'
      print *,'  this case.'
      print *,' '
      print *,'  The prefit is done AFTER data modification for data modes'
      print *,'  greater than 2 (see above).'
      print *,' '
      print *,'traveltime prefit mode'
      print *,'  If the travel time prefit value is not zero, ist defines'
      print *,'  a number of traveltime samples that will be used to find'
      print *,'  an offset time that may be due to trigger delays or filter'
      print *,'  delays. This offset time will not be explained through the'
      print *,'  model parameters.'
      print *,' '
      print *,'chopping master'
      print *,'  The chopping master index defines the index value of the'
      print *,'  last named model parameter that will be taken into account'
      print *,'  to calculate the layer thickness of the model of homgeneous'
      print *,'  layers. If the chopping master value is 2, only the'
      print *,'  p-velocity and the s-velocity affect the layer thickness.'
      print *,' '
      print *,'vp track factor [vptr]'
      print *,'  If vptrackfactor is positive it will be used to create a vp'
      print *,'  model that is exactly vp=vs*vptrackfactor each time'
      print *,'  mod_parcor is called.'
      print *,' '
      print *,'parameter weight condition [pwco]'
      print *,'  mweightcondition defines the parameters that are to be optimized'
      print *,'  within a resolution analysis. In resolution analysis it might'
      print *,'  be desirable not to optimize defined polynomial orders. If'
      print *,'  mweightmax is the largest mweight value than only a parameter'
      print *,'  with (mweightcondition*mweight)>mweightmax will be optimized'
      print *,'  in the analysis. Here mweight is the ''search range''.'
      print *,' '
      print *,'mseisfk offset correction [msco]:'
      print *,'  The direct wave in the water layer produced a'
      print *,'  significant offset to the hydrophon synthetics'
      print *,'  calculated by ''mseisfk''. This offset is not'
      print *,'  present in the data, because it contributes only'
      print *,'  in the near-field of the source. This options'
      print *,'  allows to remove the offset from the synthetics.'
      print *,' '
      print *,'  Once you select the option, correction values'
      print *,'  are calculated for the given water velocity and'
      print *,'  density. If you ever change the water velocity'
      print *,'  and/or density and/or source depth or amplitude,'
      print *,'  you have to recalculate the'
      print *,'  correction values. Do this by disabling and'
      print *,'  enabling the option in turn.'
      print *,' '
      print *,'  Notice that this option is meaningless for'
      print *,'  models without a top water layer or with'
      print *,'  true near-field data.'
      return
      end
c
c ----- END OF par_showmodes.f -----
