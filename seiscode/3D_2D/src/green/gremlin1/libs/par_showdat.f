c this is <par_showdat.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c display parameters concerning data
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
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/98   V1.1   introduced green amplitude error
c    20/08/98   V1.2   new meaning of logstretch
c    07/04/00   V1.3   introduced new data modification options
c    09/04/00   V1.4   now use conord1 and conord2
c    04/07/02   V1.5   mseisfk offset correction
c
c==============================================================================
cS
c 
      subroutine par_showdat
c 
c display all data related parameters
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_data.inc'
      include 'glq_mseiscorr.inc'
c 
cE
      print 50, val_fmin, val_fmax, val_smin, val_smax, val_xmax
      print 51, balance, tterror, gerror, datamode, 
     &          polyord, conthresh, conord1, conord2, logstretch
      if (prefit_mode.eq.0) then
        print 52,0,'COMPLEX'
      elseif (prefit_mode.eq.1) then
        print 52,1,'REAL'
      elseif (prefit_mode.eq.2) then
        print 52,2,'MEAN REAL'
      elseif (prefit_mode.eq.3) then
        print 52,3,'MAXIMUM REAL'
      else
        call mod_panic('ERROR (par_showdat): unknown prefit mode')
      endif
      if (dottprefit.gt.0) then
        print 53,dottprefit, travx(dottprefit), data_ntts
      else
        print 53,dottprefit, 0., data_ntts
      endif
      if (msc_apply) then
        print 54,'REMOVE'
      else
        print 54,'leave unchanged'
      endif
c 
      return
   50 format(/'data parameters',/,
     &  '  frequency:',t30,'[fmin]: ',f10.3,' Hz     [fmax]: ',f10.3,' Hz',/
     &  '  slowness:',t30,'[smin]: ',f10.3,' s/km   [smax]: ',f10.3,' s/km',/
     &  '  offset:',t30,'[xmax]: ',f10.3,' km')
   51 format('  balance:',t30,'[bala]: ',f10.7,' ([tt]0. <= bala <= 1.[green])'/
     &       '  travel time error:',t30,'[tter]: ',f10.3,' s',/
     &       '  green amplitude error:',t30,'[gerr]: ',f10.3,/
     &       '  green mode:',t30,'[gmod]: ',i10,
     &                    ' (1,2,3,4 | 4 [+1] [+2] [+4] [+8] [+16])'/
     &       '  polynomial order:',t30,'[poly]: ',i10/
     &       '  contrast thershold:',t30,'[cont]: ',g10.2/
     &       '  contrast order (suppress):',t30,'[cons]: ',g10.2/
     &       '  contrast order (increase):',t30,'[cono]: ',g10.2/
     &       '  log dynamic range:',t30,'[logs]: ',g10.2)
   52 format('  prefit_mode:',t30,'[cpre]: ',i10,/
     &       '    (means: ',a,' amplitude prefit)')
   53 format('  travel time prefit over:',t30,'[tpre]: ',i3, ' samples to ',
     &       f10.6,'km (max=',i3,')')
   54 format('  mseisfk offset:',t30,'[msco]: ',a)
      end
c
c ----- END OF par_showdat.f -----
