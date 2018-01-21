c this is <par_showindex.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c    13/01/06   V1.1   added a lot of information which might not be intended
c                      to be output here. But I found useful to be provided
c                      to the user. And it was easiest to put it here, since
c                      it explains the meaning of the provided index values.
c
c==============================================================================
c
cS
c----------------------------------------------------------------------
c 
      subroutine par_showindex
c 
c show index values set in glq_dim
c
      include 'glq_dim.inc'
c 
cE 
      print *,'array index values'
      print *,'=================='
      print *,' '
      print *,'Within the program the model values are stored in'
      print *,'multidimensional arrays.'
      print *,' '
      print *,'Polynomial models:'
      print *,'This is the model type read from file. The values'
      print *,'are stored in a three-dimensional arrays. The indices'
      print *,'specify: section, type of model parameter (velocity,'
      print *,'density, etc.), polynomial order. The index values'
      print *,'for the type of model parameters are given here:'
      print *,' '
      print *,'named model parameters:'
      print *,'  Vp:                   ',mi_alpha
      print *,'  Vs:                   ',mi_beta
      print *,'  density:              ',mi_density
      print *,'  Qp                    ',mi_Qalpha
      print *,'  Qs                    ',mi_Qbeta
      print *,'  depth:                ',mi_depth
      print *,' '
      print *,'For polynomial models the depth is stored in a'
      print *,'separate array.'
      print *,' '
      print *,'Prior to each calculation of synthetic data, the'
      print *,'polynomial model is converted to a representation of'
      print *,'discrete layers. The resulting values are stored'
      print *,'in a two-dimensional array, where on index specifies'
      print *,'the layer and the other specifies the named model'
      print *,'parameter.'
      print *,' '
      print *,'The program holds polynomial models in two places.'
      print *,'It hold a reference model, which is initialized to the'
      print *,'model read from file and which will be updated after'
      print *,'every iteration of the inversion procedure. And it'
      print *,'holds a working copy, which may be used to calculate'
      print *,'partial derivatives by finite differences. Both'
      print *,'copies are held in one multi-dimensional array, where'
      print *,'one index selects the copy:'
      print *,'  reference model:      ',mb_ref
      print *,'  work model:           ',mb_work
      print *,' '
      print *,'The work model is derived from the reference model'
      print *,'in subroutine mod_parcor. This subroutine adds model'
      print *,'perturbations to the reference model and stores the'
      print *,'result in the reference model. The model perturbations'
      print *,'are the free model parameters of the inversion'
      print *,'procedure.'
      print *,' '
      print *,'The inversion procedure describes the model by a'
      print *,'simple vector (the model vector). It is stored in'
      print *,'a one-dimensional linear array. Which element of'
      print *,'the vector relates to which parameters of the'
      print *,'subsurface model is not apparent in first place.'
      print *,'The relation is only defined through subroutine'
      print *,'mod_parcor. For this reason, we call the model'
      print *,'parameter vector ''anonymous model parameters''.'
      print *,'There may be fewer anonymous model parameters than'
      print *,'physical parameters. This is defined by the search'
      print *,'range table through function spc from the main menu.'
      print *,' '
      print *,'Be careful: In case you defined a set of model'
      print *,'parameters to be active in the inversion and to be'
      print *,'thus contained in the set of anonymous model'
      print *,'parameters, you must not read a new model file'
      print *,'(calling med from them main menu e.g.) without'
      print *,'respecifying the search ranges in case you changed'
      print *,'the number of sections or added or removed follow'
      print *,'flags. Otherwise your anonymous model parameters'
      print *,'will not be synchronous to your subsurface model'
      print *,'anymore and the program will take the emergency exit.'
      print *,' '
      print *,'Data is held in four places within one multi-dimensional'
      print *,'array. The four different data types are selected by'
      print *,'one index of the array:'
      print *,' '
      print *,'data space:'
      print *,'  read data:            ',di_read
      print *,'  modified data:        ',di_mread
      print *,'  synthetics:           ',di_mcalc
      print *,'  reference synthetics: ',di_mref
c 
      return
      end
c
c ----- END OF par_showindex.f -----
