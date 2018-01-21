c this is <sub/refmet_intro.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c copright statement in usage information
c
c ============================================================================
c
c this file contains some text lines for all usage infos for
c refmet, refmat and resus
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
c   30/09/2013    added reference to original paper by Fuchs and Mueller
c
c----------------------------------------------------------------------
      subroutine refmet_intro()
      print *,' '
      print *,' Reflectivity Method'
      print *,' '
      print *,' 1990 & 1997 University of Stuttgart, Institute of Geophysics'
      print *,' '
      print *,' This program calculates synthetic seismograms using the '
      print *,' reflectivity method. The solution contains the full '
      print *,' wavefield including far- and nearfield for a moment '
      print *,' tensor point source. The original code is due to '
      print *,' J. Ungerer (refseis.f 1990). It was changed and '
      print *,' supplemented by T. Forbriger (refmet.f, refmat.f,',
     &               ' resus.f 1997).'
      print *,' '
      print *,' References:'
      print *,' '
      print *,' Ungerer, J., 1990.'
      print *,'   Berechnung von Nahfeldseismogrammen mit der '
      print *,'   Reflektivit"atsmethode, Diplomarbeit, '
      print *,'   Institut f"ur Geophysik der Universit"at Stuttgart.'
      print *,' '
      print *,' Mueller, G., 1985.'
      print *,'   The reflectivity method: a tutorial, '
      print *,'   J. Geophys., 58, 153--174.'
      print *,' '
      print *,' Fuchs, K. and Mueller, G., 1971.'
      print *,'   Computation of Synthetic Seismograms with the Reflectivity Method and'
      print *,'   Comparison with Observations. Geophys. J. R. astr. Soc., 23(4), 417-433.'
      print *,' '
      print *,'-----------------------------------------------------------'
      return
      end
c
c ----- END OF sub/refmet_intro.f ----- 
