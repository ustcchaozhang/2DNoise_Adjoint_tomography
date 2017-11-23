c this is <refract_strings.inc>
c------------------------------------------------------------------------------
c
c 04/07/98 by Thomas Forbriger (IfG Stuttgart)
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
c    04/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      character*20 string_pmamp, string_pmclip, string_pmexpo
      parameter(string_pmamp='amplitude: <+,->')
      parameter(string_pmclip='clipping: <+,->')
      parameter(string_pmexpo='exponent: <+,->')
c 
      character*30 string_picktt, string_pickt1, string_pickt2,
     &             string_pickt3, string_pickt4, string_pickta
      parameter(string_picktt='pick traveltimes')
      parameter(string_pickt1='pick taper 1')
      parameter(string_pickt2='pick taper 2')
      parameter(string_pickt3='pick taper 3')
      parameter(string_pickt4='pick taper 4')
      parameter(string_pickta='pick arrivals')
c
c ----- END OF refract_strings.inc -----
