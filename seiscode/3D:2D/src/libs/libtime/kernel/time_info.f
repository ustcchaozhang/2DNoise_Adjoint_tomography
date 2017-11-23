c this is <time_info.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c print description of time format
c
c ----
c libtime is free software; you can redistribute it and/or modify
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
c    29/06/2007   V1.0   Thomas Forbriger
c
c ============================================================================
cS
      subroutine timeinfo
c
c print description of time format
cE
      print *,'Times and dates can be specified as follows:'
      print *,'1. Absolute time'
      print *,'   defines a specific date, like '
     &       ,'2005/7/15_12:30 specifies the'
      print *,'   15th of July in 2005 at half past twelve. '
     &       ,'The general format'
      print *,'   for absolute time is: '
     &       ,'yyyy/mm/dd/HH/MM/SS.SSSSSS'
      print *,'2. Relative time'
      print *,'   specifies a time range like 0/8/15, which '
     &       ,'means 8 hours and'
      print *,'   fifteen minutes. The general format for '
     &       ,'relative time is'
      print *,'   dd/HH/MM/SS.SSSSSS'
      print *,'In the format strings given above, `/` serves '
     &       ,'as a field separator.'
      print *,'This character may be replaced by any '
     &       ,'non-digit character. Leading'
      print *,'zeroes may be omitted in all fields, where '
     &       ,'yyyy means year, mm'
      print *,'means month, dd means day, HH means hour, mm '
     &       ,'means minute and'
      print *,'SS.SSSSSS means seconds. Seconds are '
     &       ,'specified as a floating point'
      print *,'value with precision down to microseconds. '
     &       ,'Two-digit year values'
      print *,'will be interpreted as years in the 20th '
     &       ,'century (for values larger'
      print *,'than 69) or in the 21st century (for values '
     &       ,'smaller than 70).'
      print *,'Fields from HH on may be omitted.'
c
      return
      end
c
c ----- END OF time_info.f ----- 
