c this is <time_util_warning_report_time.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
c
c report time through warning interface
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
c    25/12/2012   V1.0   Thomas Forbriger
c
c ============================================================================
cS
c
      subroutine time_util_warning_report_time(caller,date)
c
c declare parameters
      character*(*) caller
      integer date(7)
c
cE
c
c------------------------------------------------------------------------------
c go
      call time_util_warning_n(caller, "year",   date(1))
      call time_util_warning_n(caller, "doy",    date(2))
      call time_util_warning_n(caller, "hour",   date(3))
      call time_util_warning_n(caller, "minute", date(4))
      call time_util_warning_n(caller, "second", date(5))
      call time_util_warning_n(caller, "milsec", date(6))
      call time_util_warning_n(caller, "micsec", date(7))
      end
c
c ----- END OF time_util_warning_report_time.f ----- 
