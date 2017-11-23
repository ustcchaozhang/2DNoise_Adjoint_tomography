c this is <refract_data.inc>
c------------------------------------------------------------------------------
c
c 17/03/98 by Thomas Forbriger (IfG Stuttgart)
c
c data common block for refract
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
c    17/03/98   V1.0   Thomas Forbriger
c    19/06/03   V1.1   remember trace number in file
c    14/11/11   V1.2   store minimum and maximum offset too
c    12/11/12   V1.3   store offset shift
c    20/11/12   V1.4   provide file specific variable area plot flag
c                      store field offset explicitely
c                      support plotting of a baseline
c    26/04/2013 V1.5   increased number of characters in file name
c    24/10/2013 V1.6   added comment on offset arrays
c
c==============================================================================
c
      integer nsamples(maxtraces), firstsample(maxtraces)
      integer fileindex(maxtraces)
      integer chain(maxtraces)
      integer revchain(maxtraces)
      integer traceinfile(maxtraces)
      integer firstinchain
      integer firstinrevchain
      character*120 filename(maxfiles)
      character*20 source(maxfiles)
      character*5 station(maxtraces)
      character*3 channel(maxtraces)
      character*4 auxid(maxtraces)
      character*6 instype(maxtraces)
      integer usevarplot(maxtraces)
      logical plotbaseline(maxtraces)
      real toffset(maxtraces), dt(maxtraces)
c
c roffset: to locate trace in plot panel
c fieldoffset: offset of receiver in field profile
c  
c both values will be equal, except if trace is shifted intentionally
c fieldoffset: source to receiver offset as defined in data file
c              this is used for offset dependent scaling as well as
c              for travel time reduction
c roffset:     source to receiver offset to be used for ordinate axis
      real roffset(maxtraces), fieldoffset(maxtraces)
c
      real maxval(maxtraces), average(maxtraces), minval(maxtraces)
      real maxoffset, minoffset
c
c as we do not know how many samples the longest trace will have, we
c must expect to need maxsamples of time values (tos)
      real data(maxsamples), timeofsample(maxsamples)
      integer nfiles, ntraces
c 
      common /refract_data/ nsamples, firstsample, filename, source,
     &                      station, channel, auxid, instype,
     &                      toffset, roffset, fieldoffset, plotbaseline,
     &                      dt, fileindex, traceinfile, usevarplot,
     &                      maxval, average, minval, data, timeofsample,
     &                      nfiles, ntraces, firstinchain, chain, revchain,
     &                      firstinrevchain, maxoffset, minoffset
c
c ----- END OF refract_data.inc -----
