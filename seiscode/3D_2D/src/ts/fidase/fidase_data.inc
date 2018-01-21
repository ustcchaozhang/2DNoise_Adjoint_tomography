c this is <fidase_data.inc>
c------------------------------------------------------------------------------
c $Id$
c
c 09/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c data common block for fidase
c
c REVISIONS and CHANGES
c    09/07/98   V1.0   Thomas Forbriger
c    23/10/01   V1.1   hold stack counter for each trace
c
c==============================================================================
c
      integer nsamples(maxtraces), firstsample(maxtraces)
      integer fileindex(maxtraces)
      integer chain(maxtraces)
      integer revchain(maxtraces)
      integer firstinchain
      integer firstinrevchain
      character*80 filename(maxfiles)
      character*20 source(maxfiles)
      character*5 station(maxtraces)
      character*3 channel(maxtraces)
      character*4 auxid(maxtraces)
      character*6 instype(maxtraces)
      integer nstack(maxtraces)
      real toffset(maxtraces), dt(maxtraces)
      real roffset(maxtraces)
      real maxval(maxtraces), average(maxtraces), minval(maxtraces)
c
c as we do not know how many samples the longest trace will have, we
c must expect to need maxsamples of time values (tos)
      real data(maxsamples), timeofsample(maxsamples)
      integer nfiles, ntraces
c 
      common /fidase_data/ nsamples, firstsample, filename, source,
     &                      station, channel, auxid, instype,
     &                      toffset, roffset, dt, fileindex,
     &                      maxval, average, minval, data, timeofsample,
     &                      nfiles, ntraces, firstinchain, chain, revchain,
     &                      firstinrevchain, nstack
c
c ----- END OF fidase_data.inc -----
