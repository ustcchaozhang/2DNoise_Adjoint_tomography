c this is <stuff.f>
c
c Copyright (c) 1996 by Wolfgang Friederich
c Copyright (c) 1996, 2010 by Thomas Forbriger 
c
c ----
c This file is part of libsff.
c
c libsff is free software; you can redistribute it and/or modify
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
cA
c======================================================================
c
c SFF (Stuttgart File Format) is a definition of a standardized
c file format for seismic time series used at the University of
c Stuttgart. In its kernel it uses GSE2.0 data blocks and provides
c
c several additional information and the ability to store
c several traces in one file.
c
c stuff.f is a package to read and write seismic time series in 
c standardized SFF format (using GSE2.0 format in its kernel).
c
c----------------------------------------------------------------------
c
c REVISIONS and CHANGES to stuff.f (don't forget to update sff_libversion)
c
c   V1.00   27/03/96   W.F.   first release
c   V1.01   21/11/96   T.F.   several changes and additions:
c                             - full format definition
c           22/11/96          - changed Documentation ID-style
c                             - changed parameter names of PrepWid2
c                             - changed format of calib in PrepWid2 to
c                               e10.2 as defined in GSE2.0
c                             - added variables code and last to parameter
c                               list of SkipData
c                             - uses now sff_libversion in RStatus and WStatus
c   V1.02   25/11/96          new routines: sff_f2i, sff_i2f,
c                               sff_WOpen, sff_WOpenF, sff_WOpenS, sff_WOpenFS,
c                               sff_ROpen, sff_ROpenF, sff_ROpenS, sff_ROpenFS,
c                               sff_WTrace, sff_WTraceF, sff_WTraceI, 
c                               sff_WTraceFI, sff_RTrace, sff_RTraceF, 
c                               sff_RTraceI, sff_RTraceFI, sff_MakeTime
c   V1.03   02/12/96   T.F.   - removed sff_MakeTime
c                             - corrected some positions in SRCE definition
c                               and INFO definition
c                             - added several functions for WID2 modification
c                               and time calculation:
c                               sff_ModWid2samprat, sff_ModWid2samps, 
c                               sff_ModWid2date, sff_ModWid2time, 
c                               sff_ModWid2shift, sff_TimeIsLeapYear, 
c                               sff_TimeSetDOY, sff_TimeGetDOY, 
c                               sff_TimeSplit, sff_TimeAdd
c   V1.04   13/12/96   T.F.   - set correct file status on open action
c   V1.05   14/12/96   T.F.   - new routine sff_New
c   V1.06   08/01/97   T.F.   - changed subroutines sff_RTraceF, sff_RTraceFI,
c                               sff_ROpenF, sff_ROpenFS and sff_RFree - there
c                               was a confusion about the meaning of lindim 
c                               - array lines before was declared as 
c                               lines(nline)*(*) but must be lines(lindim)*(*) 
c                               - nline will be returned
c   V1.07   21/01/97   T.F.   - removed the original (and unchanged) GSE2.0 
c                               code to gse20.f
c                             - changed all routines to explicit variable
c                               declaration
c                             - buildt now separate checksum routine
c                               sff_checksum
c                             - changed sff_WData, sff_RData, sff_RData2
c                             - buildt separate routine sff_RWData to
c                               reduce memory space allocated by several cbufs
c   V1.08   22/01/97   T.F.   changed sff_f2i to be aware of ampfac.eq.0.
c           30/04/97   T.F.   added explanation for workspace idata
c   V1.09   06/05/97   T.F.   added clear sign and unit definition to the
c                             coordinate format specification and the
c                             stack data field and coordinate field format
c   V1.10   23/12/03   T.F.   allow for nchar=-1 in DAST line
c                             this is needed for C++-written files
c           28/12/03   T.F.   allow for nchar=-1 in SkipData too
c   V1.10a  28/10/05   T.F.   subroutine RData2: check for 'CHK2 ' rather than
c                             'CHK2'
c   V1.10b  29/06/06   T.F.   add leading zeroes to seconds field in WID2
c   V2.00   18/11/10   T.F.   distinguish between decoding and encoding 
c                             library version:
c                             The library is able to decode files up to
c                             version 2.0 (which is the C++ standard,
c                             writing DAST -1). It encodes files with
c                             version 1.09. This is the pre 2003
c                             standard. To distinguish here was
c                             necessary, since we now share data with
c                             Bochum, where library version 1.10
c                             indicates an increased character buffer
c                             size and nothing alse...
c   V2.00a  18/11/10   T.F.   provide sff_close to synchronize API with
c                             libfapidxx
c   V2.00b  18/11/10   T.F.   there was a format error in the STAT line
c
cB
c======================================================================
c
c Definition of the Stuttgart File Format:
c
c   The SFF (Stuttgart File Format) is an attempt to reconcile
c   different demands on the way seismic data used at the Institute
c   of Geophysics at Stuttgart University should be archived.
c
c   A single data format allows the standardization of software
c   used to perform common tasks on the data as reading, 
c   writing, processing and plotting of data. Software has to
c   be written only once, may be used by many people and may
c   be kept at a single place in the computer system.
c
c   The general structure of the file format is a header block
c   followed by one or more data blocks. Within the header and
c   the data blocks optional blocks containing additional
c   information are allowed.  Each data block is structured as
c   described by the GSE2.0 format. The data are compressed
c   using second differences and are encoded into pure Ascii
c   characters using a six bit encoding scheme also described
c   by the GSE2.0 format. The Ascii encoding ensures portability
c   of the data across different operating systems and computer
c   architectures. Moreover, it allows sending data via e-mail.
c
c Overall structure
c
c   The whole datafile is ASCII readable with any text editor
c   and is therefor transferable from any system to any system
c   via email. You can extract valid GSE2.0 data blocks from
c   the files by just using a text editor to delete additional lines.
c
c   The whole file consists of one file header block and one ore
c   more data blocks:
c
c     File Header
c     Data Block
c        .
c        .
c        .
c
c File Header
c  
c   The File Header consists of a STAT line which is obligatory.
c   There may be an optional FREE block and/or and an optional
c   SRCE line:
c
c     STAT line              obligatory
c     FREE block             optional
c     SRCE line              optional
c
c Data Block
c
c   Each Data Block has to start with an obligatory DAST line
c   and a WID2 line defined in GSE2.0 format. After that
c   there have to follow the encoded data samples between
c   an DAT2 identifier and a CHK2 checksum. These lines may
c   be followed by an optional FREE block and/or an optional
c   INFO line.
c
c     DAST line              obligatory
c     WID2 line              obligatory \
c     DAT2 identifier        obligatory  | The GSE2.0 data block consists
c     dataset                obligatory  | of these four elements.
c     CHK2 line              obligatory /
c     FREE block             optional
c     INFO line              optional
c
c Definition of the elements:
c
c   STAT line
c  
c     This line provides general information about the data file
c 
c     position   format   contents
c     1-5        a5       STAT  (identifier)
c     6-12       f7.2     library version 
c                           minor versions are counted in 0.01 steps
c                           major versions are counted in 1.0 steps
c     14-26      a13      timestamp of file creation time:
c                           yymmdd.hhmmss
c     28-37      a10      code with a combination of two 
c                         possible characters:
c                          F   there follows a FREE block
c                          S   there folloes a SRCE line
c
c   FREE block
c
c     This is a block of any set of 80 characters wide lines.
c     The start of this block is indicated a single line
c     containing FREE in the first 5 positions. Another line
c     of this content indicates the end of the FREE block.
c     A FREE block may contain any usefull information for
c     the user and has to follow no other standard than
c     a line length of 80 characters.
c
c   SRCE line
c
c     This line holds information of the source that caused the
c     seismic signal
c
c     position   format   contents
c     1-5        a5       SRCE  (identifier)
c     6-25       a20      type of source (any string like "earthquake")
c     27         a1       type of coordinate system:
c                             C: cartesian    S: spherical
c     29-43      f15.6    c1   x               latitude
c     44-58      f15.6    c2   y               longitude
c     59-73      f15.6    c3   z               height
c                         see below for comments on coordinate specification
c     75-80      a6       date of source event: yymmdd
c     82-91      a10      time of source event: hhmmss.sss
c
c   DAST line
c
c     This line holds information on the actual dataset
c
c     position   format   contents
c     1-5        a5       DAST  (identifier)
c     7-16       i10      number of characters in encoded dataset
c                         From library version 1.10 this field may be -1.
c                         In this case the reading program has to determine
c                         the number of characters itself by detecting the
c                         CHK2 line. This change was necessary to implement
c                         the C++ version of libsff since this starts writing
c                         without having encoded the whole trace already.
c     18-33      e16.6    ampfac
c                         This is a factor to scale the (floating point)
c                         dataset to an desireable dynamic range
c                         before converting it to Fortran integer
c                         values. After reading the dataset and
c                         decoding and converting it to floating point
c                         you have to multiply each sample by ampfac
c                         to get back the original values.
c                         As the maximum range of integer values goes
c                         from -(2.**31) to (2.**31)-1 you might
c                         like to adjust the maximum integer value
c                         to 0x7FFFFFFF. This may cause problems
c                         as the second differences compressing algorithm
c                         may increase the dynamic range of your data
c                         by a factor of four in the worst case.
c                         It is save to adjust the largest absolute
c                         value in the dataset to (2.**23)-1 which
c                         is 0x7FFFFF.
c     35-44      a10      code with a combination of three possible
c                         characters indicating possible optional blocks
c                         and a following further dataset:
c                           F    a FREE block follows after dataset
c                           I    an INFO line follows after dataset
c                           D    there is another Data Block following
c                                in this file (this must be the last
c                                character in code)
c
c   WID2 line (is 132 characters wide!)
c
c     This waveform identification line holds information on the dataset 
c     as defined in GSE2.0 format.
c
c     position   name     format           contents
c     1-4        id       a4               WID2 (identifier)
c     6-15       date     i4,a1,i2,a1,i2   date of first sample: yyyy/mm/dd
c     17-28      time     i2,a1,i2,a1,f6.3 time of first sample: hh:mm:ss.sss
c     30-34      station  a5               for a valid GSE2.0 block use
c                                          ISC station code
c     36-38      channel  a3               for a valid GSE2.0 block use
c                                          FDSN channel designator
c     40-43      auxid    a4               auxiliary identification code
c     45-47      datatype a3               must be CM6 in SFF
c     49-56      samps    i8               number of samples
c     58-68      samprat  f11.6            data sampling rate in Hz
c     70-79      calib    e10.2            calibration factor
c     81-87      calper   f7.3             calibration period where calib
c                                          is valid
c     89-94      instype  a6               instrument type (as defined
c                                          in GSE2.0
c     96-100     hang     f5.1             horizontal orientation of
c                                          sensor, measured in degrees
c                                          clockwise from North 
c                                          (-1.0 if vertical)
c     102-105    vang     f4.1             vertical orientation of sensor,
c                                          measured in degrees from vertical
c                                          (90.0 if horizontal)
c
c   DAT2 indicator
c
c     This line indicates the beginning of the encoded dataset.
c     The dataset follows in 80 characters wide lines.
c
c   CHK2 line
c
c     Provides a checksum for the dataset. The checksum has to be
c     calculated as defined in GSE2.0:
c
c     integer function CHECKSUM(nsamp, idata)
c     integer nsamp, idata
c     integer nchecksum, data, ichecksum
c     integer MODULO_VALUE
c     parameter(MODULO_VALUE=100 000 000)
c     modulo=MODULO_VALUE
c     do 1 i=1,nsamp
c       data=idata(i)
c       if (abs(data).ge.modulo) data=data-(data/modulo)*modulo
c       nchecksum=nchecksum+data
c       if (abs(nchecksum).ge.modulo)
c    &    nchecksum=nchecksum-(nchecksum/modulo)*modulo
c   1 continue
c     ichecksum=abs(nchecksum)
c     CHECKSUM=ichecksum
c     return
c     end
c
c     position   format   contents
c     1-4        a4       CHK2 (identifier)
c     6-13       i8       checksum
c
c   INFO line
c
c     Holds additional information on the seismometer position.
c
c     position   format   contents
c     1-5        a5       INFO  (identifier)
c     6          a1       type of coordinate system:
c                             C: cartesian    S: spherical
c     8-22       f15.6    c1   x               latitude
c     23-37      f15.6    c2   y               longitude
c     38-52      f15.6    c3   z               height
c                         see below for comments on coordinate specification
c     54-57      i4       number of stacks done during acquisition
c                         (a value of zero and a value of one both mean
c                         a single shot)
c
c  Coordinate Specification
c
c    Notice that given coordinates imply a spatial relation between
c    the source location and the receiver locations. While spherical
c    coordinates refer to a fixed reference frame on the earth, cartesian
c    coordinates refer to an arbitrary origin. The creator of the 
c    datafile is responsible to take care that coordinate information
c    is consistent between the SRCE line and the several possible
c    INFO lines.
c
c    cartesian coordinates
c
c      x, y and z are vector components in a right handed cartesian
c      reference frame. While x and y lie arbitrary orientated in the
c      horizontal plane, z is counted positive upwards from an arbitrary
c      reference level (preferably the free surface). All three coordinate
c      values are measured in meters.
c
c    spherical coordinates
c
c      Latitude and longitude are given in the geographical reference frame
c      and measured in degrees. The height value gives the height above
c      the geoid and is measured in meters.
c
cC
c======================================================================
c
c this is a copy of the Nov.1996 Version
c
c  Subroutine package to read and write data in the 
c  Stuttgart File Format (SFF). All routines start with 
c  'sff_' to avoid conflicts with other routine names.
c  The following routines are provided:
c 
c  Quick access routines:                                   since version
c
c      sff_New          delete file (works as rm-command)            1.05
c                       WOpen routines refuse to replace an 
c                       existing file - delete this file first
c      sff_WOpen        open file for writing                        1.02
c                       write STAT line
c      sff_WOpenF       open file for writing                        1.02
c                       write STAT line and FREE block
c      sff_WOpenS       open file for writing                        1.02
c                       write STAT line and SRCE line
c      sff_WOpenFS      open file for writing                        1.02
c                       write STAT line, FREE block and SRCE line
c
c      sff_ROpen        open file for reading                        1.02
c                       read STAT line
c      sff_ROpenF       open file for reading                        1.02
c                       read STAT line and FREE block
c      sff_ROpenS       open file for reading                        1.02
c                       read STAT line and SRCE line
c      sff_ROpenFS      open file for reading                        1.02
c                       read STAT line, FREE block and SRCE line
c
c      sff_WTrace       write one trace                              1.02
c                       write data block and close if last
c      sff_WTraceF      write one trace                              1.02
c                       write data block and close if last
c                       write FREE block
c      sff_WTraceI      write one trace                              1.02
c                       write data block and close if last
c                       write INFO line
c      sff_WTraceFI     write one trace                              1.02
c                       write data block and close if last
c                       write FREE block and INFO line
c
c      sff_RTrace       read one trace                               1.02
c                       read data block and close if last
c      sff_RTraceF      read one trace                               1.02
c                       read data block and close if last
c                       read FREE block
c      sff_RTraceI      read one trace                               1.02
c                       read data block and close if last
c                       read INFO line
c      sff_RTraceFI     read one trace                               1.02
c                       read data block and close if last
c                       read FREE block and INFO line
c      sff_close        close input or output file                   2.00
c
c  Writing routines:
c
c      sff_QuickWrite:  write a complete file with no                1.00
c                       optional blocks
c      sff_WStatus:     write a status line (obligatory)             1.00
c      sff_WFree:       write a free block (optional)                1.00
c      sff_WSource:     write a source line (optional)               1.00
c      sff_WData:       write a data block (obligatory)              1.00
c      sff_WInfo:       write an info line (optional)                1.00
c
c  Reading routines:
c
c      sff_QuickRead:   read a complete file but return only         1.00
c                       a minimum amount of information
c      sff_RStatus:     read a status line                           1.00
c      sff_RFree:       read a free block                            1.00 
c      sff_RSource:     read a source line                           1.00 
c      sff_RData:       read a data block                            1.00
c      sff_RWid2:       read a wid2line only                         1.00
c      sff_RData2:      read a data block beginning after the        1.00
c                       WID2-line
c      sff_RInfo:       read an info line                            1.00
c
c  Skipping routines:
c
c      sff_SkipFree:    skip a free block                            1.00
c      sff_SkipData:    skip a data block including any following    1.00
c                       optional blocks
c
c  Service routines:
c
c      sff_GetDate:     extract date from WID2-line                  1.00
c      sff_GetTime:     extract time from WID2-line                  1.00
c      sff_GetStation   extract station from WID2-line               1.00
c      sff_GetChannel:  extract channel from WID2-line               1.00
c      sff_GetN:        extract number of samples from WID2-line     1.00
c      sff_GetDt:       extract sampling rate from WID2-line         1.00
c      sff_TrimLen:     return the number of characters in a         1.00
c                       string without counting trailing blanks
c      sff_PrepWid2:    prepares a WID2-line from scratch with       1.00
c                       default values for unspecified parameters
c      sff_f2i:         convert real data to integer data            1.02
c      sff_i2f:         convert integer data to real data            1.02
c                       tmin and tsec variables
c      sff_checksum     used for internal checksum calculation       1.07
c      sff_RWData       reading and writing with only one cbuf       1.07
c
c  Routines to modify WID2 line:
c
c      sff_ModWid2samprat  modify samprat entry                      1.03
c      sff_ModWid2samps modify number of samples entry               1.03
c      sff_ModWid2date  modify date of first sample                  1.03
c      sff_ModWid2time  modify time of first sample                  1.03
c      sff_ModWid2shift shift time of first sample                   1.03
c
c  Routines to manage time of first sample:
c
c      sff_TimeIsLeapYear  check for leap year                       1.03
c      sff_TimeSetDOY   set date from day of year                    1.03
c      sff_TimeGetDOY   calculate day of year                        1.03
c      sff_TimeSplit    calculate day, hour, minute and seconds      1.03
c      sff_TimeAdd      add a specific time shift                    1.03
c
cD
c======================================================================
c 
c Here is the major function that returns the actual library version
c
c The libary is able to decode files up to this version. Files with
c version larger than this will rejected upon read.
c
      real function sff_libversion()
      real libversion
      parameter(libversion=2.00)
      sff_libversion=libversion
      return
      end
cD
cD
c======================================================================
c 
c Here is the major function that returns the actual library version
c
c The library writes files in this SFF format.
c
      real function sff_encode_libversion()
      real libversion
      parameter(libversion=1.09)
      sff_encode_libversion=libversion
      return
      end
cD
c======================================================================
c
c From this point on we define some helpfull reading and writing
c routines for a quick access to SFF files.
c
cD
c----------------------------------------------------------------------
      subroutine sff_f2i(idata, fdata, nsamples, ampfac)
c 
c This routine provides real to integer conversion.
c 
c To write data with the GSE2.0 routines you must first convert
c them to integer data. To keep a good dynamic resolution we
c use a factor ampfac which allows to scale the integer data
c to a maximum dynamic range. It is save to choose a maximum
c value of 2.*23 for the integer data as the dynamic range may
c be increased by the second differences algorithm for high
c frequencies.
c 
c input:
c   fdata      array of floating point samples
c   nsamples   number of samples
c output:
c   idata      array of integer samples
c   ampfac     scaling factor for integer data
c 
c On machines that use the same amount of memory for integer and
c floating point (single real) variables you may use the same
c memory space for the integer and the real arry. A simple (and
c somehow proper) way to do this in the calling program is
c an array declaration lika that:
c
c integer idata(maxsamples)
c real fdata(maxsamples)
c equivalence(idata, fdata)
c
      integer nsamples
      integer idata(nsamples)
      real fdata(nsamples)
      real ampfac
cE
      integer sample
      real maxval
      maxval=0.
      do sample=1,nsamples
        maxval=max(maxval, abs(fdata(sample)))
      enddo
c be aware of maxval being 0. exact
      if (maxval.eq.0.) maxval=2.**23
      ampfac=maxval/(2.**23)
c be aware of ampfac being 0. exact
      if (ampfac.eq.0.) ampfac=1.
      do sample=1,nsamples
        idata(sample)=int(fdata(sample)/ampfac+0.5)
      enddo
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_i2f(idata, fdata, nsamples, ampfac)
c 
c This routine provides integer to real conversion.
c 
c For details see the explanations coming with sff_i2f.
c
c input:
c   idata        array of integer type samples
c   nsamples     number of samples
c   ampfac       scaling factor to retrieve the original values
c output:
c   fdata        array of real type samples
c
      integer nsamples
      integer idata(nsamples)
      real fdata(nsamples)
      real ampfac
cE
      integer sample
      do sample=1,nsamples
        fdata(sample)=float(idata(sample))*ampfac
      enddo
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_New(lu, filename, ierr)
c 
c This routine just deletes the file indicated by filename. You need this
c as all WOpen routines refuse to replace an existing file. You will have
c to remove this file first and than open a new one.
c
c input:
c   lu         logical file unit to use for operation
c   filename   name of file to be deleted
c output:
c   ierr       will be 0 if operation was successfull
c
      integer lu, ierr
      character filename*(*)
cE
      ierr=0
      open(lu, file=filename, err=99)
      close(lu, err=98, status='delete')
      return
   99 ierr=1
      print *,'sff_New: ERROR opening file'
      return
   98 ierr=1
      print *,'sff_New: ERROR closing and deleting file'
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WOpen(lu, filename, ierr)
c 
c Open file for writing. Write STAT line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr
      character filename*(*)
cE
      character code*10
c
      open(lu, file=filename, err=99, status='new')
      code=' ' 
      call sff_WStatus(lu, code)
      ierr=0
      return
c error messages
   99 print *,'sff_WOpen: ERROR opening file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WOpenF(lu, filename, lines, nline, ierr)
c 
c Open file for writing. Write STAT line and FREE block.
c
c input:
c   lu              logical file unit
c   filename        name of file
c   nline           number of FREE lines
c   lines           80 character wide FREE lines
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr, nline
      character*(*) filename, lines(nline)
cE
      character code*10
c
      open(lu, file=filename, err=99, status='new')
      code='F' 
      call sff_WStatus(lu, code)
      call sff_WFree(lu, nline, lines)
      ierr=0
      return
c error messages
   99 print *,'sff_WOpenF: ERROR opening file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WOpenS(lu, filename, 
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for writing. Write STAT line and SRCE line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c   type            type of source (any 20 character string)
c   cs              coordinate system (S: spherical, C: cartesian)
c   c1, c2, c3      coordiantes as defined by SFF
c   time, date      time and date of source signal
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr
      character filename*(*), type*(*), cs*1
      real c1, c2, c3
      character time*(*), date*(*)
cE
      character code*10
c
      open(lu, file=filename, err=99, status='new')
      code='S' 
      call sff_WStatus(lu, code)
      call sff_WSource(lu, type, cs, c1, c2, c3, date, time)
      ierr=0
      return
c error messages
   99 print *,'sff_WOpenS: ERROR opening file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WOpenFS(lu, filename,
     &                      lines, nline,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for writing. Write STAT line, FREE block and SRCE line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c   nline           number of lines in FREE block
c   lines           lines in FREE block
c   type            type of source (any 20 character string)
c   cs              coordinate system (S: spherical, C: cartesian)
c   c1, c2, c3      coordiantes as defined by SFF
c   time, date      time and date of source signal
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr, nline
      character filename*(*), type*(*), cs*1
      real c1, c2, c3
      character time*(*), date*(*)
      character*(*) lines(nline)
cE
      character code*10
c
      open(lu, file=filename, err=99, status='new')
      code='FS' 
      call sff_WStatus(lu, code)
      call sff_WFree(lu, nline, lines)
      call sff_WSource(lu, type, cs, c1, c2, c3, date, time)
      ierr=0
      return
c error messages
   99 print *,'sff_WOpenFS: ERROR opening file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ROpen(lu, filename, 
     &                     version, timestamp, code, ierr)
c 
c Open file for reading. Read STAT line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr
      real version
      character timestamp*(*), code *(*)
      character filename*(*)
cE
      integer i
c 
      open(lu, file=filename, err=99, status='old')
      ierr=0
      call sff_RStatus(lu, version, timestamp, code, ierr)
      if (ierr.ne.0) goto 98
      i=1
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_SkipFree(lu, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'S') then
          read(lu, '(1x)', err=96, end=95)
        endif
        i=i+1
        goto 1
      endif
      return
c error messages
   99 print *,'sff_ROpen: ERROR opening file'
      ierr=1
      return
   98 print *,'sff_ROpen: ERROR reading STAT line'
      return
   97 print *,'sff_ROpen: ERROR skipping FREE block'
      return
   96 print *,'Sff_ROpen: ERROR skipping SRCE line'
      ierr=1
      return
   95 print *,'Sff_ROpen: ERROR skipping SRCE line: unexpected end of file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ROpenF(lu, filename,
     &                      version, timestamp, code,
     &                      nline, lines, lenmax, lindim, ierr)
c 
c Open file for reading. Read STAT line and FREE block.
c 
c input:
c   lu              logical file unit
c   filename        name of file
c   lindim          number of elements in FREE block array lines
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c   nline           number of FREE lines read
c   lines           FREE lines
c   lenmax          length of longest FREE line in array lines
c
      integer lu, ierr, nline, lindim, lenmax
      real version
      character timestamp*(*), code *(*), lines(lindim)*(*)
      character filename*(*)
cE
      integer i
c set defaults
      nline=0
      lenmax=0
      lines(1)=' '
c go
      open(lu, file=filename, err=99, status='old')
      ierr=0
      call sff_RStatus(lu, version, timestamp, code, ierr)
      if (ierr.ne.0) goto 98
      i=1
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_RFree(lu, nline, lines, lenmax, lindim, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'S') then
          read(lu, '(1x)', err=96, end=95)
        endif
        i=i+1
        goto 1
      endif
      return
c error messages
   99 print *,'sff_ROpenF: ERROR opening file'
      ierr=1
      return
   98 print *,'sff_ROpenF: ERROR reading STAT line'
      return
   97 print *,'sff_ROpenF: ERROR reading FREE block'
      return
   96 print *,'Sff_ROpenF: ERROR skipping SRCE line'
      ierr=1
      return
   95 print *,'Sff_ROpenF: ERROR skipping SRCE line: unexpected end of file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ROpenS(lu, filename,
     &                      version, timestamp, code,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for reading. Read STAT line and SRCE line.
c 
c input:
c   lu              logical file unit
c   filename        name of file
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c   type            type of source
c   cs              coordinate system
c   c1, c2, c3      coordinates of source
c   date, time      date and time of source signal
c
      integer lu, ierr
      real version
      character timestamp*(*), code *(*)
      character filename*(*), type*(*), date*(*), time*(*), cs*1
      real c1, c2, c3
cE
      integer i
c set defaults
      cs='C'
      c1=0
      c2=0
      c3=0
      type='NSP'
      time='000000.000'
      date='000000'
c go 
      open(lu, file=filename, err=99, status='old')
      ierr=0
      call sff_RStatus(lu, version, timestamp, code, ierr)
      if (ierr.ne.0) goto 98
      i=1
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_SkipFree(lu, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'S') then
          call sff_RSource(lu, type, cs, c1, c2, c3, date, time, ierr)
          if (ierr.ne.0) goto 96
        endif
        i=i+1
        goto 1
      endif
      return
c error messages
   99 print *,'sff_ROpenS: ERROR opening file'
      ierr=1
      return
   98 print *,'sff_ROpenS: ERROR reading STAT line'
      return
   97 print *,'sff_ROpenS: ERROR skipping FREE block'
      return
   96 print *,'Sff_ROpenS: ERROR reading SRCE line'
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ROpenFS(lu, filename,
     &                      version, timestamp, code,
     &                      nline, lines, lenmax, lindim,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for reading. Read STAT line, FREE block and SRCE line.
c 
c input:
c   lu              logical file unit
c   filename        name of file
c   lindim          number of elements in FREE block array lines
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c   nline           number of FREE lines read
c   lines           FREE lines
c   lenmax          length of longest FREE line in array lines
c   type            type of source
c   cs              coordinate system
c   c1, c2, c3      coordinates of source
c   date, time      date and time of source signal
c
      integer lu, ierr, nline, lindim, lenmax
      real version
      character timestamp*(*), code *(*), lines(lindim)*(*)
      character filename*(*), type*(*), date*(*), time*(*), cs*1
      real c1, c2, c3
cE
      integer i
c set defaults
      nline=0
      lines(1)=' '
      lenmax=0
      cs='C'
      c1=0
      c2=0
      c3=0
      type='NSP'
      time='000000.000'
      date='000000'
c go 
      open(lu, file=filename, err=99, status='old')
      ierr=0
      call sff_RStatus(lu, version, timestamp, code, ierr)
      if (ierr.ne.0) goto 98
      i=1
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_RFree(lu, nline, lines, lenmax, lindim, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'S') then
          call sff_RSource(lu, type, cs, c1, c2, c3, date, time, ierr)
          if (ierr.ne.0) goto 96
        endif
        i=i+1
        goto 1
      endif
      return
c error messages
   99 print *,'sff_ROpenFS: ERROR opening file'
      ierr=1
      return
   98 print *,'sff_ROpenFS: ERROR reading STAT line'
      return
   97 print *,'sff_ROpenFS: ERROR reading FREE block'
      return
   96 print *,'Sff_ROpenFS: ERROR reading SRCE line'
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WTrace(lu, wid2line, nsamp, fdata, idata, last, ierr)
c
c Write one data block starting with DAST line.
c The File will be closed after writing the last trace.
c 
c input:
c   lu              logical file unit
c   wid2line        valid WID2 line
c   nsamp           number of samples
c   fdata           data array
c   last            must be true is the trace to be written is the
c                   last one in this file
c ouput:
c   ierr            error status (ok: ierr=0)
c
c workspace:
c   idata           fdata will be converted to idata using sff_f2i
c                   (both array may be in same memory space - see
c                   comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp)
      real fdata(nsamp)
      logical last
      character wid2line*132
cE
      character code*(10)
      real ampfac
      integer ierr
c
      ierr=0
      if (last) then
        code=' '
      else
        code='D'
      endif
      call sff_f2i(idata, fdata, nsamp, ampfac)
      call sff_WData(lu, wid2line, nsamp, idata, ampfac, code)
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_WTrace: ERROR closing file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WTraceF(lu, 
     &                   wid2line, nsamp, fdata, idata, last, 
     &                   nline, lines, ierr)
c
c Write one data block starting with DAST line.
c Write also FREE block.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   wid2line        valid WID2 line
c   nsamp           number of samples
c   fdata           data array
c   last            must be true is the trace to be written is the
c                   last one in this file
c   nline           number of FREE block lines
c   lines           FREE block lines
c ouput:
c   ierr            error status (ok: ierr=0)
c
c workspace:
c   idata           fdata will be converted to idata using sff_f2i
c                   (both array may be in same memory space - see
c                   comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nline
      real fdata(nsamp)
      logical last
      character wid2line*132
      character*(*) lines(nline)
cE
      character code*(10)
      real ampfac
      integer ierr
c
      ierr=0
      if (last) then
        code='F'
      else
        code='FD'
      endif
      call sff_f2i(idata, fdata, nsamp, ampfac)
      call sff_WData(lu, wid2line, nsamp, idata, ampfac, code)
      call sff_WFree(lu, nline, lines)
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_WTraceF: ERROR closing file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WTraceI(lu, 
     &                   wid2line, nsamp, fdata, idata, last, 
     &                   cs, c1, c2, c3, nstack, ierr)
c
c Write one data block starting with DAST line.
c Write also  INFO line.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   wid2line        valid WID2 line
c   nsamp           number of samples
c   fdata           data array
c   last            must be true is the trace to be written is the
c                   last one in this file
c   cs              coordinate system
c   c1, c2, c3      receiver coordinates
c   nstack          number of stacks
c ouput:
c   ierr            error status (ok: ierr=0)
c
c workspace:
c   idata           fdata will be converted to idata using sff_f2i
c                   (both array may be in same memory space - see
c                   comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nstack
      real fdata(nsamp), c1, c2, c3
      logical last
      character wid2line*132, cs*1
cE
      character code*(10)
      real ampfac
      integer ierr
c
      ierr=0
      if (last) then
        code='I'
      else
        code='ID'
      endif
      call sff_f2i(idata, fdata, nsamp, ampfac)
      call sff_WData(lu, wid2line, nsamp, idata, ampfac, code)
      call sff_WInfo(lu, cs, c1, c2, c3, nstack)
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_WTraceI: ERROR closing file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WTraceFI(lu, 
     &                   wid2line, nsamp, fdata, idata, last, 
     &                   nline, lines, 
     &                   cs, c1, c2, c3, nstack, ierr)
c
c Write one data block starting with DAST line.
c Write also FREE block and INFO line.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   wid2line        valid WID2 line
c   nsamp           number of samples
c   fdata           data array
c   last            must be true is the trace to be written is the
c                   last one in this file
c   nline           number of FREE block lines
c   lines           FREE block lines
c   cs              coordinate system
c   c1, c2, c3      receiver coordinates
c   nstack          number of stacks
c
c ouput:
c   ierr            error status (ok: ierr=0)
c
c workspace:
c   idata           fdata will be converted to idata using sff_f2i
c                   (both array may be in same memory space - see
c                   comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nline, nstack
      real fdata(nsamp), c1, c2, c3
      logical last
      character wid2line*132, cs*1
      character lines(nline)*(*)
cE
      character code*(10)
      real ampfac
      integer ierr
c
      ierr=0
      if (last) then
        code='FI'
      else
        code='FID'
      endif
      call sff_f2i(idata, fdata, nsamp, ampfac)
      call sff_WData(lu, wid2line, nsamp, idata, ampfac, code)
      call sff_WFree(lu, nline, lines)
      call sff_WInfo(lu, cs, c1, c2, c3, nstack)
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_WTraceFI: ERROR closing file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RTrace(lu, tanf, dt, 
     &                   wid2line, nsamp, fdata, idata, code, last, ierr)
c
c Read one data block starting with DAST line.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   nsamp           array dimension of idata and fdata
c ouput:
c   ierr            error status (ok: ierr=0)
c   code            code indicating optional blocks
c   wid2line        valid WID2 line
c   tanf            time of first sample from midnight
c   dt              sampling interval in seconds
c   nsamp           number of samples
c   fdata           data array
c   last            is true if read trace is the last one in this file
c
c workspace:
c   idata           data will be first read to idata and then converted
c                   to fdata using sff_i2f (both array may be in same memory
c                   space - see comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp)
      real fdata(nsamp), dt, tanf
      logical last
      character wid2line*132, code*(*)
cE
      integer i
      real ampfac
      integer ierr
c
      ierr=0
      call sff_RData(lu, wid2line, nsamp, tanf, dt, idata, ampfac, code, ierr)
      if (ierr.ne.0) goto 96
      call sff_i2f(idata, fdata, nsamp, ampfac)
      i=1
      last=.true.
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_SkipFree(lu, ierr)
          if (ierr.ne.0) goto 98
        endif
        if (code(i:i).eq.'I') then
          read(lu, '(1x)', err=97, end=95)
        endif
        if (code(i:i).eq.'D') last=.false.
        i=i+1
        goto 1
      endif
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_RTrace: ERROR closing file'
      ierr=1
      return
   98 print *,'sff_RTrace: ERROR skipping FREE block'
      return
   97 print *,'sff_RTrace: ERROR skipping INFO line'
      ierr=1
      return
   96 print *,'sff_RTrace: ERROR reading data'
      return
   95 print *,'sff_RTrace: ERROR skipping INFO line - unexpected end of file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RTraceF(lu, tanf, dt,
     &                   wid2line, nsamp, fdata, idata, code, last,
     &                   nline, lines, lindim, lenmax, ierr)
c
c Read one data block starting with DAST line.
c Read also FREE block.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   lindim          number of FREE block elements in array lines
c   nsamp           array dimension of idata and fdata
c ouput:
c   ierr            error status (ok: ierr=0)
c   code            code indicating optional blocks
c   wid2line        valid WID2 line
c   tanf            time of first sample from midnight
c   dt              sampling interval in seconds
c   nsamp           number of samples
c   fdata           data array
c   last            is true if read trace is the last one in this file
c   nline           number of FREE block lines read
c   lines           FREE block lines
c   lenmax          length of longest line in FREE block array lines
c
c workspace:
c   idata           data will be first read to idata and then converted
c                   to fdata using sff_i2f (both array may be in same memory
c                   space - see comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nline, lindim, lenmax
      real fdata(nsamp), dt, tanf
      logical last
      character wid2line*132, lines(lindim)*(*), code*(*)
cE
      integer i, ierr
      real ampfac
c set defaults
      nline=0
      lenmax=0
      lines(1)=' '
c
      ierr=0
      call sff_RData(lu, wid2line, nsamp, tanf, dt, idata, ampfac, code, ierr)
      if (ierr.ne.0) goto 96
      call sff_i2f(idata, fdata, nsamp, ampfac)
      i=1
      last=.true.
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_RFree(lu, nline, lines, lenmax, lindim, ierr)
          if (ierr.ne.0) goto 98
        endif
        if (code(i:i).eq.'I') then
          read(lu, '(1x)', err=97, end=95)
        endif
        if (code(i:i).eq.'D') last=.false.
        i=i+1
        goto 1
      endif
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_RTraceF: ERROR closing file'
      ierr=1
      return
   98 print *,'sff_RTraceF: ERROR reading FREE block'
      return
   97 print *,'sff_RTraceF: ERROR skipping INFO line'
      ierr=1
      return
   96 print *,'sff_RTraceF: ERROR reading data'
      return
   95 print *,'sff_RTraceF: ERROR skipping INFO line - unexpected end of file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RTraceI(lu, tanf, dt,
     &                   wid2line, nsamp, fdata, idata, code, last,
     &                   cs, c1, c2, c3, nstack, ierr)
c
c Read one data block starting with DAST line.
c Read also INFO line.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   nsamp           array dimension of idata and fdata
c ouput:
c   ierr            error status (ok: ierr=0)
c   code            code indicating optional blocks
c   wid2line        valid WID2 line
c   tanf            time of first sample from midnight
c   dt              sampling interval in seconds
c   nsamp           number of samples
c   fdata           data array
c   last            is true if read trace is the last one in this file
c   cs              coordinate system
c   c1, c2, c3      receiver coordinates
c   nstack          number of stacks
c
c workspace:
c   idata           data will be first read to idata and then converted
c                   to fdata using sff_i2f (both array may be in same memory
c                   space - see comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nstack
      real fdata(nsamp), c1, c2, c3, dt, tanf
      logical last
      character wid2line*132, cs*1, code*(*)
cE
      integer i, ierr
      real ampfac
c set defaults
      cs='C'
      c1=0
      c2=0
      c3=0
      nstack=0
c
      ierr=0
      call sff_RData(lu, wid2line, nsamp, tanf, dt, idata, ampfac, code, ierr)
      if (ierr.ne.0) goto 96
      call sff_i2f(idata, fdata, nsamp, ampfac)
      i=1
      last=.true.
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_SkipFree(lu, ierr)
          if (ierr.ne.0) goto 98
        endif
        if (code(i:i).eq.'I') then
          call sff_RInfo(lu, cs, c1, c2 ,c3, nstack, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'D') last=.false.
        i=i+1
        goto 1
      endif
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_RTraceI: ERROR closing file'
      ierr=1
      return
   98 print *,'sff_RTraceI: ERROR skipping FREE block'
      return
   97 print *,'sff_RTraceI: ERROR reading INFO line'
      return
   96 print *,'sff_RTraceI: ERROR reading data'
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RTraceFI(lu, tanf, dt,
     &                   wid2line, nsamp, fdata, idata, code, last,
     &                   nline, lines, lindim, lenmax,
     &                   cs, c1, c2, c3, nstack, ierr)
c
c Read one data block starting with DAST line.
c Read also FREE block and INFO line.
c The File will be closed after writing the last trace.
c 
c input
c   lu              logical file unit
c   lindim          number of elements in FREE block array lines
c   nsamp           array dimension of idata and fdata
c ouput:
c   ierr            error status (ok: ierr=0)
c   code            code indicating optional blocks
c   wid2line        valid WID2 line
c   tanf            time of first sample from midnight
c   dt              sampling interval in seconds
c   nsamp           number of samples
c   fdata           data array
c   last            is true if read trace is the last one in this file
c   nline           number of FREE block lines read
c   lines           FREE block lines
c   lenmax          length of longest line in FREE block array lines
c   cs              coordinate system
c   c1, c2, c3      receiver coordinates
c   nstack          number of stacks
c
c workspace:
c   idata           data will be first read to idata and then converted
c                   to fdata using sff_i2f (both array may be in same memory
c                   space - see comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nline, nstack, lindim, lenmax
      real fdata(nsamp), c1, c2, c3, dt, tanf
      logical last
      character wid2line*132, lines(lindim)*(*), cs*1, code*(*)
cE
      integer i, ierr
      real ampfac
c set defaults
      nline=0
      lenmax=0
      lines(1)=' '
      cs='C'
      c1=0
      c2=0
      c3=0
      nstack=0
c
      ierr=0
      call sff_RData(lu, wid2line, nsamp, tanf, dt, idata, ampfac, code, ierr)
      if (ierr.ne.0) goto 96
      call sff_i2f(idata, fdata, nsamp, ampfac)
      i=1
      last=.true.
    1 if (code(i:i).ne.' ') then
        if (code(i:i).eq.'F') then
          call sff_RFree(lu, nline, lines, lenmax, lindim, ierr)
          if (ierr.ne.0) goto 98
        endif
        if (code(i:i).eq.'I') then
          call sff_RInfo(lu, cs, c1, c2 ,c3, nstack, ierr)
          if (ierr.ne.0) goto 97
        endif
        if (code(i:i).eq.'D') last=.false.
        i=i+1
        goto 1
      endif
      if (last) close(lu, err=99)
      return
c error messages
   99 print *,'sff_RTraceFI: ERROR closing file'
      ierr=1
      return
   98 print *,'sff_RTraceFI: ERROR reading FREE block'
      return
   97 print *,'sff_RTraceFI: ERROR reading INFO line'
      return
   96 print *,'sff_RTraceFI: ERROR reading data'
      return
      end
cD
c======================================================================
c
c Now there will follow some routines for easy WID2 handling
c
cD
c----------------------------------------------------------------------
      subroutine sff_ModWid2samprat(wid2line, samprat)
c
c Just modify samprat-entry in WID2 line
c
c input:
c   samprat                new sampling rate to set
c input and output:
c   wid2line               modified WID2 line
c
c no default setting are allowed
c
      real samprat
      character wid2line*(*)
cE
      write(wid2line(58:68), '(f11.6)') samprat
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ModWid2samps(wid2line, samps)
c
c Just modify samps-entry in WID2 line
c
c input:
c   samps                  new number of samples to set
c input and output:
c   wid2line               modified WID2 line
c
c no default setting are allowed
c
      integer samps
      character wid2line*(*)
cE
      write(wid2line(49:56), '(i8)') samps
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ModWid2date(wid2line, year, month, day)
c
c Just modify date-entry in WID2 line
c
c input:
c   year, month, day       new date to set
c input and output:
c   wid2line               modified WID2 line
c
c no default setting are allowed
c
      integer year, month, day
      character wid2line*(*)
cE
      write(wid2line(6:15), '(i4.4,1h/,i2.2,1h/,i2.2)') year,month,day
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ModWid2time(wid2line, hour, minute, second)
c
c Just modify time-entry in WID2 line
c
c input:
c   hour, minute, second   new time to set
c input and output:
c   wid2line               modified WID2 line
c
c no default setting are allowed
c
      integer hour, minute
      real second
      character wid2line*(*)
cE
      write(wid2line(17:28), '(i2.2,1h:,i2.2,1h:,f6.3)') hour,minute,second
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_ModWid2shift(wid2line, tmin, tsec)
c
c Change time and date of first sample by adding tmin minutes
c and tsec seconds
c
c input:
c   tmin        minutes to add
c   tsec        seconds to add
c input and output:
c   wid2line    string to change
c
      real tmin, tsec
      character*(*) wid2line
cE
      character time*12, date*10
      integer year1, doy1, month1, day1, hour1, minute1
      integer minute2
      integer year, doy, month, day, hour, minute
      real second1, second2, second
      integer sff_TimeGetDOY
c
      call sff_GetDate(wid2line, date)
      call sff_GetTime(wid2line, time)
      read(date, '(i4,1x,i2,1x,i2)') year1, month1, day1
      read(time, '(i2,1x,i2,1x,f6.3)') hour1, minute1, second1
      doy1=sff_TimeGetDOY(year1, month1, day1)
      minute2=int(tmin)
      second2=tsec+60.*(tmin-float(minute2))
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 0, 0, 0, minute2, second2,
     &                 year, doy, hour, minute, second)
      call sff_TimeSetDOY(doy, year, month, day)
      call sff_ModWid2date(wid2line, year, month, day)
      call sff_ModWid2time(wid2line, hour, minute, second)
      return
      end
cD
c======================================================================
c
c Here we go with some definitions of easy time handling routines
c
cD
c----------------------------------------------------------------------
      logical function sff_TimeIsLeapYear(year)
c
c Returns true if year is a leap year
c
c input
c   year:        year to check
c
      integer year
cE
      logical result
c
      result=(((mod(year,4).eq.0).and.(mod(year,100).ne.0)).or.
     &     (mod(year,400).eq.0)) 
      sff_TimeIsLeapYear=result
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_TimeSetDOY(doy, year, month, day)
c
c Returns month and day for day of year
c
c input:
c   doy, year           date for which the day of year should be 
c                       calculated
c output:
c   month, day          date for corresponding day of year
c
      integer year, month, day, doy
cE
      integer days(12,2), iyear
      logical sff_TimeIsLeapYear
      data days/31,28,31,30,31,30,31,31,30,31,30,31,
     &          31,29,31,30,31,30,31,31,30,31,30,31/
c
      iyear=1
      if (sff_TimeIsLeapYear(year)) iyear=2 
      day=doy
      month=1
    1 if (day.gt.days(month,iyear)) then
        day=day-days(month,iyear)
        month=month+1
        goto 1
      endif
      return
      end
cD
c----------------------------------------------------------------------
      integer function sff_TimeGetDOY(year, month, day)
c
c Returns the number of the day in the year
c
c input:
c   year, month, day    date for which the day of year should be 
c                       calculated
c
      integer year, month, day
cE
      integer days(12,2), iyear, i, doy
      logical sff_TimeIsLeapYear
      data days/0,31,28,31,30,31,30,31,31,30,31,30,
     &          0,31,29,31,30,31,30,31,31,30,31,30/
c
      iyear=1
      if (sff_TimeIsLeapYear(year)) iyear=2 
      doy=0
      do i=1,month
        doy=doy+days(i,iyear)
      enddo
      doy=doy+day
      sff_TimeGetDOY=doy
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_TimeSplit(tsec, day, hour, minute, second)
c
c Split a time given in seconds into day, hour, minute and
c second part
c
c input:
c   tsec                        time in seconds
c output:
c   day, hour, minute, second   time parts
c
      real tsec, second
      integer day, hour, minute
cE
      second=mod(tsec,60.)
      day=int(tsec/60)
      minute=mod(day,60)
      day=int(day/60)
      hour=mod(day,24)
      day=int(day/24)
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                       year2, doy2, hour2, minute2, second2,
     &                       year,  doy,  hour,  minute,  second)
c
c Adds two dates
c
c input:
c   year1, doy1, hour1, minute1, second1   first date to add
c   year2, doy2, hour2, minute2, second2   second date to add
c output:
c   year, doy, hour, minute, second   sum of both dates
c
      integer year1, doy1, hour1, minute1
      integer year2, doy2, hour2, minute2
      integer year,  doy,  hour,  minute
      real second1, second2, second
cE
      integer diy
      logical sff_TimeIsLeapYear
c
      second=second1+second2
      minute=int(second/60)
      second=second-float(minute)*60.
      minute=minute+minute1+minute2
      hour=int(minute/60)
      minute=minute-hour*60
      hour=hour+hour1+hour2
      doy=int(hour/24)
      hour=hour-doy*24
      doy=doy+doy1+doy2
      year=year1+year2
      if (sff_TimeIsLeapYear(year)) then
        diy=366
      else
        diy=365
      endif
c
    1   if (doy.lt.1) then
          year=year-1
          if (sff_TimeIsLeapYear(year)) then
            diy=366
          else
            diy=365
          endif
          doy=doy+diy
          goto 1
        elseif (doy.gt.diy) then
          doy=doy-diy
          year=year+1
          if (sff_TimeIsLeapYear(year)) then
            diy=366
          else
            diy=365
          endif
          goto 1
        endif
      return
      end
cD
c======================================================================
c
c From this point on there follows the original code from version 1.0
c
c---------------------------------------------------------------------
c                  S F F (Stuttgart file format)
c
c  Version 1.0    March 27, 1996    Wolfgang Friederich
cD
c----------------------------------------------------------------------
      subroutine sff_QuickWrite(lu,wid2line,nsamp,idata,ampfac)
c
c  writes a complete SFF-File containing ONE data block,
c  no optional blocks are written
c  Arguments:
c    lu:            file unit
c    wid2line:      WID2-line
c    nsamp:         number of samples
c    idata:         data
c    ampfac:        amplification factor
c 
      character wid2line*(*)
      integer idata(nsamp), nsamp, lu
      real ampfac
cE
c 
c
      call sff_WStatus(lu,' ')
c
c  write data
c
      call sff_WData(lu,wid2line,nsamp,idata,ampfac,' ')
c
      return
      end
cD
c---------------------------------------------------------------------
      subroutine sff_QuickRead(lu,wid2line,nsamp,tanf,dt,idata,ampfac,ierr)
c
c  Input:  lu:   File unit
c  Reads a complete SFF-File containing ONE data block, 
c  but returns only
c    wid2line:      WID2-line
c    nsamp:         number of samples
c    tanf:          starting time in seconds after midnight
c    dt:            sampling interval
c    idata:         data
c    ampfac:        amplification factor
c
c  Any optional blocks following the data are ignored
c 
      character wid2line*(*)
      integer idata(nsamp), nsamp, lu, ierr
      real tanf, dt, ampfac
cE
      character code*10,timestamp*13
      real version
      integer i
c
      ierr=0
c
      call sff_RStatus(lu,version,timestamp,code,ierr)
c
c  skip optional FS blocks
c
      i=1
 11   if(code(i:i).ne.' ') then
        if(code(i:i).eq.'F') call sff_SkipFree(lu,ierr)
        if(code(i:i).eq.'S') read(lu,'(1x)')
        i=i+1
        goto 11
      endif
c
c  read data
c
      call sff_RData(lu,wid2line,nsamp,tanf,dt,idata,ampfac,code,ierr)
c
      return
      end
cD
c--------------------------------------------------------------------
      subroutine sff_WStatus(lu,code)
c
c  Writes a STATUS line
c
c  A status line consists of version number, date and time the file
c  was written in form yyddmm.hhmmss,
c  and a character code to specify what kind of optional blocks follow.
c  Input:
c    lu:            File unit
c    code:          Character code like FS not longer than 10 chars
c 
c  major changes:
c    22/11/96   T.F.   version is now set by sff_libversion
c    18/11/10   T.F.   now write sff_encode_libversion
c    18/11/10   T.F.   there was a format error, which we missed ever
c                      since
c
      integer lu
      character code*(*)
cE
      character timestamp*13
      integer iarray(3), jyear, i
      real sff_encode_libversion, version
c
      version=sff_encode_libversion()
      call idate(iarray)
      jyear=iarray(3)-100*(iarray(3)/100)
      write(timestamp(1:7),'(3i2.2,a1)') jyear,iarray(2),iarray(1),'.'
      call itime(iarray)
      write(timestamp(8:13),'(3i2.2)') (iarray(i),i=1,3)
c
      write(lu,'(a5,f7.2,1x,a13,1x,a)') 'STAT ',version,timestamp,code
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RStatus(lu,version,timestamp,code,ierr)
c
c  Reads a status line
c
c  input: lu:            File unit
c  output: version, timestamp, code, ierr
c  ierr = 1, if an error occured, 0 otherwise
c  see also WStatus
c 
c  major changes:
c    22/11/96   T.F.   checks now file version against sff_libversion
c
      integer lu, ierr
      character code*(*), timestamp*(*)
      real version
cE
      integer ncode, nstamp, lstamp, nc
      parameter(ncode=2, nstamp=13)
      character stamp*(nstamp), stat*5
      real sff_libversion
c
      ierr=0
      nc=len(code)
      if(nc.lt.ncode) then
        write(0,'(a,2i3,a)') 'RStatus ERROR: Code must have at least ',
     1              ncode,' characters'
        ierr = 1
        return
      endif
      read(lu,'(a5,1x,f6.2,1x,a13,1x,a)', err=101) 
     &  stat,version,stamp,code
      if(stat.ne.'STAT ') then
        write(0,'(a)') 'RStatus ERROR: not a STATUS line'
        ierr = 1
        return
      endif
      lstamp=len(timestamp)
      if(lstamp.lt.nstamp) then
        write(0,'(a,i2,a)') 'RStatus WARNING: timestamp should have ',
     1              nstamp,' characters! Timestamp is truncated.'
      endif
      timestamp=stamp(1:min(lstamp,nstamp))
c  check version
      ierr = 0
      if (version.gt.sff_libversion()) then
        if (int(sff_libversion()).lt.int(version)) then
          write(0,'(a)') 
     &      'RStatus ERROR: file has higher major version than library'
          ierr=1
        else
          write(0,'(a)')
     &      'RStatus WARNING: file has higher minor version than library'
          write(0,'(a)')
     &      '                 this should cause no problems'
        endif
        write(0,'(a,1x,f6.2,3x,a,1x,f6.2)')
     &                 'RStatus: file version:',version,
     &                       'library version:',sff_libversion()
      endif
c 
      return
 101  write(0,'(a,i3)') 'RStatus: ERROR reading file with unit ',lu
      ierr =1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WFree(lu,nline,lines)
c
c  Writes a free block
c  all input
c
c  A free block consists of the word 'FREE ' followed by an arbitrary number
c  of lines with text of max 80 chars and a final line with the word FREE
c
      integer lu, nline
      character*(*) lines(nline)
cE
      integer i
c 
      write(lu,'(a5)') 'FREE '
      do 10 i=1,nline
 10     write(lu,'(a)') lines(i)
      write(lu,'(a5)') 'FREE '
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RFree(lu,nline,lines,lenmax,lindim,ierr)
c
c  Reads a free block
c
c  Input: lu:     logical file unit
c         lindim: number of elements in FREE block array lines
c  Output:
c         nline, lines, ierr, lenmax
c
c  ierr = 1 if an error occurs, 0 otherwise
c  lenmax  gives length of longest line excluding trailing blanks
c  see also WFree
c
      character lines(lindim)*(*)
      integer lu, nline, lenmax, lindim, ierr
cE
      integer linelen, llen, i, ntrim
      parameter(linelen=80)
      character lin*(linelen), free*5
c
c      print *,'DEBUG: read FREE block (in libsff)'
      ierr=0
      llen=len(lines(1))
      if(llen.lt.linelen) then
        write(0,'(a,i2,a)') 'RFree WARNING: lines should have at least ',
     1              linelen,' characters! Lines are truncated.'
      endif
c      print *,'DEBUG: read FREE block - loc 1'
      read(lu,'(a5)') free
c      print *,'DEBUG: read FREE block - loc 2'
      if(free.ne.'FREE ') then
        write(0,'(a)') 'RFree ERROR: not a FREE block'
        ierr = 1
        return
      endif
      i=1
      lenmax = 0
c      print *,'DEBUG: read FREE block - loc 2a'
 101  read(lu,'(a)') lin
      if(lin(1:4).ne.'FREE') then
        call sff_TrimLen(lin,ntrim)
        lenmax = max(lenmax,min(ntrim,llen))
        lines(i) = lin(1:min(ntrim,llen))
        i=i+1
c        print *,'DEBUG: read FREE block - loc 3'
        if(i.gt.lindim) then
          write(0,'(a,a)') 'RFree WARNING: more lines in FREE block ',
     1            'than specified. Some lines will be missing. '
          goto 102
        endif
        goto 101
      endif
 102  nline=i-1
      ierr = 0
      return
      end      
cD
c----------------------------------------------------------------------
      subroutine sff_SkipFree(lu,ierr)
c 
c  Skips a free block
c
c  Input: lu:            File unit
c
      integer lu,ierr
cE
      character free*5, lin*80
c
      ierr=0
      read(lu,'(a5)') free
      if(free.ne.'FREE ') then
        write(0,'(a)') 'SkipFree ERROR: not a FREE block'
        ierr = 1
        return
      endif
 101    read(lu,'(a)') lin
        if(lin(1:4).ne.'FREE') then
          goto 101
        endif
      ierr = 0
      return
      end      
cD
c----------------------------------------------------------------------
      subroutine sff_WSource(lu,typh,cs,c1,c2,c3,date,time)
c
c  Writes a source line
c
c  A source line consists of the word 'SRCE ' followed by
c      typh:       type of source: e.g. earthquake, hammer, gun, dropweight,
c                    maximally 20 characters long, if the passed string is
c                    longer it will be truncated to 20 chars
c      cs:         used coordinate system: C (cartesian) and S (spherical)
c      c1 - c3     coordinates of source: x,y,z (in meters) or latitude,
c                  longitude, height (in degrees and meters), format 3f15.6
c      date:       Date of shot: yymmdd
c      time:       Time of shot: hhmmss.sss
c  The line is written with format:
c    a5, a20, 1x, a1, 1x, 3f15.6, 1x, a6, 1x, a10
c
      integer lu
      character *(*) typh
      character cs*1, date*6, time*10
      real c1, c2, c3
cE
      integer lentyp, ltyp
      parameter(lentyp=20)
      character typ*(lentyp)
c
      ltyp = len(typh)
      typ=typh(1:min(ltyp,lentyp))
      write(lu,'(a5, a20, 1x, a1, 1x, 3f15.6, 1x, a6, 1x, a10)')
     1        'SRCE ', typ, cs, c1, c2, c3, date, time
c
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RSource(lu,typ,cs,c1,c2,c3,date,time,ierr)
c
c  Reads a source line
c
c  Input:   lu     file unit
c  Output:  typ, cs, c1, c2, c3, date, time
c  see also description for WSource
c
      integer lu, ierr
      character*(*) typ, date, time
      character cs*1
      real c1, c2, c3
cE
      integer ntyp, ndate, ntime, ltyp, ldate, ltime
      parameter(ntyp=20, ndate=6, ntime=10)
      character typh*(ntyp), dateh*(ndate), timeh*(ntime)
      character src*5
c
      ierr=0
      ltyp=len(typ)
      ldate=len(date)
      ltime=len(time)
      if(ltyp.lt.ntyp .or. ldate.lt.ndate .or. ltime.lt.ntime) then
        write(0,'(a,a,3i3)') 'RSource WARNING: Length of typ, date, time ',
     1      'should be at least ',ntyp, ndate, ntime
        write(0,'(a)') 'RSource WARNING: Corresponding strings will be truncated'
      endif
      read(lu,'(a5, a20, 1x, a1, 1x, 3f15.6, 1x, a6, 1x, a10)', err = 101)
     1            src, typh, cs, c1, c2, c3, dateh, timeh
      if(src.ne.'SRCE ') then
        write(0,'(a)') 'RSource ERROR: Not a SRCE line'
        ierr = 1
        return
      endif
      typ = typh(1:min(ntyp,ltyp))
      date = dateh(1:min(ndate,ldate))
      time = timeh(1:min(ntime,ltime))
      ierr = 0
      return
 101  write(0,'(a,i3)') 'RSource: ERROR reading file with unit ', lu
      ierr = 1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WInfo(lu,cs,c1,c2,c3,nstack)
c
c  Writes an info line (associated with one data block)
c  all input
c
c  An info line consists of the word 'INFO ' followed by
c    cs:           coordinate system used (C or S, see WSource)
c    c1, c2, c3:   station coordinates (x,y,z) or (lat, lon, height),
c                    format f15.6
c    nstack:       number of stacks  (i4)
c  The line is written with format: a5, a1, 1x, 3f15.6, 1x, i4
c
      integer lu, nstack
      real c1, c2, c3
      character cs*1
cE
c
      write(lu,'(a5, a1, 1x, 3f15.6, 1x, i4)')
     1        'INFO ', cs, c1, c2, c3, nstack
c
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RInfo(lu,cs,c1,c2,c3,nstack,ierr)
c
c  Reads an info line (associated with one data block)
c  see also WInfo
c
      integer lu, ierr, nstack
      real c1, c2, c3
      character cs*1
cE
      character info*5
c
      ierr=0
      read(lu,'(a5, a1, 1x, 3f15.6, 1x, i4)', err = 101)
     1            info, cs, c1, c2, c3, nstack
      if(info.ne.'INFO ') then
        write(0,'(a)') 'RInfo ERROR Not a INFO line '
        ierr = 1
        return
      endif
      ierr = 0
      return
 101  write(0,'(a,i3)') 'RInfo: ERROR reading file with unit ',lu
      ierr = 1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_checksum(nsamp, idata, nchecksum)
c
c  Calculates GSE2.0 defined checksum of data array idata
c
c  input:
c    idata      integer data array
c    nsamp      number of samples in idata
c  output:
c    nchecksum  integer checksum for idata
c
      integer nsamp, nchecksum
      integer idata(nsamp)
cE
      integer MODULO_VALUE, modulo, i, j
      parameter(MODULO_VALUE=100 000 000)
c
      nchecksum=0
      modulo=MODULO_VALUE
      do i=1,nsamp
        j=idata(i)
        if (abs(j).ge.modulo) j=j-int(j/modulo)*modulo
        nchecksum=nchecksum+j
        if (abs(nchecksum).ge.modulo)
     &    nchecksum=nchecksum-int(nchecksum/modulo)*modulo
      enddo
      nchecksum=abs(nchecksum)
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RWData(lu, wid2line, code, ampfac, nsamp, idata, 
     &  rwflag, ierr)
c
c  This routine performs reading AND writing actions to reduce
c  memory space by using only one cbuf-buffer. The actions include
c  DAST, WID2, DAT2 and CHK2.
c 
c  rwflag selects reading or writing
c
c  reading  rwflag must be .TRUE.
c  -------
c 
c    input:
c      lu           logical file unit
c      nsamp        array dimension of idata
c    output:
c      nsamp        number of read samples
c      idata        read samples
c      code         flag code of DAST line
c      ampfac       ampfac of DAST line
c      wid2line     WID2 line read
c      ierr         =0 if no error occured
c
c  writing  rwflag must be .FALSE.
c  -------
c
c    input
c      lu           logical file unit
c      nsamp        number of samples to write
c      idata        samples to write
c      ampfac       ampfac of DAST line
c      code         DAST line flag code to write
c      wid2line     WID2 line to write
c    output
c      ierr         =0 if no error occured
c
      integer lu, nsamp, ierr
      integer idata(nsamp)
      logical rwflag
      character*132 wid2line
      character code*(*)
      real ampfac
cE
      integer maxbuf, nchar, nchecksum, ichecksum, i, iline, nexpect, ierror
      integer sff_GetN
      parameter(maxbuf=600 000)
      character*1 cbuf(maxbuf)
      character label*5, line*80
c
      if (rwflag) then
c
c reading
c
        ierr=0
c read DAST line
        read(lu, '(a5, 1x, i10, 1x, e16.6, 1x, a)', err=99, end=98)
     &    label, nchar, ampfac, code
        if (label.ne.'DAST ') then
          print *,'RWData: ERROR missing DAST label'
          ierr=1
          return
        endif
c read WID2 line
        read(lu, '(a)', err=99, end=98) wid2line
        if (wid2line(1:5).ne.'WID2 ') then
          print *,'RWData: ERROR missing WID2 label'
          ierr=1
          return
        endif
        nexpect=sff_GetN(wid2line)
c read DAT2 block
        read(lu, '(a5)', err=99, end=98) label
        if (label.ne.'DAT2 ') then
          print *,'RWData: ERROR missing DAT2 label'
          ierr=1
          return
        endif
        iline=0
    1   continue
          read(lu, '(a)', err=99, end=98) line
          if(line(1:5).eq.'CHK2 ') goto 2
          iline=iline+1
          if ((80*iline).gt.maxbuf) then
            print *,'RWData: ERROR character buffer too small'
            ierr=1
            return
          endif
          do i=1,80
            cbuf(80*(iline-1)+i)=line(i:i)
          enddo
          goto 1
    2   continue
        read(line(5:), *) ichecksum
c The C++ library cannot know the number of characters when writing the DAST
c line and will write '-1'. Thus we allow for a sloppy mode here.
        if (nchar.eq.-1) then
          nchar=(iline-1)*80
          do while((cbuf(nchar+1).ne.' ').and.(nchar.lt.(iline*80)))
            nchar=nchar+1
          enddo
        else
          if (nchar.ne.(iline*80)) then
            print *,'RWData: ERROR number of data =! expected number'
            ierr=1
            return
          endif
        endif
c decode data
        call DCOMP6(nchar, cbuf, nsamp, idata, ierror)
        if (ierror.ne.0) then
          print *,'RWData: ERROR Data array not large enough'
          ierr=1
          return
        endif
        if (nsamp.ne.nexpect) then
          print *,'RWData: ERROR in retrieving correct number of samples'
          print *,'RWData: ERROR expected ',nexpect,' received ',nsamp
          ierr=1
          return
        endif
c remove second differences
        call REMDIF1(idata, nsamp)
        call REMDIF1(idata, nsamp)
c check checksum
        call sff_checksum(nsamp, idata, nchecksum)
        if (ichecksum.ne.nchecksum) then
          print *,'RWData: ERROR in checksum'
          print *,'RWData: ERROR read ',ichecksum,' calculated ',nchecksum
          ierr=1
          return
        endif
        ierr=0
      else
c 
c writing
c
c calculate checksum
        call sff_checksum(nsamp, idata, nchecksum)
c second differences
        call DIF1(idata, nsamp)
        call DIF1(idata, nsamp)
c do encoding
        nchar=maxbuf
        call CMPRS6(nsamp, idata, nchar, cbuf, ierror)
        if (ierror.ne.0) then
          print *,'RWData: ERROR character buffer not large enough'
          ierr=1
          return
        endif
c test for multiple of 80 characters
        if ((nchar-int(nchar/80)*80).ne.0) then
          print *,'RWData: ERROR not multiple of 80 characters'
          ierr=1
          return
        endif
c write data
        write(lu,'(a5,1x,i10,1x,e16.6,1x,a)', err=97) 'DAST ',nchar,ampfac,code
        write(lu,'(a)', err=97) wid2line
        write(lu,'(a)', err=97) 'DAT2'
        do iline=1,int(nchar/80)
          do i=1,80
            line(i:i)=cbuf(80*(iline-1)+i)
          enddo
          write(lu,'(a)') line(1:80)
        enddo
        write(lu,'(a4,1x,i8)') 'CHK2',nchecksum
      endif
      ierr=0
      return
c error conditions
   99 print *,'RWData: ERROR reading file'
      ierr=1
      return
   98 print *,'RWData: ERROR reading file - unexpected end of file'
      ierr=1
      return
   97 print *,'RWData: ERROR writing file'
      ierr=1
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_WData(lu,wid2line,nsamp,idata,ampfac,code)
c
c  Writes data block including DAST, WID2, DAT2, data and CHK2
c  assumes that WID2 line has already been prepared
c  Input:
c    lu:        File unit
c    wid2line:  WID2 line
c    idata:     integer data
c    nsamp:     number of samples
c    code:      character code describing optional blocks
c                 appended after data block (currently F and I)
c
      integer lu, nsamp, idata(nsamp)
      character wid2line*132, code*(*)
      real ampfac
cE
      integer ierr
c
      call sff_RWData(lu, wid2line, code, ampfac, nsamp, idata,
     &  .FALSE., ierr)
c original code does not support ierr parameter 
c so be ruthless
      if (ierr.ne.0) then
        print *,'WData: ERROR when calling RWData...'
        stop
      endif
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RWid2(lu,wid2line,nsamp,tanf,dt,nchar,ampfac,code,ierr)
c
c  Reads the DAST and WID2-lines only
c  Input: lu
c  Output
c    nchar:    number of bytes written
c    ampfac:   amplification factor
c    code:     info about following optional blocks 
c    wid2line: WID2 line
c
      integer lu, ierr, nsamp, nchar
      character wid2line*(*), code*(*)
      real ampfac, tanf, dt
cE
      character dast*5
      character time*12, wid2*132
      integer sff_GetN, lenwid2, hour, minute
      real sff_GetDt, second
c
      ierr=0
      lenwid2=len(wid2line)
      if(lenwid2.lt.132) then
        print *,'RWid2 WARNING: Length of wid2line < 132'
        print *,'RWid2 WARNING: Provided length: ',lenwid2
        print *,'RWid2 WARNING: wid2line will be truncated'
      endif
c
      read(lu,'(a5,1x,i10,1x,e16.6,1x,a)',end=101) dast,nchar,ampfac,code
      if(dast.ne.'DAST ') then
        write(0,'(a)') 'RWid2 ERROR: Data block does not begin with DAST'
        ierr = 1
        return
      endif
      read(lu,'(a)') wid2
      wid2line = wid2(1:min(lenwid2,132))
      read(lu,'(1x)')
      nsamp = sff_GetN(wid2)
      dt = sff_GetDt(wid2)
      call sff_GetTime(wid2,time)
      read(time(1:2),'(i2)') hour
      read(time(4:5),'(i2)') minute
      read(time(7:12),'(f6.3)') second
      tanf = hour*3600. +minute*60. +second
      goto 99
c
 101  ierr = 1
      write(0,'(a)') 'RWid2 ERROR: Dast line not read successfully'
 99   return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RData(lu,wid2line,nsamp,tanf,dt,idata,ampfac,code,ierr)
c
c  Reads a data block including DAST, WID2, DAT2, data and CHK2
c  Input:
c    lu           logical file unit
c    nsamp        dimension of array idata
c  Output
c    wid2line:    WID2 line
c    idata:       integer data
c    ampfac:      amplification factor
c    nsamp:       number of samples read
c    tanf:        start time in second after midnight
c    dt:          sampling interval in sec
c    code:        character code describing optional blocks
c                   appended after data block (currently F and I)
c
      integer lu, nsamp, idata(nsamp), ierr
      character wid2line*(*), code*(*)
      real ampfac, dt, tanf
cE
      character time*12, wid2*132
      integer sff_GetN, hour, minute, lenwid2
      real sff_GetDt, second
c
      ierr=0
      lenwid2=len(wid2line)
      if(lenwid2.lt.132) then
        print *,'RData WARNING: Length of wid2line < 132'
        print *,'RData WARNING: Provided length: ',lenwid2
        print *,'RData WARNING: wid2line will be truncated'
      endif
c
      call sff_RWData(lu, wid2, code, ampfac, nsamp, idata, 
     &  .TRUE., ierr)
      if (ierr.ne.0) then
        print *,'RData: ERROR when calling RWData'
        ierr=1
        return
      endif
c now extract information from WID2 line
      wid2line = wid2(1:min(lenwid2,132))
      nsamp = sff_GetN(wid2)
      dt = sff_GetDt(wid2)
      call sff_GetTime(wid2,time)
      read(time(1:2),'(i2)') hour
      read(time(4:5),'(i2)') minute
      read(time(7:12),'(f6.3)') second
      tanf = hour*3600. +minute*60. +sec ond
      ierr = 0
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_RData2(lu,nsamp,idata,cbuf,nchar,ierr)
c
c  Reads a data block from DAT2 to CHK2
c  Input: 
c      lu:     File unit
c      nsamp:  array dimension = expected number of samples
c      nchar:  dimension of work space cbuf
c  Output:
c      idata:  integer data
c      nsamp:  number of samples received
c      ierr:   =0 if no error occured
c  Work space:
c      cbuf:   work space of size ibufsize
c
      integer lu, ierr, nsamp, idata(nsamp), nchar
      character cbuf(nchar)*1
cE
      character line*80
      integer i, j, ichecksum, nchecksum, nmax, ierror
c
c     Now read in data according to the format:
c
      ierr=0
      j=0
 11   read(lu,'(a)') line
        if(line(1:5).eq.'CHK2 ') goto 12
        j=j+1
        if ((80*j).gt.nchar) then
          print *,'RData2: ERROR character buffer not large enough'
          ierr=1
          return
        endif
        do 10 i=1,80
 10       cbuf(80*(j-1)+i)=line(i:i)
      goto 11
 12   read(line(5:),*) ichecksum
      if(nchar.ne.j*80) then
        write(0,'(a)') 'RData ERROR: Number of data != expected number'
        ierr =1
        return
      endif
      nchar=j*80
      nmax=nsamp
      call DCOMP6(nchar,cbuf,nsamp,idata,ierror)
      if(ierror.eq.-1) then
        write(6,*)'RData ERROR: Data array not large enough !'
        ierr = 1
        return
      endif
      if(nsamp.ne.nmax)then
        write(0,*)'RData ERROR in retrieving correct number of samples !'
        write(0,*)'Expected ',nmax,', received ',nsamp
        ierr = 1
        return
      endif
c     remove second differences
      call REMDIF1(idata,nsamp)
      call REMDIF1(idata,nsamp)
c
      call sff_checksum(nsamp, idata, nchecksum)
      if(nchecksum.ne.ichecksum)then
        write(0,*)'RData: ERROR in checksum'
        write(0,*)'Read checksum       =',ichecksum
        write(0,*)'Calculated checksum =',nchecksum
        ierr = 1
        return
      endif
      ierr = 0
      return
      end
cD
c--------------------------------------------------------------------------
      subroutine sff_SkipData(lu,code,last,ierr)
c
c  Skip a data block including DAST, WID2 DAT2, data, CHK2 and optionally
c  appended Free and Info blocks. Close file if last data block
c  is read.
c
c  major changes:
c    22/11/96   T.F.   added variables code and last to parameter list
c                      (is needed to decide whether there will
c                       follow another datablock or not)
c
      integer lu,ierr
      logical last
      character code*(*)
cE
      character dast*5
      integer nchar, numline, i
      real ampfac
      
      last=.true.
      ierr=0
      read(lu,'(a5,1x,i10,1x,e16.6,1x,a)') dast,nchar,ampfac,code
      if(dast.ne.'DAST ') then
        write(0,'(a)') 'SkipData ERROR: Data block does not begin with DAST'
        ierr = 1
        return
      endif
      read(lu,'(1x)',err=102)
      read(lu,'(1x)',err=102)
      if (nchar.ne.(-1)) then
        numline = nchar/80
        do 10 i=1,numline
10        read(lu,'(1x)', err=102)
        read(lu,'(1x)', err=102)
      else
11      read(lu, '(a5)') dast
        if (dast.ne.'CHK2 ') goto 11
      endif
      i=1
 101  if(code(i:i).ne.' ') then
        if(code(i:i).eq.'F') call sff_SkipFree(lu,ierr)
        if(code(i:i).eq.'I') read(lu,'(1x)', err=98, end=97)
        if(code(i:i).eq.'D') last=.false.
        i=i+1
        goto 101
      endif
      if (last) close(lu, err=99)
      ierr = 0
      return
 102  ierr = 1
      write(0,'(a)') 'SkipData ERROR: Skipping of data unsuccessful'
      return
   99 print *,'sff_SkipData: ERROR closing file'
      ierr=1
      return
   98 print *,'sff_SkipData: ERROR skipping INFO line'
      ierr=1
      return
   97 print *,'sff_SkipData: ERROR skipping INFO line - unexpected end of file'
      ierr=1
      return
      end
cD
c--------------------------------------------------------------------------
      subroutine sff_PrepWid2(nsamp, samprat, station, year, month, day,
     &  hour, minute, comp, auxid, instyp, second, calib, calper,
     &  hang, vang, wid2line, ierr)
c
c  Prepare a WID2 line from scratch
c
c  The routine sets defaults for all variables set either to -1
c  for integers, -1. for floats or 'NSP' (not specified) for characters
c  If a component e.g. in the form LHZ is given hang and vang are
c  determined automatically
c
c  Reasonable values must be given for at least 
c      samprat:    sampling rate (= 1./(sampling interval))
c      nsamp:      number of smaples
c      station:    station name
c
c  Defaults are:
c      year:       0
c      month.      0
c      day:        0
c      hour:       0
c      minute:     0
c      second:     10.0
c      comp:       NSP
c      auxid:      NSP     
c      instyp:     NSP
c      calib:      1.
c      calper:     1.
c      hang:       -1.
c      vang:       90.
c 
c  Returns  wid2line
c
c  major changes:
c    22/11/96   T.F.   changed format of calib to e10.2 as defined by GSE2.0
c
      integer nsamp, year, month, day, hour, minute
      character comp*(*), auxid*(*), station*(*), instyp*(*)
      real samprat, second, calib, calper, hang, vang
      character wid2line*(*)
      integer ierr
cE
      integer dyear, dmonth, dday, dhour, dminute
      character dcomp*3, dauxid*4, dstation*5, dinstyp*6
      real dsecond, dcalib, dcalper, dhang, dvang
c
      ierr=0
      dyear=year
      dmonth=month
      dday=day
      dhour=hour
      dminute=minute
      dsecond=second
      dcalib=calib
      dcalper=calper
      dhang=hang
      dvang=vang
      dstation=station(1:5)
c
c  defaults
c
      if(year.eq.-1) dyear=0
      if(month.eq.-1) dmonth=0
      if(day.eq.-1) dday=0
      if(hour.eq.-1) dhour=0
      if(minute.eq.-1) dminute=0
      if(comp(1:3).eq.'NSP') then
        dcomp='NSP'
      else
        dcomp=comp(1:min(len(comp),3))
      endif
      if(auxid(1:3).eq.'NSP') then
        dauxid=' NSP'
      else
        dauxid=auxid(1:min(len(auxid),4))
      endif
      if(instyp(1:3).eq.'NSP') then
        dinstyp='   NSP'
      else
        dinstyp=instyp(1:min(len(instyp),6))
      endif
      if(calib .eq. -1.) dcalib=1.0
      if(calper .eq. -1.) dcalper=1.0
      if(vang .eq. -1.) dvang=90.
      if(second .eq. -1.) dsecond=10.0
      if(comp.ne.'NSP') then
        dhang=-1.0
        if(comp(3:3).eq.'N') dhang=0.
        if(comp(3:3).eq.'E') dhang=90.
        dvang=0.
        if(comp(3:3).eq.'N' .or. comp(3:3).eq.'E') dvang=90.
      endif
c
200   format(5x,i4.4,2(1h/,i2.2),1x,2(i2.2,1h:),f6.3,1x,a5,1x,a3,1x,
     &  a4,1x,a3,1x,i8,1x,f11.6,1x,e10.2,1x,f7.3,1x,a6,1x,f5.1,1x,f4.1)
      write(wid2line,200)
     2  dyear, dmonth, dday, dhour, dminute, dsecond, dstation, dcomp, 
     3  dauxid, 'CM6', nsamp, samprat, dcalib, dcalper, dinstyp,
     4  dhang, dvang
      if (wid2line(23:23).eq.' ') wid2line(23:23)='0'
      if (wid2line(24:24).eq.' ') wid2line(24:24)='0'
      wid2line(1:4)='WID2'
c
      return
      end
cD
c======================================================================
c  Utilities to extract info from wid2line
cD
c---------------------------------------------------------------------------
      subroutine sff_GetDate(wid2line,date)
c
c  extract date (yyyy/mm/dd)
c
      character wid2line*132, date*(*)
cE
      date = wid2line(6:15)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetTime(wid2line, time)
c
c  extract time (hh:mm:ss.sss)
c
      character wid2line*132, time*(*)
cE
      time = wid2line(17:28)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetStation(wid2line, sta)
c
c  extract station name (a5)
c
      character wid2line*132, sta*(*)
cE
      sta = wid2line(30:34)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetChannel(wid2line, channel)
c
c  extract channel name (a3)
c
      character wid2line*132, channel*(*)
cE
      channel = wid2line(36:38)
      return
      end
cD
c---------------------------------------------------------------------------
      integer function sff_GetN(wid2line)
c
c  extract number of samples
c
      character wid2line*132
cE
      integer n
c 
      read(wid2line(49:56),'(i8)') n
      sff_GetN = n
      return
      end
cD
c---------------------------------------------------------------------------
      real function sff_GetDt(wid2line)
c
c  extract sampling interval
c
      character wid2line*132
cE
      real dt
c 
      read(wid2line(58:68),'(f11.6)') dt
      sff_GetDt = 1./dt
      return
      end
cD
c----------------------------------------------------------------------
      subroutine sff_TrimLen(string,ntrim)
c
c  give length of a string excluding trailing blanks
c  Input:
c        string:  String to be trimmed
c  Output:
c        ntrim:   length of string excluding trailing blanks
c
      integer ntrim
      character string*(*)
cE
      do 10 ntrim=len(string),1,-1
 10     if(string(ntrim:ntrim).ne.' ') return
      ntrim = 1
      return
      end
c 
c
cD
c----------------------------------------------------------------------
      subroutine sff_close(lu, ierr)
c
c  Calling this subroutine instead of the Fortran close functions
c  provides interface compatibility to libfapidxx
c
c  Input:
c        lu:      Fortran file unit
c  Output:
c        ierr:    error status (ok: ierr=0)
c
      integer lu, ierr
cE
      close(lu, err=99)
      ierr=0
      goto 98
 99   ierr=1
 98   return
      end
c 
c ----- END OF stuff.f -----
