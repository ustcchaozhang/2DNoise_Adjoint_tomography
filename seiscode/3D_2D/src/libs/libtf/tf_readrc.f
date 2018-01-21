c this is <tf_readrc.f>
c------------------------------------------------------------------------------
c
c $Id$
c
c Copyright (c) 2000 by Thomas Forbriger (IfG Stuttgart)
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
c extract information from a structured runtime-configuration file
c
c This tool provides more than one entry point. You will have to open a new
c config file using the entry point 'tf_openrc'. 
c
c REVISIONS and CHANGES
c    06/02/00   V1.0   Thomas Forbriger
c    10/02/00   V1.1   added include command
c    11/02/00   V1.2   added error verbosity 
c    12/02/00   V1.3   added chapter output to tf_inforc
c    25/02/00   V1.4   - added beverbose to static list
c                      - now uses stack of parameters
c    17/12/07   V1.5   initialize internal variable to satisfy gfortran
c
c==============================================================================
cS
c
      subroutine tf_readrc(sar, ardim, nel, ierr)
c
c declare parameters
      integer ardim, nel, ierr
      character*(*) sar(ardim)
c 
c parameters used by other entry points
      integer inlumin, inlumax, mlen
      character*(*) indelim
      character*(*) name, chapter
c
c This subroutine reads one input line from the rc-file and possibly splits it
c into its components defined by the character sequence delim. By default the
c delimiting sequence is set to the ' ' (blank or space) character. Whitespace
c is stripped of all character strings and they are written do the output
c array 'sar'. If there are more substrings delimited by the delimiting
c character seqeunce the last element in 'sar' holds the rest of the input
c string (including the delimiting character sequences). 
c
c All characters following a hash ('#') symbol (including the symbol) are
c treated as comments and are removed from the input stream. Empty lines are
c not returned to the caller.
c
c A line containing 'include FILENAME' (after comments removed) will include
c the contents of file FILENAME as if they would appear in config file itself.
c There are abitrary many levels (well, limited by your operating systems
c ability to keep more than one file open) of includes. See the 'inlumin' and
c 'inlumax' parameters to the entry point 'tf_openrc'.
c
c Lines of the from '[any string]' (after comments removed) will be treated as
c chapter openings. The current chapter to be read is defined during
c 'tf_openrc'. All other chapters will be ignored by 'tf_readrc'.
c
c input:
c   ardim:  dimension of the output array 'sar'
c output:
c   sar:    contents of the read line
c   nel:    number of elements retrieved by splitting the line due to the
c           delimiting character sequence
c   ierr:   0: executed successfully
c           1: file is closed (last line was read)
c           2: the include depth range (maximum depth was defined by
c              lumin/lumax during open) is exceeded - file was closed.
c           3: a read error occured - file was closed
c           4: no file open
c           5: empty chapter opening - file was closed
c           6: at least one of the elements (after splitting) was too long for
c              the length of 'sar'-elements - file was NOT closed
c           7: invalid include filename
c 
cE
c declare local variables
      character tf_readrc_id*(*)
      parameter (tf_readrc_id=
     & '$Id$')
c 
      integer i, j
      integer maxlen, maxdelim
      parameter (maxlen=240, maxdelim=5)
      integer tfstr_trimbeg
      integer tfstr_trimlen
      integer first, last
      logical hot, flag
      character*(maxlen) line
c declare controlling stack
c   istack: index of current stack entry (0...mstack)
c           a value of zero means that there is no file open
c   iset:   index of stack entry to apply settings (1...mstack)
      integer mstack
      parameter(mstack=5)
      integer istack, iset
      integer lu(mstack), lumin(mstack), lumax(mstack)
      integer ndelim(mstack), chlen(mstack)
      logical inchapter(mstack), checkchapters(mstack), beverbose(mstack)
      character*(maxdelim) delim(mstack)
      character*(maxlen) thischapter(mstack)
      save lu, lumin, lumax, ndelim, delim, thischapter, chlen
      save inchapter, checkchapters, beverbose
c defaults
      data delim(1) /' '/
      data ndelim(1)/1/
      data beverbose(1)/.false./
      data istack/0/
      data iset/1/
c
c------------------------------------------------------------------------------
c go
c to satisfy gfortran:
      last=0
      first=0
      j=0
      i=0
      if (istack.lt.1) then
        ierr=4
        if (beverbose(iset)) print '("ERROR (tf_readrc): ",a)',
     &                       'no file is open!'
        return
      endif
c 
      hot=.true.
c
c ============================================
c start line reading loop
c (cycle until a valid entry was found or EOF)
c ============================================
c
      do while(hot)
        read(lu(istack), '(a)', err=98, end=97) line
c
c strip off comments and whitespace
c ---------------------------------
        first=tfstr_trimbeg(line)
        last=index(line, '#')
        if (last.eq.0) then
          last=tfstr_trimlen(line)
        else
          last=tfstr_trimlen(line(1:last-1))
        endif
c 
c empty line?
c -----------
        if (last.lt.first) go to 95
c 
c check for include command
c -------------------------
        if (line(first:min(last,first+7)).eq.'include ') then
          first=first+6+tfstr_trimbeg(line(first+7:last))
c is there a filename?
          if (first.gt.last) then
            ierr=7
            go to 96
          endif
          i=index(line(first:last),' ')
          if (i.ne.0) last=first-2+i
c          print *,'DEBUG include: >',line(first:last),'<',first,last
c is there a new file-unit available?
          if (lu(istack).lt.lumax(istack)) then
            lu(istack)=lu(istack)+1
            open(lu(istack), file=line(first:last), status='old', err=94)
            go to 95
c open error (jump-label)
   94       continue
            ierr=7
            go to 96
          else
            ierr=2
            go to 96
          endif
        endif
c 
c check for new chapter
c ---------------------
        if (checkchapters(istack)) then
          if ((line(first:first).eq.'[').and.
     &        (line(last:last).eq.']')) then
            if ((first+1).eq.last) then
              ierr=5
              go to 96
            endif
            if (line(first+1:last-1).eq.
     &          thischapter(istack)(1:chlen(istack))) then
              inchapter(istack)=.true.
            else
              inchapter(istack)=.false.
            endif
          else
            if (inchapter(istack)) hot=.false.
          endif
        else
          hot=.false.
        endif
        go to 95
c 
c handle end of file 
c ------------------
c (jump-label for read-command)
   97   continue
        if (lu(istack).gt.lumin(istack)) then
          close(lu(istack))
          lu(istack)=lu(istack)-1
        else
          ierr=1
          nel=0
          go to 96
        endif
c
   95   continue
      enddo
c
c ================================================================
c end loop
c (reaching this point there is a valid entry in line(first:last))
c ================================================================
c
c split the line
c --------------
      ierr=0
      hot=.true.
      j=1
c loop until all word are seperated or the all array elements are filled
      do while(hot)
c last array element?
        if (j.eq.ardim) then
          hot=.false.
          sar(j)=line(first:last)
          nel=j
          if (len(sar(j)).lt.len(line(first:last))) ierr=6
        else
c there are more array elements available
          i=index(line(first:last), delim(istack)(1:ndelim(istack)))
c last input element?
          if (i.eq.0) then
            hot=.false.
            sar(j)=line(first:last)
            nel=j
            if (len(sar(j)).lt.len(line(first:last))) ierr=6
          else
c there are more input elements
            i=first+i-2
            sar(j)=line(first:i)
            if (len(sar(j)).lt.len(line(first:i))) ierr=6
            i=i+ndelim(istack)+1
            first=tfstr_trimbeg(line(i:last))+i-1
            if (first.gt.last) then
              nel=j
              hot=.false.
            endif
            j=j+1
          endif
        endif
      enddo
      if (beverbose(iset)) then
        if (ierr.eq.6) print '("ERROR (tf_readrc): ",a)',
     &                       'at least one element was truncated!'
      endif
c that's it
      return
c
c ---------------------------------
c
c non-normal exit
c ---------------
c read error (jump-label for read-command)
   98 continue
      ierr=3
c close all files
   96 continue
      do i=lu(istack),lumin(istack),-1
        close(i)
      enddo
      lu(istack)=-2
      lumin(istack)=-1
      lumax(istack)=-1
      istack=istack-1
      iset=max(1,istack)
      if (beverbose(iset)) then
        if (ierr.eq.2) print '("ERROR (tf_readrc): ",a,/2x,2(a,i5))',
     &                       'the include depth range is exceeded!',
     &                       'lumin: ',lumin(istack),'lumax: ',lumax(istack)
        if (ierr.eq.3) print '("ERROR (tf_readrc): ",a)',
     &                       'read error!'
        if (ierr.eq.5) print '("ERROR (tf_readrc): ",a)',
     &                       'empty chapter opening!'
        if (ierr.eq.7) print '("ERROR (tf_readrc): ",a,/2x,a)',
     &                       'invalid include filename:',line(first:last)
      endif
      return
c 
c======================================================================
cS
c 
      entry tf_closerc(ierr)
c 
c close open file(s)
c
c returns:
c integer ierr=0:       executed successfully
c integer ierr=1:       no rc-file was open
c
cE
      if (istack.lt.1) then
        ierr=1
      else
        do i=lu(istack),lumin(istack),-1
          close(i)
        enddo
        ierr=0
      endif
      lu(istack)=-2
      lumin(istack)=-1
      lumax(istack)=-1
      istack=istack-1
      iset=max(1,istack)
      if (beverbose(iset)) then
        if (ierr.eq.1) print '("ERROR (tf_closerc): ",a)',
     &                       'no file is open!'
      endif
      return
c 
c======================================================================
cS
c 
      entry tf_openrc(name, chapter, inlumin, inlumax, ierr)
c 
c open new config file
c
c input:
c   character*(*) name:    name of config file to be read
c   character*(*) chapter: name of config file to be read
c   integer inlumin:       minimum logical file unit number to be used
c   integer inlumax:       maximum logical file unit number to be used
c output:
c   integer ierr:          0: executed successfully
c                          1: could not open file
c                          2: invalid lumin/lumax range
c                          3: invalid chapter
c                          4: chapter too long
c                          5: stack is full
c 
c The lumin/lumax range is used to facilitate the include command. With each
c include level the logical file unit number in use is increased by one.
c inlumax must be greater than or equal inlumin and inlumin must be greater or
c equal ten. File units within the lumin/lumax range may not be used outside
c the tf_readrc-tool while a config file is held open.
c
c The chapter to be read by the following sequence of tf_readrc-calls is
c defined by the string 'chapter'. It may be at most as long as the maximum
c length of input lines (see tf_lenrc) minus two. If the provided chapter name
c is empty, the whole file will be read (including chapter openings).
c
c These routines have an internal stack of limited size. You may use a cascade
c of rc file reading subroutines. Each openrc will switch to the next place on
c the stack, while each close (also implicit close within readrc) will return
c to the previous stack entry.
c
cE
      if ((inlumin.lt.10).or.(inlumin.gt.inlumax)) then
        ierr=2
      elseif (istack.eq.mstack) then
        ierr=5
      else
c next stack entry
        istack                =istack+1
c get defaults
        inchapter(istack)     =.false.
        checkchapters(istack) =.true.
        delim(istack)         =delim(iset)
        ndelim(istack)        =ndelim(iset)
        beverbose(istack)     =beverbose(iset)
c 
        iset                  =istack
c 
        i=tfstr_trimbeg(chapter)
        j=tfstr_trimlen(chapter)
        chlen(istack)=j-i+1
        if (chlen(istack).gt.(len(thischapter(istack))-2)) then
          ierr=4
        else
          if (i.ge.j) then
            checkchapters(istack)=.false.
          else
            thischapter(istack)=chapter(i:j)
          endif
          open(inlumin, file=name, status='old', err=99)
          lumin(istack)=inlumin
          lumax(istack)=inlumax
          lu(istack)=inlumin
          ierr=0
          return
   99     continue
          ierr=1
        endif
      endif
      if (beverbose(iset)) then
        if (ierr.eq.1) print '("ERROR (tf_openrc): ",a,/2x,a)',
     &                       'could not open file:',
     &                       name(1:index(name,' ')-1)
        if (ierr.eq.2) print '("ERROR (tf_openrc): ",a,i5,"/",i5,/2x,a)',
     &                       'invalid inlumin/inlumax range:',
     &                       inlumin,inlumax,
     &                       'must be: 10 <= inlumin <= inlumax'
        if (ierr.eq.3) print '("ERROR (tf_openrc): ",a,/2x,">",a,"<")',
     &                       'invalid chapter name:',
     &                       chapter
        if (ierr.eq.4) print '("ERROR (tf_openrc): ",a,/2x,">",a,"<",2x,a,i4)',
     &                       'chapter name is too long:', chapter(i:j),
     &                       'allowed maximum length: ',
     &                       len(thischapter(istack))-2
        if (ierr.eq.5) print '("ERROR (tf_openrc): ",a,1x,i2,1x,a)',
     &                       'exceeded stack size of',mstack,'entries'
      endif
      return
c 
c======================================================================
cS
c 
      entry tf_delimrc(indelim, ierr)
c 
c change delimiter
c
c input:
c   character*(*) indelim:    delimiter (first character of string) to be set
c output
c   integer ierr=0:           executed successfully
c   integer ierr=1:           indelim is too short (delim is not changed)
c   integer ierr=2:           indelim is too long (delim is not changed)
c
cE
      if (len(indelim).lt.1) then
        ierr=1
      elseif (len(indelim).gt.len(delim(iset))) then
        ierr=2
      else
        delim(iset)=indelim
        ndelim(iset)=len(indelim)
        ierr=0
      endif
      if (beverbose(iset)) then
        if (ierr.eq.1) print '("ERROR (tf_delimrc): ",a,/2x,">",a,"<")',
     &                       'indelim is incorrect (too short):'
        if (ierr.eq.2) print 
     &                '("ERROR (tf_delimrc): ",a,/2x,">",a,"<",/2x,a,i4)',
     &                       'indelim is too long:', indelim,
     &                       'allowed maximum length: ',len(delim(iset))
      endif
      return
c 
c======================================================================
cS
c 
      entry tf_inforc
c 
c report revision Id and maximum length of input line
c report whether file is open and report chapter
c
cE
      print *,'tf_readrc-tools:'
      print *,tf_readrc_id
      print *,'maximum length of input line:         ',maxlen
      print *,'stack size:                           ',mstack
      print *,'stack entry:                          ',istack
      print *,'option changes are applied to stack#  ',iset
      print *,'maximum length of delimting sequence: ',maxdelim
      print *,'current delimiter:                   "',
     &           delim(iset)(1:ndelim(iset)),'"'
      if (istack.gt.0) then
        print *,'rc-file is open'
        if (checkchapters(istack)) then
          print *,'working on chapter [',
     &      thischapter(istack)(1:chlen(istack)),']'
        else
          print *,'no chapter sectioning - full file will be read'
        endif
      endif
      return
c 
c======================================================================
cS
c 
      entry tf_lenrc(mlen)
c 
c return maximum length of input line
c
c integer mlen:   maximum length of input line
c
cE
      mlen=maxlen
      return
c 
c======================================================================
cS
c 
      entry tf_lendelimrc(mlen)
c 
c return maximum length of delimiting character sequence line
c
c integer mlen:   maximum length of delimiting character sequence
c
cE
      mlen=maxdelim
      return
c 
c======================================================================
cS
c 
      entry tf_verboserc(flag)
c 
c sets the verbosity for error/warning conditions
c
c logical flag: .true.:  print error/warning message to stdout
c               .false.: do NOT print error/warning message to stdout
c
cE
      beverbose(iset)=flag
      return
      end
c
c ----- END OF tf_readrc.f -----
