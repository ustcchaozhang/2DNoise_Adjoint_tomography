c this is <refract_usage.f>
c------------------------------------------------------------------------------
cS
c
c 24/05/2000 by Thomas Forbriger (IfG Stuttgart)
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
c print command line hints
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger (thof)
c    25/05/2000   V1.1   quick info was missing
c    11/01/2001   V1.2   introduced hypocenter option
c    17/06/2003   V1.3   introduced plflag_tracenum
c    09/09/2004   V1.4   introduced plflag_tracenum
c    26/11/2010   V1.5   provide help regarding file formats
c    17.12.2010   V1.6   format selection was missing in quick help
c    12/11/2012   V1.7   new option -Sn
c    20/11/2012   V1.8   new file specific flag v:
c                        new option -Eu
c                        new file specific flag b:
c    24/10/2013   V1.9   - new option -So
c                        - new option -S3
c    18/11/2013   V1.10  new option -TF
c    21/03/2014 thof:    new option -TR
c    07/12/2015 htof:    new option -SN
c
c==============================================================================
c
      subroutine refract_usage_short(version)
c
c declare parameters
      character*(*) version
cE
c------------------------------------------------------------------------------
c go
      print *,version
      print *,'Usage: refract [-D] [-d dev] [-v] [-p] [-ty format]'
      print *,'               [-Tt title] [-Tx label] [-Ty label] [-Tm T|F]'
      print *,'               [-Ta label] [-Tl T|F] [-Ts factor] [-TM t,x]'
      print *,'               [-TL n] [-TF] [-TR]'
      print *,'               [-Lw width] [-Lc] [-Lm max] [-Lt width]'
      print *,'               [-Cb r,g,b] [-Cf r,g,b] [-CW] [-Cc] [-Cm max]'
      print *,'               [-Eg] [-Ev] [-Eb] [-EP T|F] [-ES T|F] [-ET T|F]'
      print *,'               [-En] [-Es] [-Ew T|F] [-Et T|F] [-Ep T|F]'
      print *,'               [-Eu] [-Ef]'
      print *,'               [-Fp file] [-Fa file] [-Ft file] [-Fm file]'
      print *,'               [-Sx x1,x2] [-St t1,t2] [-Se exp] [-Sa lev]'
      print *,'               [-Sc lev] [-Sm mode] [-Sr vel] [-Si] [-SM]'
      print *,'               [-Sn] [-SR radius] [-SO minoff] [-Sh] [-So n]'
      print *,'               [-S3 min,max] [-SN min,max]'
      print *,'               [-C] [-L] [-R radius] [-O minoff]'
      print *,'          file [t:n,n-n] [o:s] [s:i,s,w] [n:name]'
      print *,'               [h:h,l,s] [r:r,g,b] [f:format] [v:f]'
      print *,'         [file ...]'
      print *,'   or: refract -help'
      print *,'   or: refract -xhelp'
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine refract_usage_formats(version)
c
c declare parameters
      character*(*) version
c
cE
c go
      call refract_usage_short(version)
      print *,' '
      call sff_help_details
c
      stop
      end
c 
cS
c----------------------------------------------------------------------
c
      subroutine refract_usage(version)
c
c declare parameters
      character*(*) version
c
cE
c declare local variables
      integer iargc
c
c------------------------------------------------------------------------------
c go
      call refract_usage_short(version)
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
c 
      print *,' '
      print *,'REFRACTion seismics - data interpretation'
      print *,'Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)'
      print *,' '
      call usage_purpose
      print *,' '
      print *,'-help        print online help to terminal'
      print *,'-xhelp       print online help on file formats to terminal'
      print *,' '
      print *,'-D           debug'
      print *,'-d dev       output device'
      print *,'-v           verbose'
      print *,'-p           interactive mode'
      print *,'-ty formatID format ID of data file type to be used'
      print *,' '
      print *,'Titles, labels, legends'
      print *,'-----------------------'
      print *,' '
      print *,'-Tt title      title string'
      print *,'-Tx label      x axis label'
      print *,'-Ty label      y axis label'
      print *,'-Ta label      replace mode setting annotations by ''label'' '
      print *,'-Tm T|F        print mode settings (T: yes; F: no)'
      print *,'-Tl T|F        print standard legend (T: yes; F: no)'
      print *,'-Ts factor     character height factor'
      print *,'-TM t,x        plot model box at time t and offset x'
      print *,'-TL n          do not use more than n lines for'
      print *,'               file name annotations'
      print *,'-TF            append filename to file label'
      print *,'-TR            reverse order of file legend'
      print *,' '
      print *,'Line options'
      print *,'------------'
      print *,' '
      print *,'-Lw width      standard line width'
      print *,'-Lc            use cycling line styles'
      print *,'-Lm max        maximum line style cycling index'
      print *,'-Lt width      width of synthetic traveltime curves'
      print *,' '
      print *,'Line options'
      print *,'------------'
      print *,' '
      print *,'-Cb r,g,b      background (index 0) RGB value'
      print *,'-Cf r,g,b      foreground (index 1) RGB value'
      print *,'-CW            swap color index 0 and 1 (black on white)'
      print *,'-Cc            use cycling line colors'
      print *,'-Cm max        maximum line color cycling index'
      print *,' '
      print *,'Elements and style'
      print *,'------------------'
      print *,' '
      print *,'-Eg            plot grid'
      print *,'-Ev            wiggle plot (variable area)' 
      print *,'-Eb            bubble plot'
      print *,'-EP T|F        cycle styles for picks (T:yes; F:no)'
      print *,'-ES T|F        cycle styles for waveforms (T:yes; F:no)'
      print *,'-ET T|F        cycle styles for traveltimes (T:yes; F:no)'
      print *,'-Ew T|F        plot waveforms (T:yes; F:no)'
      print *,'-Et T|F        plot synthetic traveltimes (T:yes; F:no)'
      print *,'-Ep T|F        plot picks (T:yes; F:no)'
      print *,'-En            label each trace with its number'
      print *,'-Es            label each trace with its station name'
      print *,'-Eu            plot scales for each trace'
      print *,'-Ef            plot frame enclosing view surface'
      print *,' '
      print *,'File reading'
      print *,'------------'
      print *,' '
      print *,'-Fp file       read traveltime polygon values from file'
      print *,'-Fa file       read arrival time pick values from file'
      print *,'-Ft file       read taper pick values from file'
      print *,'-Fm file       read model values from file'
      print *,' '
      print *,'Seismogram scaling'
      print *,'------------------'
      print *,' '
      print *,'-Sx x1,x2      display offset range [x1,x2]'
      print *,'-St t1,t2      display time range [t1,t2]'
      print *,'-Se exp        set scaling exponent (must not be negative)'
      print *,'-Sa lev        set scaling amplitude level'
      print *,'-Sc lev        set scaling clipping level'
      print *,'-Sm mode       set scaling mode'
      print *,'                 1: scale traces individually'
      print *,'                 2: scale all traces to first trace as reference'
      print *,'                 3: scale all traces to nearest offset'
      print *,'                    trace of each dataset as reference'
      print *,'-SN min,max    refer amplitude scaling to average'
      print *,'               amplitude in range from min to max.'
      print *,'-S3 min,max    equivalent to ''-Sm 3 -SN min,max'' '
      print *,'               provided for compatibility only'
      print *,'-Sr vel        set traveltime reduction velocity'
      print *,'-Sn            do not align offset shifted traces along'
      print *,'               slope of reduced times'
      print *,'-Si            invert polarity'
      print *,'-SM            remove average'
      print *,'-SR radius     offset scale will be given in degrees on'
      print *,'               a sphere with radius [km].'
      print *,'-SO minoff     set minimum offset difference for two'
      print *,'               different receiver locations'
      print *,'-Sh            offset is distance to hypocenter (rather than'
      print *,'               epicenter)'
      print *,'-So n          define ordinate scale:'
      print *,'               n=0: source to receiver offset (default)'
      print *,'               n=1: x1 coordinate'
      print *,'               n=2: x2 coordinate'
      print *,'               n=3: x3 coordinate'
      print *,'               in cases where ordinate scales other than'
      print *,'               source to receiver offset are used together'
      print *,'               with reduced time scales, the application'
      print *,'               of option -Sn is recommended.'
      print *,' '
      print *,'The following options are supported for backward compatibility:'
      print *,'---------------------------------------------------------------'
      print *,' '
      print *,'-C           colors'
      print *,'-L           line styles'
      print *,'-M mode      scaling mode'
      print *,'-e expo      scaling exponent'
      print *,'-R radius    offset scale will be given in degrees on'
      print *,'             a sphere with radius [km].'
      print *,'-O minoff    set minimum offset difference for two'
      print *,'             different receiver locations'
      print *,' '
      print *,'File specific options that may be given:'
      print *,'========================================'
      print *,' '
      print *,'Trace selection - t:'
      print *,'--------------------'
      print *,'  Each datafile name may be followed by a list of'
      print *,'  traces. This list selects a range of traces in'
      print *,'  the file which will be processed. The list may'
      print *,'  not contain blanks (which is the separator to the'
      print *,'  next filname). The traces will always be processed'
      print *,'  in the order they appear in the data file, not the.'
      print *,'  order in the list.'
      print *,' '
      print *,'  Examples:'
      print *,'    t:2           will select only trace 2'
      print *,'    t:4-6,2,4     will select traces 2, 4, 5 and 6'
      print *,'    t:9,8,10,14   will select traces 8, 9, 10 and 14'
      print *,' '
      print *,'Shift offsets - o:'
      print *,'------------------'
      print *,'  You may shift all offsets within the file by ''s'' meters:'
      print *,' '
      print *,'    o:s'
      print *,' '
      print *,'Plot style options - s:'
      print *,'-----------------------'
      print *,'  You may set the color index, line style and line width'
      print *,'  through a flag following the filename:'
      print *,' '
      print *,'    s:i,s,w'
      print *,' '
      print *,'  The integer values are:'
      print *,'  i:     color index'
      print *,'  s:     line style'
      print *,'  w:     line width'
      print *,' '
      print *,'Legend string - n:'
      print *,'------------------'
      print *,' '
      print *,'  You may set a legend string alternative to the filename'
      print *,'  through:'
      print *,' '
      print *,'    n:string'
      print *,' '
      print *,'RGB color - r:'
      print *,'--------------'
      print *,' '
      print *,'  You may set an explicit color to be used for that file:'
      print *,'  (r,g,b are real values giving the RGB color triple)'
      print *,' '
      print *,'    r:r,g,b'
      print *,' '
      print *,'HLS color - h:'
      print *,'--------------'
      print *,' '
      print *,'  You may set an explicit color to be used for that file:'
      print *,'  (h,l,s are real values giving the HLS color triple)'
      print *,' '
      print *,'    h:h,l,s'
      print *,' '
      print *,'Input file format - f:'
      print *,'----------------------'
      print *,' '
      print *,'  For each input file you can select a data fiel format'
      print *,'  from the list given below (at end of help text).'
      print *,' '
      print *,'    f:formatID'
      print *,' '
      print *,'Variable area plot - v:'
      print *,'-----------------------'
      print *,' '
      print *,'  Select variable area plot for this specific file.'
      print *,' '
      print *,'    v:f'
      print *,' '
      print *,'  f=0:  do not use variable area for this file'
      print *,'  f=1:  use variable area for this file'
      print *,'  else: use variable area if selected with global'
      print *,'        option -v'
      print *,' '
      print *,'Plot baseline for traces from this file - b:'
      print *,'--------------------------------------------'
      print *,' '
      print *,'    b:T|F'
      print *,' '
      print *,'Menus to be used in interactive picking mode:'
      print *,'============================================='
      print *,' '
      call help_loopaction
      call help_menu_scaling
      call help_menu_pick
      call help_menu_hardcopy
      call help_menu_flags
      call help_menu_keyboard
      call help_menu_elements
      call help_menu_readwrite
      print *,' '
      call pgp_showdevices
      print *,' '
      call sff_help_formats
      stop
c
      end
c
c----------------------------------------------------------------------
c
      subroutine usage_purpose
c
      print *,'Purpose'
      print *,'  - plots of seismic waveforms arranging the '
     &       ,'traces along an offset axis'
      print *,'    for waveform comparison'
      print *,'  - refracted wave travel-time inversion to '
     &       ,'subsurface model'
      print *,'  - picking of arrival times'
      print *,'  - definition of offset dependent waveform '
     &       ,'tapers'
      print *,' '
      print *,'Features'
      print *,'  - supports interactive as well as '
     &       ,'non-interactive plotting'
      print *,'  - graphics output is done through PGPLOT '
     &       ,'featuring a variety of devices and'
      print *,'    graphics formats (including Postscript, '
     &       ,'X11, Tektronix, PNG, GIF, etc)'
      print *,'  - supports a variety of input formats '
     &       ,'including SeismicUn*x and raw ASCII'
      print *,'    (through libdatrwxx)'
      print *,'  - appearance of plots can be controlled by '
     &       ,'many parameters'
      print *,'    (color, line width, etc)'
      print *,'  - plotting on velocity reduced time scale'
      print *,'  - comparing shallow seismic shot data with '
     &       ,'true amplitudes'
      print *,'  - display of synthetic arrival times of '
     &       ,'refracted and reflected waves'
      print *,'    together with waveforms'
      print *,'...'
      print *,'    …'
c
      return
      end
c
c ----- END OF refract_usage.f -----
