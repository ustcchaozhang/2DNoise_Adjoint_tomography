c  this is file <gpt.f>    
c======================================================================
c
c Copyright (c) 1995 by Thomas Forbriger (IfG Stuttgart)
c 
c  GPT.F - Library
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
c  24/8/1995 Thomas Forbriger
c  9/9/1995 added gptps
c  19/9/1995   V1.2  added landscape modes
c  02/10/1995  V1.3  added setdevice and showdevice
c
cS
c  these are output-subroutines to prepare different devices for
c  gnuplot-output
c
c  call the subroutines like this:
c     CALL DEVNAME(FILENO)
c     where FILENO is the number of the output file to which you
c     are writing gnuplot-commands
c
c  routine:    suppoerted device:
c  --------    ------------------
c  gptdefault  set all defaults
c  gptx11      X-Windows
c  gptlp       postscript to printer lpr
c  gptps       postscript to file gpt.ps
c  gpttek      Tektronix-Terminal
c  gptlpls     postscript to printer lpr landscape
c  gptpsls     postscript to file gpt.ps landscape
c
c======================================================================
c show devices
      subroutine showdevices
cE
      print *,'You may choose one of the following devices:'
      print *,'  x11      X-Windows'
      print *,'  lp       postscript to printer lpr'
      print *,'  ps       postscript to file gpt.ps'
      print *,'  tek      Tektronix-Terminal'
      print *,'  lpls     postscript to printer lpr landscape'
      print *,'  psls     postscript to file gpt.ps landscape'
      return
      end
c----------------------------------------------------------------------
cS
c set devices
      subroutine setdevice(fileno, device)
      integer fileno
      character*10 device
cE
      print *,'setting output to device ',device
      call gptdefault(fileno)
      if (device.eq.'x11') then
        call gptx11(fileno)
      else if (device.eq.'lp') then
        call gptlp(fileno)
      else if (device.eq.'ps') then
        call gptps(fileno)
      else if (device.eq.'tek') then
        call gpttek(fileno)
      else if (device.eq.'lpls') then
        call gptlpls(fileno)
      else if (device.eq.'psls') then
        call gptpsls(fileno)
      else
        stop 'ERROR: unknown device-type'
      endif
      return
      end
c----------------------------------------------------------------------
c  SUB 100
c
cS
c  subroutine for X-Window plots
      subroutine gptx11(fino)
      integer fino
cE
      write(fino, 101)
  101 format('set terminal X11',
     &       /'set output')
      return
      end
c----------------------------------------------------------------------
c  SUB 200
c
cS
c  subroutine to set defaults
      subroutine gptdefault(fino)
      integer fino
cE
      write(fino, 201)
      write(fino, 202)
      write(fino, 203)
      write(fino, 204)
      write(fino, 205)
  201 format('set terminal x11 ',
     &       /'set output ',
     &       /'set noclip points',
     &       /'set clip one',
     &       /'set noclip two',
     &       /'set border',
     &       /'set boxwidth',
     &       /'set dummy x,y',
     &       /'set format x "%g"',
     &       /'set format y "%g"',
     &       /'set format z "%g"')
  202 format( 'set nogrid',
     &       /'set key',
     &       /'set nolabel',
     &       /'set noarrow',
     &       /'set nologscale',
     &       /'set offsets 0, 0, 0, 0',
     &       /'set nopolar',
     &       /'set angles radians',
     &       /'set noparametric',
     &       /'set view 60, 30, 1, 1',
     &       /'set samples 100, 100',
     &       /'set isosamples 10, 10')
  203 format( 'set surface',
     &       /'set nocontour',
     &       /'set clabel',
     &       /'set nohidden3d',
     &       /'set cntrparam order 4',
     &       /'set cntrparam linear',
     &       /'set cntrparam levels auto 5',
     &       /'set cntrparam points 5',
     &       /'set size 1,1',
     &       /'set data style points',
     &       /'set function style lines')
  204 format( 'set xzeroaxis',
     &       /'set yzeroaxis',
     &       /'set tics in',
     &       /'set ticslevel 0.5',
     &       /'set xtics',
     &       /'set ytics',
     &       /'set ztics',
     &       /'set title "" 0,0',
     &       /'set notime',
     &       /'set rrange [-0 : 10]',
     &       /'set trange [-5 : 5]',
     &       /'set urange [-5 : 5]',
     &       /'set vrange [-5 : 5]')
  205 format( 'set xlabel "" 0,0',
     &       /'set xrange [-10 : 10]',
     &       /'set ylabel "" 0,0',
     &       /'set yrange [-10 : 10]',
     &       /'set zlabel "" 0,0',
     &       /'set zrange [-10 : 10]',
     &       /'set autoscale r',
     &       /'set autoscale t',
     &       /'set autoscale xy',
     &       /'set autoscale z',
     &       /'set zero 1e-08')
      return
      end
c----------------------------------------------------------------------
c  SUB 300
c
cS
c  subroutine for Postscript-Plots
      subroutine gptlp(fino)
      integer fino
cE
      write(fino, 301)
  301 format('set terminal postscript portrait',
     &       /'set output "| lpr"',
     &       /'set size 0.721, 1.387')
      return
      end
c----------------------------------------------------------------------
c  SUB 400
c
cS
c  subroutine for Tektronix-Plots
      subroutine gpttek(fino)
      integer fino
cE
      write(fino, 401)
  401 format('set terminal tek40xx',
     &       /'set output')
      return
      end
c----------------------------------------------------------------------
c  SUB 500
c
cS
c  subroutine for Postscript-Plot to File
      subroutine gptps(fino)
      integer fino
cE
      write(fino, 501)
  501 format('set terminal postscript portrait',
     &       /'set output "gpt.ps"',
     &       /'set size 0.721, 1.387')
      return
      end
c----------------------------------------------------------------------
c  SUB 500
c
cS
c  subroutine for Postscript-Plot to File landscape
      subroutine gptpsls(fino)
      integer fino
cE
      write(fino, 501)
  501 format('set terminal postscript landscape',
     &       /'set output "gpt.ps"')
      return
      end
c----------------------------------------------------------------------
c  SUB 500
c
cS
c  subroutine for Postscript-Plot to File
      subroutine gptlpls(fino)
      integer fino
cE
      write(fino, 501)
  501 format('set terminal postscript landscape',
     &       /'set output "| lpr"')
      return
      end
c
