c this is <refract_settracevp.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c set viewport and world coordinate values for a specific trace
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
c    30/04/98   V1.0   Thomas Forbriger
c    03/07/98   V1.1   new clear concept - leave clipping to pgplot
c    04/07/98   V1.2   introduced traveltime reduction
c    12/11/12   V1.3   do not apply travel time reduction, if offset of
c                      seismograms is shifted intentionally
c    08/02/16   V1.4   introduce upper limit for world coordinate range
c                      in counts
c
c==============================================================================
cS
c
      logical function settracevp(i, reverse)

c
c set viewport and world coordinate values for trace i
c
c returns true in case there is at least one sample to be plot
c 
c reverse sign of scale if flag set
c
c values set are those required by calls
c   call pgsvp(trv_vpleft, trv_vpright, trv_vpbot, trv_vptop)
c   call pgswin(trv_tmin, trv_tmax, trv_vmin, trv_vmax)
c in subroutine pgtrace
c
c return values are passed through common block refract_trv which is
c defined in refract_seipar.inc
c 
      integer i
      logical reverse
      real limit, mpc, rmaxamp
      parameter(limit=1.e38)
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
c
cE
c
c ----------------------------------------------------------------------
c limit
c -----
c
c To prevent numerical overflow, we must set an upper limit for floating
c point single precision variables. Fortran 95 provides a function
c huge(), where
c
c   huge(1.0)=3.40282347E+38
c
c in a standard Intel environment with gfortran. Since this function
c might not be available for users relying on the code being compiled
c with a standard Fortran 77 compiler, I set the limit to 1.e38
c
c ----------------------------------------------------------------------
c 
c tov stands for total viewport
c trv stands for trace viewport
c 
c vppm (viewport per meter) is the scaling factor from "normalized device
c    coordinates" to "world coordinates" (the latter are meters)
c 
c all calculations will be carried out in the offset-domain and will be
c    converted to "world coordinates" in the last step
c 
c the result flag is used to check whether any part of the curve is
c visible within the global viewport
c 
c the baseline of the curve must be 0. or averagec
c
c ----------------------------------------------------------------------
c
c The function calculates the viewport coordinates for the rectangle on
c the graphics device display to which the given trace signal shall be
c plotted, the trace viewport. These viewport coordinates are:
c
c   left:   trv_vpleft        in normalized device coordinates
c   right:  trv_vpright       in normalized device coordinates
c   top:    trv_vptop         in normalized device coordinates
c   bottom: trv_vpbot         in normalized device coordinates
c
c The total viewport for the complete graphical display of traces is
c defined by the rectangle
c
c   left:   tov_vpleft        in normalized device coordinates
c   right:  tov_vpright       in normalized device coordinates
c   top:    tov_vptop         in normalized device coordinates
c   bottom: tov_vpbot         in normalized device coordinates
c
c The subroutine make sure that the trace viewport is inside the total
c viewport. 
c
c ----------------------------------------------------------------------
c
c The world coordinates for the total viewport are:
c
c   largest offset:     tov_rmax    in meters
c   smallest offset:    tov_rmin    in meters
c   largest time:       tov_tmax    in seconds
c   smallest time:      tov_tmin    in seconds
c
c With respect to these world coordinates in total viewport the world
c coordinates of the trace viewport are
c
c   largest offset:     trv_rtop    in meters
c   smallest offset:    trv_rbot    in meters
c   largest time:       trv_tright  in seconds
c   smallest time:      trv_tleft   in seconds
c
c ----------------------------------------------------------------------
c
c The actual world coordinates to be set to the window covering the
c trace viewport are:
c
c   left:         trv_tmin    in counts
c   right:        trv_tmax    in counts
c   top:          trv_vmax    in seconds
c   bottom:       trv_vmin    in seconds
c
c ----------------------------------------------------------------------
c
c The subroutine first sets the vertical extend and window in the offset
c domain and world coordinates in counts. In the second part it operates
c on horizontal coordinates and world coordinates in seconds.
c
c ----------------------------------------------------------------------
c
c 
      logical result
      real vppm, ttmin, ttmax, theoffset
      real realtime, plotoffset
c 
c ======================================================================
c
c 1. Operate on vertical coordinates, offset domain and world
c    coordinates in counts.
c ----------------------------------------------------------- 
c
c factor "world coordinates -> normalized device coordinates"
c derived from the total viewport
      vppm=(tov_vptop-tov_vpbot)/(tov_rmax-tov_rmin)
c 
c result will be true in case we expect any part of the curve to be visible
c within the trace viewport
      result=.true.
c
c a) Calculate raw offset range
c -----------------------------
c
c The offset range to be used for display of this trace is defined by
c the clipping margin to larger and smaller offsets, centred on the
c trace's offset.
c
c Calculate the raw offset range to be used, simplay based on trace's
c offset and clipping level:
      trv_rtop=roffset(i)+plpar_clip
      trv_rbot=roffset(i)-plpar_clip
c
c Check if this - maybe - already is outside global plot range
c
c will there anything be visible
      if ((trv_rtop.lt.tov_rmin).or.(trv_rbot.gt.tov_rmax))
     &  result=.false.
c 
      if (result) then
c 
c b) Adjust offset range
c ----------------------
c
c The offset range defined by the clipping margin might partly be
c outside the global offset range.
c
c respect clipping by total viewport
        trv_rtop=min(trv_rtop, tov_rmax)
        trv_rbot=max(trv_rbot, tov_rmin)
c
c trv_rtop and trv_rtop now specify the actual offset range in the
c window of the total graphical viewport to be used for the display of
c the current trace. If any sample value should fall outside this offset
c range, they shall now be displayed.
c
c c) Find corresponding viewport in device coordinates
c ----------------------------------------------------
c
c To select a plot area on the device surface, we have to use normalized
c device coordinates. 
c
c map to a viewport range
        trv_vpbot=(trv_rbot-tov_rmin)*vppm+tov_vpbot
        trv_vptop=(trv_rtop-tov_rmin)*vppm+tov_vpbot
c
c d) Find world coordinates for traces viewport in counts
c -------------------------------------------------------
c
c The smaller the mpc-factor, the smaller the wiggle amplitude in the
c display. This is adjusted by increasing the world coordinate range (in
c counts). If mpc approaches zero, the world coordinate range approaches
c infinity which results in numerical overflow.
c
c Value range with respect to trace offset
        rmaxamp=max(abs(trv_rtop-roffset(i)),
     &              abs(trv_rbot-roffset(i)))
c
c Find smallest mpc-factor acceptable
        mpc=max(trv_mpc(i),rmaxamp/(limit-abs(average(i))))
c 
c remap equivalent offset to seismogram counts
        if (reverse) then
          trv_vmax=-(trv_rtop-roffset(i))/mpc
          trv_vmin=-(trv_rbot-roffset(i))/mpc
        else
          trv_vmax=(trv_rtop-roffset(i))/mpc
          trv_vmin=(trv_rbot-roffset(i))/mpc
        endif
c 
c move trace within viewport in case we want to remove the average
        if (plpar_remav) then
          trv_vmax=trv_vmax+average(i)
          trv_vmin=trv_vmin+average(i)
        endif
c 
c check actual world coordinate range versus seismogram range
        if (reverse) then
          if ((trv_vmin.lt.minval(i)).or.(trv_vmax.gt.maxval(i))) 
     &      result=.false.
        else
          if ((trv_vmax.lt.minval(i)).or.(trv_vmin.gt.maxval(i))) 
     &      result=.false.
        endif
      endif
c
c 
c ======================================================================
c
c 2. Operate on horizontak coordinates, time domain and world
c    coordinates in seconds.
c ----------------------------------------------------------- 
c 
      if (result) then
c
c now check TIME SCALE
        ttmin=toffset(i)
        ttmax=toffset(i)+(nsamples(i)-1)*dt(i)
        theoffset=plotoffset(i)
        trv_tmin=realtime(tov_tmin, theoffset)
        trv_tmax=realtime(tov_tmax, theoffset)
        if ((ttmin.ge.trv_tmax).or.(ttmax.le.trv_tmin)) result=.false.
      endif
c
c set time scale if needed
      if (result) then
        trv_tleft=tov_tmin
        trv_tright=tov_tmax
        trv_vpleft=tov_vpleft
        trv_vpright=tov_vpright
      endif
c 
      settracevp=result
c     
      return
      end
c
c ----- END OF refract_settracevp.f -----
