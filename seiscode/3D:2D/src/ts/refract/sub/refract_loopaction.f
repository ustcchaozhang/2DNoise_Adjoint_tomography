c this is <refract_loopaction.f>
c------------------------------------------------------------------------------
c
c 01/05/98 by Thomas Forbriger (IfG Stuttgart)
c
c cursor interaction
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
c this file containes several subroutines that manage the relevant key and
c cursor interaction
c
c we build up some kind of menu system with loopaction being on the top level
c sublevel routines may change the behaviour of loopaction
c
c any routine should provide help on key <?>
c
c REVISIONS and CHANGES
c    01/05/98   V1.0   Thomas Forbriger
c    04/07/98   V2.0   go menu
c    18/08/98   V2.1   include code for single picks
c    18/11/98   V2.2   find source location of air-coupled sound wave
c    17/06/03   V2.3   new option plflag_tracenum
c    09/09/04   V2.4   new option plflag_tracename
c
c==============================================================================
c 
      subroutine help_loopaction
c
c some hints on how to use loopactions
c
      print *,' '
      print *,'top level keys:'
      print *,'---------------'
      print *,' '
      print *,'  general'
      print *,'    <?>        help'
      print *,'    <x|X|q|Q>  exit (and right mouse button)'
      print *,'    <space>    replot'
      print *,'    <R>        reset plot'
      print *,' '
      print *,'  navigating'
      print *,'    <<>        shift viewport left'
      print *,'    <>>        shift viewport right'
      print *,'    <^>        shift viewport up'
      print *,'    <.>        shift viewport down'
      print *,'    <l>        set viewport left margin'
      print *,'    <r>        set viewport right margin'
      print *,'    <t>        set viewport top margin'
      print *,'    <b>        set viewport bottom margin'
      print *,'    <z>        zoom'
      print *,'    <o>        set original (full) size'
      print *,' '
      print *,'  scaling'
      print *,'    <+>/<->    increase/decrease'
      print *,'                 exponent value'
      print *,'                 or amplitude'
      print *,'                 or clipping level'
      print *,' '
      print *,'  picking'
      print *,'    <p|a|A>    add pick (and left mouse button)'
      print *,'    <d|D>      delete pick (and right mouse button)'
      print *,' '
      print *,'  traveltime reduction'
      print *,'    <T>        toggle on/off'
      print *,'    <V>        select reduction velocity'
      print *,' '
      print *,'  checks'
      print *,'    <S>        find source location of air-coupled sound wave'
      print *,' '
      print *,'  menus'
      print *,'    <s>        select scaling mode'
      print *,'    <h>        do hardcopy'
      print *,'    <P>        select picking mode'
      print *,'    <f>        toggle flags'
      print *,'    <e>        set/toggle plot elements'
      print *,'    <k>        keyboard input'
      print *,'    <F>        read/write from/to files'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine loopaction
c
c top level cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    exit               <x> <X> <q> <Q>
c    replot             <space>
c    reset              <R>
c    picking            <p> <a> <A> <d> <D>
c    navigating         <<> <>> <.> <^> <l> <r> <t> <b> <z> <o>
c    scaling            <+> <->
c    tarveltime red.    <T> <V>
c    checks             <S>
c    menus
c      scaling mode     <s>
c      hardcopy         <h>
c      picking mode     <P>
c      flag toggle      <f>
c      plot elements    <e>
c      keyboard input   <k>
c      read/write       <F>
c
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_seipar.inc'
c
cE
      integer pgband
      real xcoor, ycoor, xref, yref, help ,help2
      character*70 info
      real realtime
      integer index
      character*1 button
      save xcoor, ycoor
c 
      character*80 pgq_item, pgq_value
      integer pgq_length
      integer pgopen
c 
      character*30 hint_zoom, hint_vred, hint_soundwave
      parameter(hint_zoom='select zoom window')
      parameter(hint_vred='define reduction velocity')
      parameter(hint_soundwave='pick air-coupled onsets')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,pg_pmmode,'t')
      call refract_message(.true.,pg_pickmode,'b')
      call pgframeact
c 
c ok - now read the last warning and forget it!
      plstring_lastwarn=' '
c 
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c 
c general actions
c ---------------
c
c XxQq
c
        if (index('XxqQ', button).gt.0) then
          if (verbose) print *,'exiting...'
          flag_pick=.false.
c
c ?
c
        elseif (button.eq.'?') then
          call help_loopaction
c
c space
c
        elseif (button.eq.' ') then
          if (verbose) print *,'replot...'
          flag_replot=.true.
c
c R
c
        elseif (button.eq.'R') then
          if (verbose) print *,'reopen device and reset values...'
          flag_replot=.true.
          pgq_item='DEV/TYPE' 
          call pgqinf(pgq_item, pgq_value, pgq_length)
          call pgclos
          pg_maindevice=pgopen(pgq_value(1:pgq_length))
          if (pg_maindevice.le.0) stop 
     &      'ERROR (loopaction): opening device'
          call setscale
          call mpcfactors
          call setfullrange
c 
c navigation
c ----------
c
c b
c
        elseif (button.eq.'b') then
          if (ycoor.lt.tov_rmax) then
            tov_rmin=ycoor
            flag_replot=.true.
          endif
c
c t
c
        elseif (button.eq.'t') then
          if (ycoor.gt.tov_rmin) then
            tov_rmax=ycoor
            flag_replot=.true.
          endif
c
c l
c
        elseif (button.eq.'l') then
          if (xcoor.lt.tov_tmax) then
            tov_tmin=xcoor
            flag_replot=.true.
          endif
c
c r
c
        elseif (button.eq.'r') then
          if (xcoor.gt.tov_tmin) then
            tov_tmax=xcoor
            flag_replot=.true.
          endif
c
c z
c
        elseif (button.eq.'z') then
          if (verbose) print *,'zoom...'
          xref=xcoor
          yref=ycoor
          call refract_message(.true.,hint_zoom,'h')
          if (pgband(2,1,xref,yref,xcoor,ycoor,button).eq.1) then 
            if (button.eq.'A') then
              flag_replot=.true.
              tov_rmin=min(yref,ycoor)
              tov_rmax=max(yref,ycoor)
              tov_tmin=min(xref,xcoor)
              tov_tmax=max(xref,xcoor)
            endif
          else
            call refract_warning('WARNING (loopaction): pgband failed')
          endif
          call refract_message(.false.,hint_zoom,'h')
c
c <
c
        elseif (button.eq.'<') then
          if (verbose) print *,'shift left...'
          help=(tov_tmax-tov_tmin)/3.
          tov_tmax=tov_tmax-help
          tov_tmin=tov_tmin-help
          flag_replot=.true.
c
c >
c
        elseif (button.eq.'>') then
          if (verbose) print *,'shift right...'
          help=(tov_tmax-tov_tmin)/3.
          tov_tmax=tov_tmax+help
          tov_tmin=tov_tmin+help
          flag_replot=.true.
c
c ^
c
        elseif (button.eq.'^') then
          if (verbose) print *,'shift up...'
          help=(tov_rmax-tov_rmin)/3.
          tov_rmax=tov_rmax+help
          tov_rmin=tov_rmin+help
          flag_replot=.true.
c
c .
c
        elseif (button.eq.'.') then
          if (verbose) print *,'shift down...'
          help=(tov_rmax-tov_rmin)/3.
          tov_rmax=tov_rmax-help
          tov_rmin=tov_rmin-help
          flag_replot=.true.
c
c o
c
        elseif (button.eq.'o') then
          if (verbose) print *,'set full size...'
          call setfullrange
          flag_replot=.true.
c 
c checks
c ------
c
c S
c
        elseif (button.eq.'S') then
          if (verbose) print *,'pick onsets of air-coupled sound wave...'
          call refract_message(.true.,hint_soundwave,'h')
          xref=xcoor
          yref=ycoor
          if (pgband(1,1,xref,yref,xcoor,ycoor,button).eq.1) then 
            if (button.eq.'A') then
              help=(ycoor-yref)/(realtime(xcoor,ycoor)-realtime(xref,yref))
              if (help.gt.0.) then
                help2=ycoor-(xcoor*help)
                write(info, 50) help2
                call refract_warning(info)
              else
                call refract_warning(
     &            'WARNING (loopaction): velocity is not positive')
              endif
            endif
          else
            call refract_warning('WARNING (loopaction): pgband failed')
          endif
          call refract_message(.false.,hint_soundwave,'h')
c 
c traveltime reduction
c --------------------
c
c V
c
        elseif (button.eq.'V') then
          if (verbose) print *,'select travel time reduction velocity...'
          call refract_message(.true.,hint_vred,'h')
          xref=xcoor
          yref=ycoor
          if (pgband(1,1,xref,yref,xcoor,ycoor,button).eq.1) then 
            if (button.eq.'A') then
              help=(ycoor-yref)/(realtime(xcoor,ycoor)-realtime(xref,yref))
              if (help.gt.0.) then
                plpar_vred=help*1.e-3
                flag_replot=.true.
                plflag_reduce=.true.
              else
                call refract_warning(
     &            'WARNING (loopaction): Vred is not positive')
              endif
            endif
          else
            call refract_warning('WARNING (loopaction): pgband failed')
          endif
          call refract_message(.false.,hint_vred,'h')
c
c T
c
        elseif (button.eq.'T') then
          if (verbose) print *,'toggle travel time reduction...'
          plflag_reduce=(.not.(plflag_reduce))
          flag_replot=.true.
c 
c picking
c -------
c
c a,A,p
c
        elseif (index('aAp',button).gt.0) then
          if (verbose) print *,'add pick...'
          call addpick(xcoor,ycoor)
c
c d,D
c
        elseif (index('dD',button).gt.0) then
          if (verbose) print *,'delete pick...'
          call delpick(xcoor,ycoor)
c 
c scaling
c -------
c
c -
c
        elseif (button.eq.'-') then
          if (plpar_pmmode.eq.1) then
            if (verbose) print *,'reduce amplitude scale...'
            plpar_amp=0.7*plpar_amp
            call mpcfactors
            flag_replot=.true.
          elseif (plpar_pmmode.eq.2) then
            if (verbose) print *,'reduce clipping level...'
            plpar_clip=0.7*plpar_clip
            call mpcfactors
            flag_replot=.true.
          elseif (plpar_pmmode.eq.3) then
            if (verbose) print *,'reduce scaling exponent...'
            plpar_expo=max(0.,plpar_expo-0.08)
            call mpcfactors
            flag_replot=.true.
          else
            stop 'ERROR (loopaction): unknown pmmode'
          endif
c
c +
c
        elseif (button.eq.'+') then
          if (plpar_pmmode.eq.1) then
            if (verbose) print *,'increase amplitude scale...'
            plpar_amp=1.3*plpar_amp
            call mpcfactors
            flag_replot=.true.
          elseif (plpar_pmmode.eq.2) then
            if (verbose) print *,'increase clipping level...'
            plpar_clip=1.3*plpar_clip
            call mpcfactors
            flag_replot=.true.
          elseif (plpar_pmmode.eq.3) then
            if (verbose) print *,'increase scaling exponent...'
            plpar_expo=plpar_expo+.1
            call mpcfactors
            flag_replot=.true.
          else
            stop 'ERROR (loopaction): unknown pmmode'
          endif
c 
c menus
c -----
c
c s
c
        elseif(button.eq.'s') then
          call menu_scaling
c
c f
c
        elseif(button.eq.'f') then
          call menu_flags
c
c h
c
        elseif(button.eq.'h') then
          call menu_hardcopy
c
c k
c
        elseif(button.eq.'k') then
          call menu_keyboard
c
c e
c
        elseif(button.eq.'e') then
          call menu_elements
c
c F
c
        elseif(button.eq.'F') then
          call menu_readwrite
c
c P
c
        elseif(button.eq.'P') then
          call menu_pick
c 
        else
          call refract_warning(
     &      'WARNING (loopaction): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (loopaction): cursor interaction failed')
        flag_pick=.false.
      endif
c 
      return
   50 format('source location of air-coupled sound wave: ',f10.3,'m')
      end
c
c----------------------------------------------------------------------
cS
c
      subroutine menu_scaling
c
c scaling menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    +/- mode amp       <a>
c    +/- mode clip      <c>
c    +/- mode expo      <e>
c    scaling mode 1     <1>
c    scaling mode 2     <2>
c    scaling mode 3     <3>
c
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_strings.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*30 hint
      parameter(hint='scaling: <?,a,c,e,1,2,3>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.false.,pg_pmmode,'t')
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_scaling
c
c a
c
        elseif (button.eq.'a') then
          if (verbose) print *,'<+> and <-> use amplitude...'
          pg_pmmode=string_pmamp
          plpar_pmmode=1
c
c c
c
        elseif (button.eq.'c') then
          if (verbose) print *,'<+> and <-> use clipping level...'
          pg_pmmode=string_pmclip
          plpar_pmmode=2
c
c e
c
        elseif (button.eq.'e') then
          if (verbose) print *,'<+> and <-> use exponent...'
          pg_pmmode=string_pmexpo
          plpar_pmmode=3
c
c 1
c
        elseif (button.eq.'1') then
          if (verbose) print *,'scale traces individually...'
          plpar_mode=1
          flag_replot=.true.
          call mpcfactors
c
c 2
c
        elseif (button.eq.'2') then
          if (verbose) print *,'scale traces all together...'
          plpar_mode=2
          flag_replot=.true.
          call mpcfactors
c
c 3
c
        elseif (button.eq.'3') then
          if (verbose) print *,'scale traces per file...'
          plpar_mode=3
          flag_replot=.true.
          call mpcfactors
        else
          call refract_warning(
     &      'WARNING (menu_scaling): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_scaling): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_hardcopy
c
c some hints on how to use menu hardcopy
c
      print *,' '
      print *,'hardcopy menu keys:'
      print *,'-------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <p>        portrait color postscript to pgp.ps'
      print *,'    <l>        landscape color postscript to pgp.ps'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_hardcopy
c
c hardcopy menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    portrait hardcopy  <p>
c    landscape hardcopy <l>
c 
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*30 hint
      parameter(hint='hardcopy: <?,p,l>')
      integer hc_device
      integer pgopen
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_hardcopy
c
c p
c
        elseif (button.eq.'p') then
          if (verbose) print *,'color hardcopy to file pgp.ps...'
          hc_device=pgopen('pgp.ps/vcps')
          if (hc_device.le.0) then
            call refract_warning(
     &        'WARNING (loopaction): could not open hardcopy device')
          else
            call doplot(hc_device)
            call pgclos
            call pgslct(pg_maindevice)
          endif
c
c l
c
        elseif (button.eq.'l') then
          if (verbose) print *,'color hardcopy to file pgp.ps...'
          hc_device=pgopen('pgp.ps/cps')
          if (hc_device.le.0) then
            call refract_warning(
     &        'WARNING (loopaction): could not open hardcopy device')
          else
            call doplot(hc_device)
            call pgclos
            call pgslct(pg_maindevice)
          endif
        else
          call refract_warning(
     &      'WARNING (menu_hardcopy): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_hardcopy): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_scaling
c
c some hints on how to use menu scaling
c
      print *,' '
      print *,'scaling menu keys:'
      print *,'------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <a>        change amplitude with <+> and <->'
      print *,'    <c>        change clipping level with <+> and <->'
      print *,'    <e>        change exponent with <+> and <->'
      print *,'    <1>        scale all traces individually'
      print *,'    <2>        scale all traces to one reference'
      print *,'    <3>        scale traces to a reference within each file'
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine help_menu_pick
c
c some hints on how to use menu pick
c
      print *,' '
      print *,'pick menu keys:'
      print *,'---------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <t>        pick travel time curve'
      print *,'    <a>        pick arrival time at trace'
      print *,'    <1>        pick time taper 1'
      print *,'    <2>        pick time taper 2'
      print *,'    <3>        pick time taper 3'
      print *,'    <4>        pick time taper 4'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_pick
c
c pick menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    traveltimes        <t>
c    taper 1            <1>
c    taper 2            <2>
c    taper 3            <3>
c    taper 4            <4>
c    trace arrivals     <a>
c
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_strings.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*30 hint
      parameter(hint='pick: <?,t,a,1,2,3,4>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.false.,pg_pickmode,'b')
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_pick
c
c t
c
        elseif (button.eq.'t') then
          if (verbose) print *,'pick traveltimes...'
          call refract_eraseallpicks
          pg_pickmode=string_picktt
          plpar_pickmode=1
          call refract_plotallpicks
c
c 1
c
        elseif (button.eq.'1') then
          if (verbose) print *,'pick taper 1...'
          call refract_eraseallpicks
          pg_pickmode=string_pickt1
          plpar_pickmode=2
          call refract_plotallpicks
c
c 2
c
        elseif (button.eq.'2') then
          if (verbose) print *,'pick taper 2...'
          call refract_eraseallpicks
          pg_pickmode=string_pickt2
          plpar_pickmode=3
          call refract_plotallpicks
c
c 3
c
        elseif (button.eq.'3') then
          if (verbose) print *,'pick taper 3...'
          call refract_eraseallpicks
          pg_pickmode=string_pickt3
          plpar_pickmode=4
          call refract_plotallpicks
c
c 4
c
        elseif (button.eq.'4') then
          if (verbose) print *,'pick taper 4...'
          call refract_eraseallpicks
          pg_pickmode=string_pickt4
          plpar_pickmode=5
          call refract_plotallpicks
c
c a
c
        elseif (button.eq.'a') then
          if (verbose) print *,'pick trace related arrival times...'
          call refract_eraseallpicks
          pg_pickmode=string_pickta
          plpar_pickmode=6
          call refract_plotallpicks
        else
          call refract_warning(
     &      'WARNING (menu_pick): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_pick): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_flags
c
c some hints on how to use menu flags
c
      print *,' '
      print *,'flag toggle menu keys:'
      print *,'----------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <i>        reverse sign'
      print *,'    <a>        remove average'
      print *,'    <v>        variable area plot'
      print *,'    <b>        plot these nice bubbles'
      print *,'    <c>        use different colors'
      print *,'    <l>        use different linestyles'
      print *,'    <P>        use different styles for picks'
      print *,'    <S>        use different styles for seismograms'
      print *,'    <T>        use different styles for traveltimes'
      print *,'    <g>        plot grid'
      print *,'    <V>        be verbose'
      print *,'    <D>        give debug information'
      print *,'    <#>        label each trace with its number'
      print *,'    <N>        label each trace with its station name'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_flags
c
c toggle flag menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    invers             <i>
c    remove average     <a>
c    variable area      <v>
c    plot bubbles       <b>
c    use colors         <c>
c    use styled picks   <P>
c    use styled seismo  <S>
c    use styled trav.ti <T>
c    use linestyles     <l>
c    plot grid          <g>
c    verbose            <V>
c    debug              <D>
c    label trace number <#>
c
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*40 hint
      parameter(hint='flags: <?,i,a,v,b,c,l,P,S,T,g,V,D,#,N>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_flags
c
c i
c
        elseif (button.eq.'i') then
          if (verbose) print *,'toggled reverse sign flag...'
          plflag_invers=(.not.(plflag_invers))
          flag_replot=.true.
c
c a
c
        elseif (button.eq.'a') then
          if (verbose) print *,'toggled remove average flag...'
          plpar_remav=(.not.(plpar_remav))
          call mpcfactors
          flag_replot=.true.
c
c v
c
        elseif (button.eq.'v') then
          if (verbose) print *,'toggled variable area flag...'
          plflag_vara=(.not.(plflag_vara))
          flag_replot=.true.
c
c b
c
        elseif (button.eq.'b') then
          if (verbose) print *,'toggled nice bubbles flag...'
          plflag_bubbles=(.not.(plflag_bubbles))
          flag_replot=.true.
c
c g
c
        elseif (button.eq.'g') then
          if (verbose) print *,'toggled grid flag...'
          plflag_grid=(.not.(plflag_grid))
          flag_replot=.true.
c
c S
c
        elseif (button.eq.'S') then
          if (verbose) print *,'toggled pick color style flag...'
          plflag_seistyle=(.not.(plflag_seistyle))
          flag_replot=.true.
c
c T
c
        elseif (button.eq.'T') then
          if (verbose) print *,'toggled pick color style flag...'
          plflag_ttstyle=(.not.(plflag_ttstyle))
          flag_replot=.true.
c
c P
c
        elseif (button.eq.'P') then
          if (verbose) print *,'toggled pick color style flag...'
          plflag_picol=(.not.(plflag_picol))
          flag_replot=.true.
c
c c
c
        elseif (button.eq.'c') then
          if (verbose) print *,'toggled color style flag...'
          plflag_color=(.not.(plflag_color))
          flag_replot=.true.
c
c l
c
        elseif (button.eq.'l') then
          if (verbose) print *,'toggled line style flag...'
          plflag_linestyle=(.not.(plflag_linestyle))
          flag_replot=.true.
c
c V
c
        elseif (button.eq.'V') then
          if (verbose) print *,'toggled verbose flag...'
          verbose=(.not.(verbose))
c
c D
c
        elseif (button.eq.'D') then
          if (verbose) print *,'toggled debug flag...'
          debug=(.not.(debug))
c
c N
c
        elseif (button.eq.'N') then
          if (verbose) print *,'toggled station name label flag...'
          plflag_tracename=(.not.(plflag_tracename))
          flag_replot=.true.
c
c #
c
        elseif (button.eq.'#') then
          if (verbose) print *,'toggled trace number label flag...'
          plflag_tracenum=(.not.(plflag_tracenum))
          flag_replot=.true.
        else
          call refract_warning(
     &      'WARNING (menu_flags): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_flags): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_keyboard
c
c some hints on how to use menu keyboard
c
      print *,' '
      print *,'keyboard menu keys:'
      print *,'-------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <p>        enter parameters'
      print *,'    <f>        toggle flags'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_keyboard
c
c keyboard menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    flags              <f>
c    parameters         <p>
c 
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*30 hint
      parameter(hint='keyboard: <?,f,p>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_keyboard
c 
c p
c
        elseif (button.eq.'p') then
          call refract_setpara
          flag_replot=.true.
          call mpcfactors
c 
c f
c
        elseif (button.eq.'f') then
          call refract_setflags
          flag_replot=.true.
          call mpcfactors
        else
          call refract_warning(
     &      'WARNING (menu_keyboard): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_keyboard): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_elements
c
c some hints on how to use menu elements
c
      print *,' '
      print *,'elements menu keys:'
      print *,'-------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <M>        evaluate model and place model box'
      print *,'    <m>        plot model box'
      print *,'    <S>        plot individual scale boxes'
      print *,'    <f>        tell filenames'
      print *,'    <v>        tell program version'
      print *,'    <a>        tell annotations'
      print *,'    <s>        plot scales'
      print *,'    <d>        plot seismogram data'
      print *,'    <t>        plot synthetic traveltimes'
      print *,'    <p>        plot picks'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_elements
c
c element menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    model box          <m>
c    evaluate model     <M>
c    indiv. scales      <S>
c    filenames          <f>
c    version            <v>
c    annotations        <a>
c    scales             <s>
c    data               <d>
c    synth. travelt.    <t>
c    picks              <p>
c 
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_model.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*35 hint
      parameter(hint='elements: <?,M,m,S,f,v,a,s,d,t,p>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_elements
c
c M
c
        elseif (button.eq.'M') then
          if (.not.(mod_valid)) then
            if (verbose) print *,'evaluate model...'
            call evalmod
          endif
          mod_boxx=xcoor
          mod_boxy=ycoor
          elem_modbox=.true.
          flag_replot=mod_valid
c
c
c m
c
        elseif (button.eq.'m') then
          if (verbose) print *,'toggled model box flag...'
          elem_modbox=(.not.(elem_modbox))
          flag_replot=.true.
c
c S
c
        elseif (button.eq.'S') then
          if (verbose) print *,'toggled individual scales flag...'
          plflag_subscale=(.not.(plflag_subscale))
          flag_replot=.true.
c
c f
c
        elseif (button.eq.'f') then
          if (verbose) print *,'toggled filenames flag...'
          elem_filenames=(.not.(elem_filenames))
          flag_replot=.true.
c
c v
c
        elseif (button.eq.'v') then
          if (verbose) print *,'toggled version flag...'
          elem_version=(.not.(elem_version))
          flag_replot=.true.
c
c a
c
        elseif (button.eq.'a') then
          if (verbose) print *,'toggled annotations flag...'
          elem_annot=(.not.(elem_annot))
          flag_replot=.true.
c
c s
c
        elseif (button.eq.'s') then
          if (verbose) print *,'toggled scales flag...'
          elem_scales=(.not.(elem_scales))
          flag_replot=.true.
c
c d
c
        elseif (button.eq.'d') then
          if (verbose) print *,'toggled seismogram data flag...'
          elem_data=(.not.(elem_data))
          flag_replot=.true.
c
c t
c
        elseif (button.eq.'t') then
          if (verbose) print *,'toggled synthetic traveltimes flag...'
          elem_syntt=(.not.(elem_syntt))
          flag_replot=.true.
c
c p
c
        elseif (button.eq.'p') then
          if (verbose) print *,'toggled picks flag...'
          elem_picks=(.not.(elem_picks))
          flag_replot=.true.
        else
          call refract_warning(
     &      'WARNING (menu_elements): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_elements): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine help_menu_readwrite
c
c some hints on how to use menu elements
c
      print *,' '
      print *,'read/write menu keys:'
      print *,'---------------------'
      print *,' '
      print *,'    <?>        help'
      print *,'    <t>        write tapers to file'
      print *,'    <T>        read tapers from file'
      print *,'    <m>        write model to file'
      print *,'    <M>        read model from file'
      print *,'               (depth and velocity will be read!)'
      print *,'    <p>        write traveltimes to file'
      print *,'    <P>        read traveltimes from file'
      print *,'    <a>        write picks for trace arrivals to file'
      print *,'    <A>        read picks for trace arrivals from file'
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine menu_readwrite
c
c element menu cursor interaction
c
c reserved keys are:
c
c    help               <?>
c    write tapers       <t>
c    read tapers        <T>
c    write model        <m>
c    read model         <M>
c    write traveltimes  <p>
c    read traveltimes   <P>
c    write single picks <a>
c    read single picks  <A>
c 
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_model.inc'
c
cE
      integer pgband
      real xcoor, ycoor
      character*1 button
      save xcoor, ycoor
      character*35 hint
      parameter(hint='read/write: <?,t,T,m,M,p,P,a,A>')
c
      flag_replot=.false.
      call pgslct(pg_maindevice)
      call refract_message(.true.,hint,'h')
      call pgframeact
      if (pgband(7, 0, 0., 0., xcoor, ycoor, button).eq.1) then
c
c ?
c
        if (button.eq.'?') then
          call help_menu_readwrite
c
c t
c
        elseif (button.eq.'t') then
          if (verbose) print *,'write picks to file...'
          call refract_writetappicks
c
c T
c
        elseif (button.eq.'T') then
          if (verbose) print *,'read picks from file...'
          call refract_readtappicks
c
c m
c
        elseif (button.eq.'m') then
          if (verbose) print *,'write model to file...'
          call refract_writemodel
c
c M
c
        elseif (button.eq.'M') then
          if (verbose) print *,'read model from file...'
          call refract_readmodel
c
c p
c
        elseif (button.eq.'p') then
          if (verbose) print *,'write traveltimes to file...'
          call refract_writettpicks(1)
c
c P
c
        elseif (button.eq.'P') then
          if (verbose) print *,'read traveltimes from file...'
          call refract_readttpicks(1)
c
c s
c
        elseif (button.eq.'a') then
          if (verbose) print *,'write single picks to file...'
          call refract_writettpicks(6)
c
c S
c
        elseif (button.eq.'A') then
          if (verbose) print *,'read single picks from file...'
          call refract_readttpicks(6)
        else
          call refract_warning(
     &      'WARNING (menu_readwrite): unknown action: '//button)
        endif
      else
        call refract_warning(
     &      'WARNING (menu_readwrite): cursor interaction failed')
        flag_pick=.false.
      endif
      call refract_message(.false.,hint,'h')
c 
      return
      end
c
c ----- END OF refract_loopaction.f -----
