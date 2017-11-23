c this is <gremlin_help.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c Here is some extra help on gremlin commands
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
c
c REVISIONS and CHANGES
c    25/03/98   V1.0   Thomas Forbriger
c    14/01/99   V1.1   removed link to old gremlin code
c    04/03/99   V1.2   file/mtt explanation
c    24/05/00   V1.3   - introduced reso option wgpd
c                      - introduced reso option wense
c    19/04/02   V1.4   introduced command swense
c    06/05/02   V1.5   introduced sqense and ssense
c    07/05/02   V1.6   new data weight routines
c    07/08/02   V1.7   - found old command npa
c                      - explain rms and err parameter in resolution analysis
c
c==============================================================================
c
      subroutine gremhelp_main
c
c give help for main menu
c
      call gremhelp_tit('main')
      call gremhelp_subtit('display data')
      print *,' dgr            display real green data in different modes'
      print *,' dtt            display travel time data'
      print *,' dgi index      plot green data from data array index'
      print *,' dti index      plot traveltime data from data array index'
      call gremhelp_subtit('display/edit model')
      print *,' dre            display reference model in a table'
      print *,' dwo            display working copy of model in a table'
      print *,' med            edit current model (model will be written to'
      print *,'                file edit.p.mod and will be read again)'
      print *,' dmo file       display basic control screen for model in "file"'
      print *,' dmi index      display named model of index'
      call gremhelp_subtit('set/display free parameters')
      print *,' spc            set free model parameters'
      print *,' imo mode       preset parameters for inversion modes'
      print *,'                (what you would do by "spc" normally)'
      print *,'     sgrad        shear wave gradient'
      print *,'     pvel         p-wave model'
      print *,' weight para    call mod_weight with "para" (modifies'
      print *,'                "spc" settings)'
      print *,' sano para      call par_sano with "para" (modifies'
      print *,'                "spc" settings)'
      print *,' dpc            display free parameter settings in a table'
      print *,'                similar to the "spc" mask'
      print *,' tpc            display free parameter settings in a table'
      call gremhelp_subtit('set/display inversion parameters')
      print *,' spa            set various parameters'
      print *,' dpa            display all parameter settings'
      print *,' weights th1,th2,f1,f2'
      print *,'                calculate misfit-dependend data weights'
      print *,'                you are invited to have a look in'
      print *,'                subroutine dat_mmweights to learn about'
      print *,'                the calculation rule ;-)'
      call gremhelp_subtit('control inversion')
      print *,' dda            display basic crontrol screen'
      print *,' opt lim,step   optimize model with extra break condition'
      print *,'     lim          stop when X2 reaches "lim"'
      print *,'     step         stop after a maximum os "step" iterations'
      print *,' ofi nu,lim,step,mode'
      print *,'                optimize at fixed "nu"'
      print *,'     lim          stop when X2 reaches "lim"'
      print *,'     step         stop after a maximum os "step" iterations'
      print *,'     mode         1: plot improvement'
      print *,'                  2: plot improvement, model and data'
      print *,' x2c numin,numax,npts'
      print *,'                plot X2 development'
      print *,'     numin        start with this nu'
      print *,'     numax        end with this nu'
      print *,'     npts         number of points to plot'
      print *,' lx2 numin,numax,npts'
      print *,'                plot X2 development for linearized problem'
      print *,'     numin        start with this nu'
      print *,'     numax        end with this nu'
      print *,'     npts         number of points to plot'
      print *,' tpa para       fetch new model for given "nu"'
      print *,'     para         parameter to calculate "nu" from'
      print *,'                  ''para'' is the value taken from'
      print *,'                  the x2c-abscissa.'
      print *,' npa para       like tpa, but calculate partial'
      print *,'                  derivatives first'
      call gremhelp_subtit('misc')
      print *,' verb           set verbosity modes'
      print *,' mon            switch monitor devices on/off'
      print *,' pgpar          set plot parameters'
      print *,' dpg            display pgplot parameters'
      print *,' dev device     change pgplot device'
      print *,' file           read/write various files'
      print *,' reso           linear resolution analysis'
c      print *,' old            enter old ancient gremlin code'
c      print *,'                (on your own risk)'
      print *,' term           use "term" to exit the program'
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine gremhelp_resan
c
c help on resolution analysis menu
c
      call gremhelp_tit('resolution analysis')
      print *,' '
      print *,' parder         rate partial derivatives'
      print *,' tpd ival       plot travel time partial derivatives for parameter ival'
      print *,' qtpd ival      like tpd, but take existing values'
      print *,' tpda           plot travel time partial derivatives for all parameters'
      print *,' gpd ival       plot green partial derivatives for parameter ival'
      print *,' qgpd ival      like gpd, but take existing values'
      print *,' gpda           plot green partial derivatives for all parameters'
      print *,' wgpd name      write green partial derivatives to files with'
      print *,'                base ''name'' '
      print *,' ense f,t,nu,rms   rate model parameters with index from ''f'' to'
      print *,'                ''t'' when optimizing all other free parameters'
      print *,'                stabilized with ''nu'' and accepting an'
      print *,'                increase in the rms-error'
      print *,'                by ''rms'' in a linear approximation'
      print *,'                ''rms'' is given as a fraction of the'
      print *,'                rms misfit of the reference model'
      print *,' sense f,t,nu,rms  just like ''ense'' but puts rating for each'
      print *,'                parameter on a different page'
      print *,' wense f,t,nu,rms,name'
      print *,'                just like ''sense'' but puts rating for each'
      print *,'                parameter to a file with base ''name'' '
      print *,' swense f,t,nu,err,name'
      print *,'                just like ''wense'' but checks square'
      print *,'                error rather than rms error'
      print *,'                ''err'' is the allowed increase of the' 
      print *,'                square error given as a fraction of misfit'
      print *,'                of the reference synthetics'
      print *,' ssense f,t,nu,err'
      print *,'                just like ''sense'' but in misfit,'
      print *,'                not in rms mode'
      print *,' sqense f,t,nu,err'
      print *,'                just like ''ense'' but in misfit,'
      print *,'                not in rms mode'
      print *,' qense, qsense, qssense, qsqense'
      print *,'                these are versions of ''ense'','
      print *,'                ''sense'', ''ssense'', and ''sqense'' '
      print *,'                that use preexisting partial derivatives'
      call gremhelp_subtit('tests')
      print *,' orth           calculate scalar products and normalized scalar'
      print *,'                products of vectors of partial derivatives'
      call gremhelp_subtit('parameter control')
      print *,' spc            set free model parameters'
      print *,' dpc            display free parameter settings in a table'
      print *,'                similar to the "spc" mask'
      print *,' tpc            display free parameter settings in a table'
      print *,' dgr            display real green data in different modes'
      print *,' dtt            display travel time data'
      print *,' spa            set various parameters'
      print *,' dpa            display all parameter settings'
      print *,' dda            display basic crontrol screen'
      call gremhelp_subtit('misc')
      print *,' exit           leave submenu'
      print *,' quit           leave submenu'
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine gremhelp_files
c
c help on files menu
c
      call gremhelp_tit('files')
      print *,' '
      print *,'green file     read green data from "file"'
      print *,'gwread file    read green weights from "file"'
      print *,'tt file        read travel-time data from "file"'
      print *,'mtt file       merge travel-time data from "file"'
      print *,'               this will be used for sections below asphalt'
      print *,'model file     read polynomial model from "file"'
      print *,'para file      read gremlin parameters from "file"'
      print *,'save file      write current work model to "file"'
      print *,'resp file      write complex response file'
      print *,'wsave file     save model parameter weights to file'
      print *,'wread file     read model parameter weights from file'
      print *,'wwrt file      write original traveltime weights'
      print *,'wwct file      write calculated traveltime weights'
      print *,'wwrg file      write original green weights'
      print *,'wwcg file      write calculated green weights'
      print *,'exit           leave submenu'
      print *,'quit           leave submenu'
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine gremhelp_subtit(title)
c
c print help subtitle
c
      character title*(*)
c
      print *,' '
      print *,title
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine gremhelp_tit(title)
c
c print help title
c
      character title*(*)
c
      print *,' '
      print *,'GREMLIN (help): ',title
      return
      end
c
c ----- END OF gremlin_help.f -----
