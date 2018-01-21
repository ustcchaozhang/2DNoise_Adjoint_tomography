c this is <par_verbosity.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c control verbosity levels
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
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    02/12/98   V1.1   introduced debug mode
c    24/01/99   V1.2   new option verb_changes
c
      subroutine par_verbosity
c
c 
      include 'glq_dim.inc'
      include 'glq_verbose.inc'
c 
cE
      character arg*(80)
      logical hot
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'VERBOSITY'
        call pars_verb('subaction','sac',verb_subaction)
        call pars_verb('subinput','sip',verb_subinput)
        call pars_verb('subresult','sre',verb_subresult)
        call pars_verb('substrategy','sst',verb_substrategy)
        call pars_verb('medstrategy','mst',verb_medstrategy)
        call pars_verb('topstrategy','tst',verb_topstrategy)
        call pars_verb('io','io ',verb_io)
        call pars_verb('allwarn','aw ',verb_allwarn)
        call pars_verb('graph','gra',verb_graph)
        call pars_verb('model','mod',verb_model)
        call pars_verb('debug','dbg',verb_debug)
        call pars_verb('changes','chg',verb_changes)
        print *,'VERBOSITY - your command:'
        read(5, '(a80)') arg
c 
        if (arg(1:6).eq.'help ') then
          print *,'quit exit sip sst mst tst io aw gra sac mod chg all none'
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        elseif (arg(1:4).eq.'sip ') then
          verb_subinput=(.not.(verb_subinput))
        elseif (arg(1:4).eq.'sre ') then
          verb_subresult=(.not.(verb_subresult))
        elseif (arg(1:4).eq.'sst ') then
          verb_substrategy=(.not.(verb_substrategy))
        elseif (arg(1:4).eq.'mst ') then
          verb_medstrategy=(.not.(verb_medstrategy))
        elseif (arg(1:4).eq.'tst ') then
          verb_topstrategy=(.not.(verb_topstrategy))
        elseif (arg(1:3).eq.'io ') then
          verb_io=(.not.(verb_io))
        elseif (arg(1:3).eq.'aw ') then
          verb_allwarn=(.not.(verb_allwarn))
        elseif (arg(1:4).eq.'sac ') then
          verb_subaction=(.not.(verb_subaction))
        elseif (arg(1:4).eq.'gra ') then
          verb_graph=(.not.(verb_graph))
        elseif (arg(1:4).eq.'mod ') then
          verb_model=(.not.(verb_model))
        elseif (arg(1:4).eq.'chg ') then
          verb_changes=(.not.(verb_changes))
        elseif (arg(1:4).eq.'dbg ') then
          verb_debug=(.not.(verb_debug))
        elseif (arg(1:5).eq.'none ') then
          verb_subaction=.false.
          verb_subinput=.false.
          verb_subresult=.false.
          verb_substrategy=.false.
          verb_medstrategy=.false.
          verb_topstrategy=.false.
          verb_allwarn=.false.
          verb_graph=.false.
          verb_io=.false.
          verb_debug=.false.
          verb_changes=.false.
        elseif (arg(1:4).eq.'all ') then
          verb_subaction=.true.
          verb_subinput=.true.
          verb_subresult=.true.
          verb_substrategy=.true.
          verb_medstrategy=.true.
          verb_topstrategy=.true.
          verb_allwarn=.true.
          verb_graph=.true.
          verb_io=.true.
          verb_debug=.true.
          verb_changes=.true.
        else
          print *,'NOTICE: unknown read command (try ''help'')'
        endif
c 
      enddo
c 
      return
      end
c
c ----- END OF par_verbosity.f -----
