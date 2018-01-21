/*! \file panel.cc
 * \brief all stuff to handle plot panels (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle plot panels (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 * 
 * REVISIONS and CHANGES 
 *  - 28/01/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_PANEL_CC_VERSION \
  "STUPLO_PANEL_CC   V1.0   "

#include "panel.h"

namespace stuplo {

  /*----------------------------------------------------------------------*/

  libtime::TRange PanelVector::timerange() const
  {
    Tbase::const_iterator I=this->begin();
    libtime::TRange retval(libtime::now(), 
                           libtime::now()+libtime::TRelativeTime(0,0,0,1));
    bool unset=true;
    while (I != this->end())
    {
      if (I->dtl.size()>0)
      {
        if (unset)
        {
          retval=I->dtl.timerange();
          unset=false;
        }
        else
        {
          retval.expand(I->dtl.timerange());
        }
      }
      ++I;
    }
    return(retval);
  } // libtime::TRange PanelVector::timerange() const

} // namespace stuplo

/* ----- END OF panel.cc ----- */
