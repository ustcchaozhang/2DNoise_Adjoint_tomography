/*! \file panel.h
 * \brief all stuff to handle plot panels (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle plot panels (prototypes)
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
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_PANEL_H_VERSION

#define STUPLO_PANEL_H_VERSION \
  "STUPLO_PANEL_H   V1.1   (17-03-2015)"

#include<vector>
#include <pgplotxx/xpgplotxx.h>
#include "datatrace.h"

namespace stuplo {

  /*----------------------------------------------------------------------*/

  /*! struct to hold parameters that define the appearance of a panel
   *
   * scaling parameters will be held separately
   */
  struct PanelParameters {
  }; // struct PanelParameters

  /*----------------------------------------------------------------------*/

  /*! \brief struct to hold panel parameters
   *  this includes the set of files that should be displayed within this panel
   */
  struct Panel {
    //! number of traces in this panel
    int ntraces() const { return(dtl.size()); }
    //! list of all traces in panel
    DataTraceList dtl;
    //! parameters defining panel location and world coordinates
    PanelParameters pp;
  }; // struct Panel

  /*----------------------------------------------------------------------*/

  class PanelVector: public std::vector<Panel>
  {
    public:
      typedef std::vector<Panel> Tbase;
      libtime::TRange timerange() const;
  }; // class PanelVector

} // namespace stuplo

#endif // STUPLO_PANEL_H_VERSION (includeguard)

/* ----- END OF panel.h ----- */
