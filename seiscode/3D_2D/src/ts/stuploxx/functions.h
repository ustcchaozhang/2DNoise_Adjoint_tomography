/*! \file functions.h
 * \brief external functions and structures (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * external functions and structures (prototypes)
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


// include guard
#ifndef STUPLO_FUNCTIONS_H_VERSION

#define STUPLO_FUNCTIONS_H_VERSION \
  "STUPLO_FUNCTIONS_H   V1.0   "

#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>

#include "utilitystructures.h"
#include "datafile.h"
#include "panel.h"

namespace stuplo {

  FileParametersList setparameters(const tfxx::cmdline::Tparsed& fns,
                                   const PGstyle& pgs);

  DataFileList readdata(const FileParametersList& fpl);

  PanelVector setuppanels(const DataFileList& fpl,
                          const int& npanels,
                          const bool& verbose=false,
                          const bool& debug=false);

  void setinitialranges(PanelVector& pl, const PGstyle&);

  void plotdata(pgplot::basic_device& dev,
                const PanelVector& pl, 
                const PGstyle&);

} // namespace stuplo

#endif // STUPLO_FUNCTIONS_H_VERSION (includeguard)

/* ----- END OF functions.h ----- */
