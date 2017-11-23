/*! \file pgplotxx.cc
 * \brief binary code (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/03/2008
 * 
 * binary code (implementation)
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
 *  - 07/03/2008   V1.0   Thomas Forbriger
 *  - 17/03/2015   V1.1   rename file to adopt naming convention in Seitosh
 * 
 * ============================================================================
 */
#define TF_PGPLOTCPP_CC_VERSION \
  "TF_PGPLOTCPP_CC   V1.0   (17-03-2015)"

#include <pgplotxx/pgplotxx.h>
#include "pgplot_escape_sequences_help_text.h"

namespace pgplot {

  /*----------------------------------------------------------------------*/

//! usage text for escape sequences
const char* const usage_escape_sequences=pgplot_escape_sequences_help_text;

} // namespace pgplot

/* ----- END OF pgplotxx.cc ----- */
