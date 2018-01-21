/*! \file fapid_sff_help_details.cc
 * \brief print detailed information on available formats (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/11/2010
 * 
 * print detailed information on available formats (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 26/11/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_FAPID_SFF_HELP_DETAILS_CC_VERSION \
  "TF_FAPID_FAPID_SFF_HELP_DETAILS_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/helper.h>
#include <datrwxx/formats.h>

using namespace fapidxx;

/*! \brief print detailed information about modules in libdatrwxx
 *
 * \ingroup implemented_functions
 */
int sff_help_details__(void)
{
  datrw::online_help();
  return(0);
} // int sff_help_details__(void)

/* ----- END OF fapid_sff_help_details.cc ----- */
