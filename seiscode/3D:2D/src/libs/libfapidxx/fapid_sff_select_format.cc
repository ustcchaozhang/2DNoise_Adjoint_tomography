/*! \file fapid_sff_select_format.cc
 * \brief select format for next file open operation (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/11/2010
 * 
 * select format for next file open operation (implementation)
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
 *  - 23.12.2010   V1.1   distinguish input and output format
 * 
 * ============================================================================
 */
#define TF_SFF_SELECT_FORMAT_CC_VERSION \
  "TF_SFF_SELECT_FORMAT_CC   V1.1"

#include <fapidxx/fapidsff.h>
#include <fapidxx/helper.h>
#include <fapidxx/fileunit.h>
#include <datrwxx/formats.h>
#include <datrwxx/error.h>

using namespace fapidxx;

/*! \brief select format for input and output
 *
 * \ingroup implemented_functions
 */
int sff_select_format__(char *formatid, integer *ierr, 
                        ftnlen formatid_len)
{
  std::string formatstring=stringfromfstring(formatid, formatid_len);
  *ierr=0;
  try { 
    istreammanager.setformat(formatstring);
    ostreammanager.setformat(formatstring);
  } catch (datrw::Exception) {
    *ierr=1;
  }
  return(0);
} // int sff_select_format__

/*----------------------------------------------------------------------*/

/*! \brief select format for input
 *
 * \ingroup implemented_functions
 */
int sff_select_input_format__(char *formatid, integer *ierr, 
                              ftnlen formatid_len)
{
  std::string formatstring=stringfromfstring(formatid, formatid_len);
  *ierr=0;
  try { 
    istreammanager.setformat(formatstring);
  } catch (datrw::Exception) {
    *ierr=1;
  }
  return(0);
} // int sff_select_input_format__

/*----------------------------------------------------------------------*/

/*! \brief select format for output
 *
 * \ingroup implemented_functions
 */
int sff_select_output_format__(char *formatid, integer *ierr, 
                               ftnlen formatid_len)
{
  std::string formatstring=stringfromfstring(formatid, formatid_len);
  *ierr=0;
  try { 
    ostreammanager.setformat(formatstring);
  } catch (datrw::Exception) {
    *ierr=1;
  }
  return(0);
} // int sff_select_output_format__

/* ----- END OF fapid_sff_select_format.cc ----- */
