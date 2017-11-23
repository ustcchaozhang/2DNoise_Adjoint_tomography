/*! \file gsexx_TCHK2.cc
 * \brief definition of TCHK2 functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 29/03/2002
 * 
 * definition of TCHK2 functions (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 29/03/2002   V1.0   Thomas Forbriger
 *  - 01/07/2005   V1.1   The read function still had the usual problems with
 *                        newlines
 *  - 11/11/2009   V1.2  abs is not overloaded for int in header cmath
 * 
 * ============================================================================
 */
#define TF_GSEXX_TCHK2_CC_VERSION \
  "TF_GSEXX_TCHK2_CC   V1.2"
#define TF_GSEXX_TCHK2_CC_CVSID \
  "$Id$"

#include<cmath>
#include <gsexx.h>
#include <sstream>
#include <cstdio>

namespace GSE2 {
namespace waveform {

//! helper function local to this source code
namespace helper {

  //! abs for intT
  intT abs(const intT& val)
  {
    return(val > 0 ? val : -val);
  } // int abs(const int* val)
  
} // namespace helper

/*! \class TCHK2
 *
 * This class allows the cumulative calculation of checksums. The code is
 * taken from the Appendix A.1 from the Conference Room Paper/243 of the Group
 * of Scientific Experts.
 */

//! GSE line identifier
const char* const TCHK2::GSEID="CHK2";

/*----------------------------------------------------------------------*/

/*!
 * This function adds the sample value to the checksum.
 *
 * \param value sample value to add
 */
void TCHK2::add(const intT& value)
{
  const intT modulo=100000000; // a one and eight zeros
  intT sample_value=value;

  // check for sample value overflow 
  if (helper::abs(sample_value) >= modulo)
  {
    sample_value = sample_value - int(sample_value/modulo)*modulo;
  }

  // add the sample value to the checksum
  Msum += sample_value;

  // check for checksum overflow
  if (helper::abs(Msum) >= modulo)
  {
    Msum = Msum - int(Msum/modulo)*modulo;
  }
}

/*----------------------------------------------------------------------*/

/*! \relates TCHK2
 * Write a checksum CHK2-line to a GSE data file.
 */
std::string TCHK2::write() const
{
  std::string retval("CHK2 ");
  char cvalue[10];
  std::sprintf(cvalue, "%8i\n", this->value());
  retval.append(cvalue);
  return(retval);
}

/*----------------------------------------------------------------------*/

/*! \relates TCHK2
 * Read a checksum CHK2-line from a GSE data file.
 */
void TCHK2::read(std::istream& fis)
{
  // read lineID first to pass newline after last data character
  std::string lineID;
  fis >> lineID;
  // DEBUG: std::cerr << lineID << std::endl;
  // now read a full line to chomp newline after checksum too
  std::string theline;
  // DEBUG: std::cerr << theline << std::endl;
  std::getline(fis, theline);
  std::istringstream is(theline);
  if (!GSEIDmatch<TCHK2>(lineID)) throw
     Terror("ERROR (CHK2::read): missing CHK2 line!");
  is >> this->Msum;
/* former code (01/07/2005)
  std::string lineID;
  is >> lineID;
  if (!GSEIDmatch<TCHK2>(lineID)) throw
    Terror("ERROR (TCHK2::read): missing CHK2 line!");
  is >> this->Msum;
*/
}

} // namespace waveform
} // namespace GSE2

/* ----- END OF gsexx_TCHK2.cc ----- */
