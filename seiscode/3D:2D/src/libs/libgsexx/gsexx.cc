/*! \file gsexx.cc
 * \brief GSE++ library: read and write GSE waveform data (implementation).
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 16/03/2002
 * 
 * GSE++ library: read and write GSE waveform data (implementation)
 * 
 * Contains documentation that is not put into the header file and does not
 * fit elsewhere.
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
 *  - 16/03/2002   V1.0   Thomas Forbriger
 *  - 29/03/2002   V1.1   moved class specific stuff to other files
 * 
 * ============================================================================
 */

#define TF_GSEXX_CC_VERSION \
  "TF_GSEXX_CC   V1.0   "
#define TF_GSEXX_CC_CVSID \
  "$Id$"

#include <gsexx.h>

/*! \namespace GSE2
 *
 * This namespace holds stuff related to the GSE2 format specification.
 * The module includes material for the GSE2.0 subformat and the GSE2.1
 * subformat. It is subdevided into modules for waveforms (provided in
 * GSE2::waveform) and others, which are not yet defined. 
 * \sa GSE2::waveform
 *
 * \if internal
 * \par Internal modules
 * The submodules GSE2::tests contains functions performing test cases on the
 * GSE2 module elements.
 * \sa GSE2::tests
 * \endif
 */
namespace GSE2 {

bool Terror::silent=false;

/*! \namespace GSE2::waveform
 *
 * This submodule contains classes and functions for reading and writing GSE2
 * waveform data. 
 *
 * \if internal
 * \par Internal modules
 * You will find additional submodules. They are
 * 
 * \arg GSE2::waveform::differences contains template metaprograms that apply
 *      first or second (or higher) differences to a data stream (or remove
 *      them).
 * \arg GSE2::waveform::cm6 containes function for encoding and decoding the
 *      data in the CM6 subformat.
 *
 * \sa GSE2::waveform::differences
 * \sa GSE2::waveform::cm6
 * \endif
 *
 * \sa GSE2::waveform::TWID2
 * \sa GSE2::waveform::TSTA2
 * \sa GSE2::waveform::TDAT2
 * \sa GSE2::waveform::TCHK2
 */
namespace waveform {

} // namespace waveform

/*! \fn template<class C> bool GSEIDmatch<C>(const string& line)
 *
 * \param C Template parameter: Any GSE class that contains a static GSEID
 *          member. This static member is checked against the first four
 *          characters in \a line.
 * \param line An input line which has to contain the identifier specified by
 *             the static GSEID member in class \a C.
 * \return true if identifier was found in \a line.
 */
} // namespace GSE2

/* ----- END OF gsexx.cc ----- */
