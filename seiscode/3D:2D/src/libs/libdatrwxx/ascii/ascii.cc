/*! \file ascii.cc
 * \brief interface to write ASCII data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2006
 * 
 * interface to write ASCII data (implementation)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/04/2006   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_ASCII_CC_VERSION \
  "TF_ASCII_CC   V1.0   "

#include <datrwxx/ascii.h>
#include <datrwxx/asciiheaderkeys.h>

namespace datrw {

  namespace ascii {

    const char* const keydate="date";
    const char* const keydt="dt";
    const char* const keynsamples="n";
    const char* const keystation="station";
    const char* const keychannel="channel";
    const char* const keyauxid="auxid";
    const char* const keyinstype="instype";
    const char* const keycalib="calib";
    const char* const keycalper="calper";
    const char* const keyhang="hang";
    const char* const keyvang="vang";

    const char* const keySRCEdate="SRCEdate";
    const char* const keySRCEtype="SRCEtype";
    const char* const keySRCEX="SRCE-X";
    const char* const keySRCEY="SRCE-Y";
    const char* const keySRCEZ="SRCE-Z";
    const char* const keySRCECS="SRCE-CS";

    const char* const keyRECVX="RECV-X";
    const char* const keyRECVY="RECV-Y";
    const char* const keyRECVZ="RECV-Z";
    const char* const keyRECVCS="RECV-CS";
    const char* const keynstacks="nstacks";

    const char* const keydata="DATA";
    const char* const keyint="int";
    const char* const keyfloat="float";
    const char* const keydouble="double";

    const char* const keynonfatal="nonfatal";

  } // namespace ascii

} // namespace datrw

/* ----- END OF ascii.cc ----- */
