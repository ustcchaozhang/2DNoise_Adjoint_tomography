/*! \file helper.h
 * \brief some helper functions (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * some helper functions (prototypes)
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
 *  - 18/11/2010   V1.0   Thomas Forbriger
 *  - 25/11/2010   V1.1   added trimws
 *  - 01/04/2011   V1.2   provide sff_helper_decode_wid2__
 *  - 02/04/2011   V1.3   implement WID2container::WID2struct
 *  - 05/07/2014   V1.4   move WID2container to wid2container.h
 * 
 * ============================================================================
 */

// include guard
#ifndef FAPIDXX_HELPER_H_VERSION

#define FAPIDXX_HELPER_H_VERSION \
  "FAPIDXX_HELPER_H   V1.4"

#include<string>
#include<iostream>
#include<libtime++.h>
#include<sffxx.h>
#include<fapidxx/fapidsff.h>

namespace fapidxx {

  //! create a C++ string from a Fortran string
  std::string stringfromfstring(char *fstring, ftnlen slen);

  //! fill a Fortran string with a C++ string
  void fillfstring(const std::string& s, char *fstring, ftnlen slen);

  //! remove whitespace at end of string
  std::string trimws_end(std::string s);

  //! remove whitespace at beginning and end of string
  std::string trimws(std::string s);

  //! create the tanf value
  float maketanf(const libtime::TAbsoluteTime& time);

  //! create appropriate time string for SRCE line
  libtime::TAbsoluteTime SRCEdate(char *date, char* time, 
                                  ftnlen date_len, ftnlen time_len);

  //! create C++ FREE block from Fortran FREE lines
  ::sff::FREE freeblock(integer *nline, char *lines, ftnlen lines_len);

  //! create Fortran FREE lines from C++ FREE bock
  void freeblock(const ::sff::FREE& free,
                 integer *nline, char *lines, integer *lindim, 
                 integer *lenmax, ftnlen lines_len);

} // namespace fapidxx

/*======================================================================*/

/*! \brief produce debug output
 *
 * \param C output will be generated if C == true
 * \param N name of function 
 * \param M message to print
 */

#define FAPIDXX_debug(C,N,M) \
  if (C) { \
    std::cerr << "DEBUG (" << N << ", " \
    << __FILE__ << " line #" << __LINE__ << "):" << std::endl \
    << "      " << M << std::endl; \
      std::cerr.flush(); \
  }

/*======================================================================*/
// functions with Fortran interface

extern "C" {

/*! \brief decode a WID2 character sequence
 *
 * Takes a WID2 character sequence in any encoding an provides a standard WID2
 * line.
 * See discussion in \ref sec_comments_prepwid2
 *
 * \param wid2in WID2 data (function input value)
 * \param wid2out WID2 data (function output value)
 * \param wid2in_len size of wid2in array
 * \param wid2out_len size of wid2out array
 */
extern int sff_helper_decode_wid2__(char *wid2in, char *wid2out, 
                                    ftnlen wid2in_len, ftnlen wid2out_len);

} // extern "C"

#endif // FAPIDXX_HELPER_H_VERSION (includeguard)

/* ----- END OF helper.h ----- */
