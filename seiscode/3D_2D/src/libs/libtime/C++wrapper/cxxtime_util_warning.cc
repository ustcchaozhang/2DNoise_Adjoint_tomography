/*! \file cxxtime_util_warning.cc
 * \brief issue warning (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/02/2004
 * 
 * issue warning (implementation)
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *  - 07/02/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_CXXTIME_UTIL_WARNING_CC_VERSION \
  "TF_CXXTIME_UTIL_WARNING_CC   V1.0   "

#include <libtime++.h>
#include <string>
#include <iostream>

namespace libtime {
  bool Warning::suppress_normal=false;
  bool Warning::suppress_year=false;
  bool Warning::suppress_any=false;
} // namespace libtime

extern "C" {
/* 
 * Fortran calling convention:
 */
int time_util_warning__(char *caller, char *text, 
                        time_kernel::ftnlen caller_len, 
                        time_kernel::ftnlen text_len)
{
  std::string callerstring, textstring;
  int i;
  if (!(libtime::Warning::suppress_normal|| libtime::Warning::suppress_any))
  {
    for (i=0; i<caller_len; i++) { callerstring += *(caller++); }
    for (i=0; i<text_len; i++) { textstring += *(text++); }
    std::string message="ERROR ("+callerstring+"): "+textstring;
    std::cerr << message << std::endl;
  }
  return(0);
} /* time_util_warning__ */

int time_util_warning_n__(char *caller, char *text, 
                          time_kernel::integer *n, 
                          time_kernel::ftnlen caller_len, 
                          time_kernel::ftnlen text_len)
{
  std::string callerstring, textstring;
  int i;
  if (!(libtime::Warning::suppress_year|| libtime::Warning::suppress_any))
  {
    for (i=0; i<caller_len; i++) { callerstring += *(caller++); }
    for (i=0; i<text_len; i++) { textstring += *(text++); }
    std::string message="ERROR ("+callerstring+"): "+textstring;
    i=(int)*n;
    std::cerr << message << " " << i << std::endl;
  }
  return(0);
} /* time_util_warning_n__ */

} // extern "C"

/* ----- END OF cxxtime_util_warning.cc ----- */
