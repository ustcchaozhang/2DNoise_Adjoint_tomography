/* this is <base_constr.cc>
 * ----------------------------------------------------------------------------
 *
 * 09/08/2000 by Thomas Forbriger (IfG Stuttgart)
 *
 * TBaseClassTime constructor
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
 *    09/08/2000   V1.0   Thomas Forbriger
 *    06/09/2000   V1.1   added character array initializer
 *    22/12/2000   V1.2   changed namespace time to libtime (resolved conflict
 *                        with system time library)
 *    22/12/2003   V1.3   added function now()
 *                        correction: month in struct tm is in (0,11)
 *    02/02/2004   V1.4   moved conversion code double2time()
 *    17/12/2007   V1.5   replace long int by typedef timeint
 *    11/11/2009   V1.6   header cstdlib is required for abort
 *    20/02/2012   V1.7   use const char*
 *
 * ============================================================================
 */

#include "libtime++.h"
#include <cstring>
#include <ctime>
#include <cstdlib>
#include <iostream>

namespace libtime {

void TBaseClassTime::string_read(const std::string &timestring)
{
  if (time_kernel::time_read(&Mtime_Ts, 
                             timestring.c_str())
      !=EXIT_SUCCESS)
  {
    std::cerr << "TBaseClassTime could not initialize time structure "
              << "from string:\n" << timestring << "\n";
    std::abort();
  }
}

void TBaseClassTime::char_read(const char *timestring)
{
  string_read(timestring);
}

}; // namespace libtime
 
/* ----- END OF base_constr.cc ----- */
