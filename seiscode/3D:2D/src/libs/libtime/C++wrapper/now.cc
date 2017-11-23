/*! \file now.cc
 * \brief return system time (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/02/2004
 * 
 * return system time (implementation)
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
 *  - 06/02/2004   V1.0   Thomas Forbriger
 *  - 25/11/2008   V1.1   added function utc()
 *  - 11/11/2009   V1.2   header ctime is required for time structs
 * 
 * ============================================================================
 */
#define TF_NOW_CC_VERSION \
  "TF_NOW_CC   V1.2"

#include <libtime++.h>
#include <ctime>

namespace libtime {

TAbsoluteTime now()
{
  std::time_t nowtime=std::time(NULL);
  std::tm *nowtm=std::localtime(&nowtime);
  int year=nowtm->tm_year+1900;
  int month=nowtm->tm_mon+1;
  int day=nowtm->tm_mday;
  int hour=nowtm->tm_hour;
  int minute=nowtm->tm_min; 
  int second=nowtm->tm_sec;
  TAbsoluteTime thetime(year, month, day, hour, minute, second); 
  return(thetime);
} // now()

TAbsoluteTime utc()
{
  std::time_t nowtime=std::time(NULL);
  std::tm *nowtm=std::gmtime(&nowtime);
  int year=nowtm->tm_year+1900;
  int month=nowtm->tm_mon+1;
  int day=nowtm->tm_mday;
  int hour=nowtm->tm_hour;
  int minute=nowtm->tm_min; 
  int second=nowtm->tm_sec;
  TAbsoluteTime thetime(year, month, day, hour, minute, second); 
  return(thetime);
} // utc()

} // namespace libtime

/* ----- END OF now.cc ----- */
