/*! \file hierarchicaltimestring.cc
 * \brief produce a time string that can be interpreted by the constructor (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/12/2008
 * 
 * produce a time string that can be interpreted by the constructor
 * (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 12/12/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_HIERARCHICALTIMESTRING_CC_VERSION \
  "TF_HIERARCHICALTIMESTRING_CC   V1.0   "

#include <sstream>
#include <libtime++.h>

namespace libtime {

std::string TAbsoluteTime::hierarchicalstring() const
{
  std::ostringstream oss;
  oss.width(4);
  oss.fill('0');
  oss << this->year() << "/";
  oss.width(2);
  oss.fill('0');
  oss << this->month() << "/";
  oss.width(2);
  oss.fill('0');
  oss << this->day() << "-";
  oss.width(2);
  oss.fill('0');
  oss << this->hour() << ":";
  oss.width(2);
  oss.fill('0');
  oss << this->minute() << ":";
  oss.width(2);
  oss.fill('0');
  oss << this->second() << ".";
  oss.width(3);
  oss.fill('0');
  oss << this->milsec();
  oss.width(3);
  oss.fill('0');
  oss << this->micsec();
  return(oss.str());
}

std::string TRelativeTime::hierarchicalstring() const
{
  std::ostringstream oss;
  oss << this->days() << "-";
  oss.width(2);
  oss.fill('0');
  oss << this->hour() << ":";
  oss.width(2);
  oss.fill('0');
  oss << this->minute() << ":";
  oss.width(2);
  oss.fill('0');
  oss << this->second() << ".";
  oss.width(3);
  oss.fill('0');
  oss << this->milsec();
  oss.width(3);
  oss.fill('0');
  oss << this->micsec();
  return(oss.str());
}

} // namespace libtime

/* ----- END OF hierarchicaltimestring.cc ----- */
