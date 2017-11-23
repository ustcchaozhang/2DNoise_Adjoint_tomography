/*! \file ranges.cc
 * \brief time range class (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/02/2004
 * 
 * time range class (implementation)
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
 * 
 * ============================================================================
 */
#define TF_RANGES_CC_VERSION \
  "TF_RANGES_CC   V1.0   "

#include <libtime++.h>
#include<iostream>

namespace libtime {

  TRange::TRange(const TAbsoluteTime& begin,
                 const TAbsoluteTime& end):
    Mbegin(begin), Mend(end)
    {
      libtime_assert((end>=begin),"ERROR (Trange): illegal range!");
    }

  //! true if date is within range
  bool TRange::includes(const TAbsoluteTime& date) const
  {
    return((date>=Mbegin)&&(date<=Mend));
  }

  //! true if range is fully included in this range
  bool TRange::includes(const TRange& range) const
  {
    return((range.begin()>=Mbegin)&&(range.end()<=Mend));
  }

  //! true if other range overlaps this one
  bool TRange::overlaps(const TRange& range) const
  {
    return((range.end()>=Mbegin)&&(range.begin()<=Mend));
  }

  //! find largest range spanned by both
  TRange TRange::largestcommon(const TRange& range) const
  {
    libtime_assert((this->overlaps(range)),
                   "ERROR (Trange::largestcommon): ranges must overlap");
    return(TRange(Mbegin<range.begin() ? Mbegin : range.begin(),
                  Mend>range.end() ? Mend : range.end()));
  }

  //! find largest range common to both
  TRange TRange::smallestcommon(const TRange& range) const
  {
    libtime_assert((this->overlaps(range)),
                   "ERROR (Trange::smallestcommon): ranges must overlap");
    return(TRange(Mbegin>range.begin() ? Mbegin : range.begin(),
                  Mend<range.end() ? Mend : range.end()));
  }

  //! delay this range by dt
  TRange& TRange::delay(const TRelativeTime& dt)
  {
    Mbegin+=dt;
    Mend+=dt;
    return(*this);
  }

  //! advance this range by dt
  TRange& TRange::advance(const TRelativeTime& dt)
  {
    Mbegin-=dt;
    Mend-=dt;
    return(*this);
  }

  //! return a range delayed by dt
  TRange TRange::delayedby(const TRelativeTime& dt) const
  {
    TRange retval(*this);
    retval.delay(dt);
    return(retval);
  }

  //! return a range advanced by dt
  TRange TRange::advancedby(const TRelativeTime& dt) const
  {
    TRange retval(*this);
    retval.advance(dt);
    return(retval);
  }

  //! set to largest range spanned by both
  void TRange::expand(const TRange& range) 
  {
    Mbegin = Mbegin < range.begin() ? Mbegin : range.begin();
    Mend   = Mend   > range.end()   ? Mend   : range.end();
  }

  //! set to smallest range common to both
  void TRange::shrink(const TRange& range) 
  {
    libtime_assert((this->overlaps(range)),
                   "ERROR (Trange::shrink): ranges must overlap");
    Mbegin = Mbegin > range.begin() ? Mbegin : range.begin();
    Mend   = Mend   < range.end()   ? Mend   : range.end();
  }

} // namespace libtime

/* ----- END OF ranges.cc ----- */
