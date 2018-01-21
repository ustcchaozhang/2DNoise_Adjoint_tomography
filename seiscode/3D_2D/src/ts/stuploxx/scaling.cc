/*! \file scaling.cc
 * \brief provide some scaling functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/02/2008
 * 
 * provide some scaling functions (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 29/02/2008   V1.0   Thomas Forbriger
 *  - 05/01/2017   V1.1   properly handle negative values on abszissa
 * 
 * ============================================================================
 */
#define STUPLO_SCALING_CC_VERSION \
  "STUPLO_SCALING_CC   V1.1"

#include <libtime++.h>
#include "scaling.h"

namespace stuplo {

  pgplot::Trange PanelTime::frange() const
  {
    pgplot::Trange range(this->fbegin(),this->fend());
    return(range);
  } // pgplot::Trange PanelTime::frange() const

  /*----------------------------------------------------------------------*/

  float PanelTime::fbegin() const
  {
    float diff=libtime::time2double(this->begin()-this->reference());
    if (this->begin()<this->reference()) { diff *= -1.; }
    return(diff);
  }

  /*----------------------------------------------------------------------*/

  float PanelTime::fend() const
  {
    float diff=libtime::time2double(this->end()-this->reference());
    if (this->end()<this->reference()) { diff *= -1.; }
    return(diff);
  }

  /*----------------------------------------------------------------------*/

  void PanelTime::setrange(const pgplot::Trange& range) 
  {
    libtime::TAbsoluteTime abegin;
    libtime::TAbsoluteTime aend;
    if (range.min<0)
    {
      abegin=this->reference()-libtime::double2time(-range.min);
    }
    else
    {
      abegin=this->reference()+libtime::double2time(range.min);
    }
    if (range.max<0)
    {
      aend=this->reference()-libtime::double2time(-range.max);
    }
    else
    {
      aend=this->reference()+libtime::double2time(range.max);
    }
    this->setrange(libtime::TRange(abegin, aend));
  }

} // namespace stuplo

/* ----- END OF scaling.cc ----- */
