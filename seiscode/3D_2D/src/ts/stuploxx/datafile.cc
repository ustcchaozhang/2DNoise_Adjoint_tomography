/*! \file datafile.cc
 * \brief all stuff to handle data files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle data files (implementation)
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
 * REVISIONS and CHANGES 
 *  - 28/01/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_DATAFILE_CC_VERSION \
  "STUPLO_DATAFILE_CC   V1.0   "

#include "datafile.h"

namespace stuplo {

  libtime::TRange DataFile::timerange() const
  {
    TSFile::Tfile::Ttracevector tv(this->file.data);
    TSFile::Tfile::const_iterator I=tv.begin();
    libtime::TRange retval(I->header.wid2().date, I->header.wid2().date);
    while (I != tv.end())
    {
      retval.expand(libtime::TRange(I->header.wid2().date,
                                    I->header.wid2().date));
      ++I;
    }
    return(retval);
  } // libtime::Trange DataFile::timerange() const

  /*----------------------------------------------------------------------*/

  libtime::TRange DataFileList::timerange() const
  {
    Tbase::const_iterator I=this->begin();
    libtime::TRange retval(I->timerange());
    while (I != this->end())
    {
      retval.expand(I->timerange());
      ++I;
    }
    return(retval);
  } // libtime::TRange DataFileList::timerange() const


} // namespace stuplo

/* ----- END OF datafile.cc ----- */
