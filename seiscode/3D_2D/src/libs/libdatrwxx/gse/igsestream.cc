/*! \file igsestream.cc
 * \brief read raw GSE data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/09/2007
 * 
 * read raw GSE data (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/09/2007   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static members
 * 
 * ============================================================================
 */
#define DATRW_IGSESTREAM_CC_VERSION \
  "DATRW_IGSESTREAM_CC   V1.1"

#include <string>
#include <sstream>
#include <datrwxx/gse.h>
#include <datrwxx/gseread.h>
#include <datrwxx/error.h>
#include <datrwxx/util.h>

namespace datrw {

  const std::ios_base::openmode igsestream::openmode=std::ios_base::in;

  //@{
  /*! \brief Format properties
   * \ingroup group_gse
   */
  const bool gse::isbinary=false;
  const char* const gse::streamID="gse";
  //@}

  /*----------------------------------------------------------------------*/

  igsestream::igsestream(std::istream& is): Tbase(is, true, true, true) 
  { }

  /*----------------------------------------------------------------------*/

  Tdseries igsestream::dseries()
  {
    return(datrw::util::convert<Tiseries, Tdseries>(this->iseries()));
  }

  /*----------------------------------------------------------------------*/

  Tiseries igsestream::iseries()
  {
    this->readheader();
    DATRW_assert(Mis.good(), "input stream is not good");
    return(datrw::gse::read_gse_data(this->Mis, this->wid2().nsamples));
  }

  /*----------------------------------------------------------------------*/

  Tfseries igsestream::fseries()
  {
    return(datrw::util::convert<Tiseries, Tfseries>(this->iseries()));
  }

  /*----------------------------------------------------------------------*/

  void igsestream::skipseries()
  {
    this->readheader();
  }

  /*----------------------------------------------------------------------*/

  void igsestream::readheader()
  {
    this->newtrace();
    this->setwid2(datrw::gse::next_wid2(this->Mis));
    this->setlast();
  }

  /*----------------------------------------------------------------------*/

  void igsestream::help(std::ostream& os)
  { datrw::gse::help(os); }

} // namespace datrw

/* ----- END OF igsestream.cc ----- */
