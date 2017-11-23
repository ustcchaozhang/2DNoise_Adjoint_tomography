/*! \file itfasciistream.cc
 * \brief read Thomas Forbrigers ASCII data (implementation) 
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 05/10/2010
 * 
 * Purpose: read Thomas Forbrigers ASCII data (implementation) 
 *
 * ----
 * This file is part of libdatrwxx.
 *
 * libdatrwxx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libdatrwxx is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libdatrwxx.  If not, see <http://www.gnu.org/licenses/>.
 * ----
 * 
 * Copyright (c) 2010 by Daniel Armbruster
 * 
 * REVISIONS and CHANGES 
 *  - 05/10/2010   V0.1  Daniel Armbruster
 *  - 23/11/2010   V1.1  Thomas Forbriger: introduced static member data
 * 
 * ============================================================================
 */
 
#define DATRW_TFASCII_CC_VERSION \
  "DATRW_TFASCII_CC   V1.1"

#include <datrwxx/tfascii.h>
#include <datrwxx/error.h>
#include <datrwxx/util.h>

namespace datrw {

  const std::ios_base::openmode itfasciistream::openmode=std::ios_base::in;

  //@{
  /*! \brief Format properties
   * \ingroup group_tfascii
   */
  const bool tfascii::isbinary=false;
  const char* const tfascii::streamID="tfascii";
  //@}

  /*----------------------------------------------------------------------*/
  // constructor
  itfasciistream::itfasciistream(std::istream& is, const bool& verbose) 
  : Tbase(is, true, true, true), Mverbose(verbose)
  { 
    set_fileheader();
  } // function itfasciistream::itfasciistream

  /*----------------------------------------------------------------------*/
  // destructor
  itfasciistream::~itfasciistream()
  { }

  /*----------------------------------------------------------------------*/
  void itfasciistream::help(std::ostream& os)
  { datrw::tfascii::help(os); }
  
  /*----------------------------------------------------------------------*/
  // implicit virtual functions

  Tdseries itfasciistream::dseries()
  {
    set_traceheader();
    return (util::readasciidouble<Tdseries>(Mis, this->wid2().nsamples, 
      tfascii::streamname));
  } // function itfasciistream::dseries

  /*----------------------------------------------------------------------*/
  Tfseries itfasciistream::fseries()
  {
    set_traceheader();
    return (util::readasciidouble<Tfseries>(Mis, this->wid2().nsamples, 
      tfascii::streamname));
  } // function itfasciistream::fseries

  /*----------------------------------------------------------------------*/
  Tiseries itfasciistream::iseries()
  {
    set_traceheader();
    return (util::readasciidouble<Tiseries>(Mis, this->wid2().nsamples, 
      tfascii::streamname));
  } // function itfasciistream::iseries

  /*----------------------------------------------------------------------*/
  // private functions
  void itfasciistream::set_fileheader() {
    tfascii::FileHeader fileheader = tfascii::readfileheader(Mis,
      get_verbose()); 
    if (fileheader.hasfilefree)
    {
      this->setfilefree(fileheader.filefree);
    }
    if (fileheader.hassrce)
    {
      this->setsrce(fileheader.srce);
    }
  } // function itfasciistream::set_fileheader

  /*----------------------------------------------------------------------*/
  void itfasciistream::set_traceheader()
  {
    tfascii::TraceHeader traceheader = tfascii::readtraceheader(Mis,
      get_verbose()); 
    this->newtrace();
    this->setwid2(traceheader.wid2);
    if(traceheader.hastracefree) 
    {
      this->settracefree(traceheader.tracefree);
    }
    if (traceheader.hasinfo)
    {
      this->setinfo(traceheader.info);
    }
    this->setlast();
  } // function itfasciistream::set_traceheader

  /*----------------------------------------------------------------------*/
  bool itfasciistream::get_verbose() const
  {
    return Mverbose;
  } // function itfasciistream::get_verbose

} // namespace datrw


/* ----- END OF itfasciistream.cc ----- */
