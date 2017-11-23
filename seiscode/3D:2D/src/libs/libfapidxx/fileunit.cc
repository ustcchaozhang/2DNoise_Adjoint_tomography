/*! \file fileunit.cc
 * \brief a file unit interface to libdatrwxx (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/11/2010
 * 
 * a file unit interface to libdatrwxx (implementation)
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
 *  - 17/11/2010   V1.0   Thomas Forbriger
 *  - 23.12.2010   V1.1   added output streams
 *  - 07.09.2011   V1.2   support format modifier strings
 * 
 * ============================================================================
 */
#define TF_FILEUNIT_CC_VERSION \
  "TF_FILEUNIT_CC   V1.2"

#include<fstream>
#include <fapidxx/fileunit.h>
#include <fapidxx/error.h>
#include <fapidxx/helper.h>

namespace fapidxx {

  //! the global istream manager
  ::fapidxx::IFileUnits istreammanager;

  //! the global ostream manager
  ::fapidxx::OFileUnits ostreammanager;

  //! file format to be used
  datrw::Eformat selectedformat=datrw::Fsff;

  /*======================================================================*/

  IFileUnits::IFileUnits()
    : Mformat(datrw::anyID(datrw::Fsff))
  {
    Mstreammap.clear();
  } // IFileUnits::IFileUnits()

  /*----------------------------------------------------------------------*/

  IFileUnits::~IFileUnits()
  {
    while (Mstreammap.size()>0)
    {
      Tstreammap::iterator first=Mstreammap.begin();
      this->close(first->first);
    }
    Mstreammap.clear();
  } // IFileUnits::~IFileUnits()

  /*----------------------------------------------------------------------*/
  datrw::ianystream& IFileUnits::open(const int& unit,
                                        const std::string& filename)
  {
    FAPIDXX_fuassert((!this->isopen(unit)), unit,
                     "IFileUnits::open: file is already open");
    IstreamCompound &compound=Mstreammap[unit];
    compound.Mistream=new std::ifstream(filename.c_str(),
                                        datrw::ianystream::openmode(Mformat));
    compound.Mianystream=new datrw::ianystream(*compound.Mistream, Mformat);
    return (this->getstream(unit));
  } // datrw::ianystream& IFileUnits::open

  /*----------------------------------------------------------------------*/

  void IFileUnits::close(const int& unit) 
  {
    FAPIDXX_fuassert((this->isopen(unit)), unit,
                     "IFileUnits::close: file is not open");
    IstreamCompound &compound=Mstreammap[unit];
    delete compound.Mianystream;
    delete compound.Mistream;
    Mstreammap.erase(unit);
  } // void IFileUnits::close(const int& unit)

  /*----------------------------------------------------------------------*/

  bool IFileUnits::isopen(const int& unit) const
  {
    bool retval=false;
    retval = (Mstreammap.count(unit)>0);
    return(retval);
  } // bool IFileUnits::isopen(const int& unit) const

  /*----------------------------------------------------------------------*/

  datrw::ianystream& IFileUnits::getstream(const int& unit)
  {
    FAPIDXX_fuassert((this->isopen(unit)), unit,
                     "IFileUnits::getstream: file is not open");
    return (*Mstreammap[unit].Mianystream);
  } // datrw::ianystream& IFileUnits::getstream(const int& unit)

  /*----------------------------------------------------------------------*/

  void IFileUnits::setformat(const std::string& format)
  {
    Mformat=format;
  } // void IFileUnits::setformat(const std::string& format)

  /*======================================================================*/

  OFileUnits::OFileUnits()
    : Mformat(datrw::anyID(datrw::Fsff))
  {
    Mstreammap.clear();
  } // OFileUnits::OFileUnits()

  /*----------------------------------------------------------------------*/

  OFileUnits::~OFileUnits()
  {
    while (Mstreammap.size()>0)
    {
      Tstreammap::iterator first=Mstreammap.begin();
      this->close(first->first);
    }
    Mstreammap.clear();
  } // OFileUnits::~OFileUnits()

  /*----------------------------------------------------------------------*/
  datrw::oanystream& OFileUnits::open(const int& unit,
                                      const std::string& filename)
  {
    FAPIDXX_fuassert((!this->isopen(unit)), unit,
                     "OFileUnits::open: file is already open");
    OstreamCompound &compound=Mstreammap[unit];
    datrw::abort_if_exists(filename);
    compound.Mostream=new std::ofstream(filename.c_str(),
                                        datrw::oanystream::openmode(Mformat));
    compound.Moanystream=new datrw::oanystream(*compound.Mostream, Mformat);
    return (this->getstream(unit));
  } // datrw::oanystream& OFileUnits::open

  /*----------------------------------------------------------------------*/

  void OFileUnits::close(const int& unit) 
  {
    FAPIDXX_fuassert((this->isopen(unit)), unit,
                     "OFileUnits::close: file is not open");
    OstreamCompound &compound=Mstreammap[unit];
    delete compound.Moanystream;
    delete compound.Mostream;
    Mstreammap.erase(unit);
  } // void OFileUnits::close(const int& unit)

  /*----------------------------------------------------------------------*/

  bool OFileUnits::isopen(const int& unit) const
  {
    bool retval=false;
    retval = (Mstreammap.count(unit)>0);
    return(retval);
  } // bool OFileUnits::isopen(const int& unit) const

  /*----------------------------------------------------------------------*/

  datrw::oanystream& OFileUnits::getstream(const int& unit)
  {
    FAPIDXX_fuassert((this->isopen(unit)), unit,
                     "OFileUnits::getstream: file is not open");
    return (*Mstreammap[unit].Moanystream);
  } // datrw::oanystream& OFileUnits::getstream(const int& unit)

  /*----------------------------------------------------------------------*/

  void OFileUnits::setformat(const std::string& format)
  {
    Mformat=format;
  } // void OFileUnits::setformat(const std::string& format)

} // namespace fapidxx

/*! \page page_fileunits Implementation of file units
 *
 * In Fortran the access to different files is controlled by file units which
 * are addressed by integer numbers. 
 * In order to mimic the behaviour of Fortran I/O we have to provide means to
 * handle the access to multiple input and output files such that they can be
 * addressed by integer numbers.
 * Two classes provide this functionality.
 * \sa fapidxx::IFileUnits, fapidxx::OFileUnits
 */

/* ----- END OF fileunit.cc ----- */
