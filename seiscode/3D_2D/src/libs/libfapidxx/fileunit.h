/*! \file fileunit.h
 * \brief a file unit interface to libdatrwxx (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/11/2010
 * 
 * a file unit interface to libdatrwxx (prototypes)
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
 *  - 07.09.2011   V1.2   support format modifiers by storing the format in a
 *                        string
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FILEUNIT_H_VERSION

#define TF_FILEUNIT_H_VERSION \
  "TF_FILEUNIT_H   V1.2"

#include<map>
#include<string>
#include<iostream>
#include<datrwxx/readany.h>
#include<datrwxx/writeany.h>

namespace fapidxx {

  /*! A class to provide us with an ianystream
   */
  class IFileUnits {
    public:
      //! default contructor to initialize member data
      IFileUnits();
      //! destructor has to close all open files
      ~IFileUnits();
      //! open a new file
      datrw::ianystream& open(const int& unit, 
                              const std::string& filename);
      //! return a stream associated with a file unit
      datrw::ianystream& getstream(const int& unit);
      //! check whether a file is opened for this file unit
      bool isopen(const int& unit) const;
      //! close the file associated with this file unit
      void close(const int& unit);
      //! set file format to be used upon file open
      void setformat(const std::string& format);
      //! quick access
      datrw::ianystream& operator()(const int& unit)
      { return(this->getstream(unit)); }
    private:
      //! A C++ stream has to be handled together with an ianystream
      struct IstreamCompound {
        std::istream *Mistream;
        datrw::ianystream *Mianystream;
      }; // struct IstreamCompound
      //! do not allow copying
      IFileUnits(const IFileUnits& ifu);
      //! do not allow copying
      IFileUnits& operator= (const IFileUnits& ifu);
      //! type of my ianystream container
      typedef std::map<int, IstreamCompound> Tstreammap;
      //! file type to be used
      std::string Mformat;
      //! place to hold my ianystream objects
      Tstreammap Mstreammap;
  }; // clas IFileUnits

  /*----------------------------------------------------------------------*/

  /*! A class to provide us with an oanystream
   */
  class OFileUnits {
    public:
      //! default contructor to initialize member data
      OFileUnits();
      //! destructor has to close all open files
      ~OFileUnits();
      //! open a new file
      datrw::oanystream& open(const int& unit, 
                              const std::string& filename);
      //! return a stream associated with a file unit
      datrw::oanystream& getstream(const int& unit);
      //! check whether a file is opened for this file unit
      bool isopen(const int& unit) const;
      //! close the file associated with this file unit
      void close(const int& unit);
      //! set file format to be used upon file open
      void setformat(const std::string& format);
      //! quick access
      datrw::oanystream& operator()(const int& unit)
      { return(this->getstream(unit)); }
    private:
      //! A C++ stream has to be handled together with an oanystream
      struct OstreamCompound {
        std::ostream *Mostream;
        datrw::oanystream *Moanystream;
      }; // struct OstreamCompound
      //! do not allow copying
      OFileUnits(const OFileUnits& ifu);
      //! do not allow copying
      OFileUnits& operator= (const OFileUnits& ifu);
      //! type of my oanystream container
      typedef std::map<int, OstreamCompound> Tstreammap;
      //! file type to be used
      std::string Mformat;
      //! place to hold my oanystream objects
      Tstreammap Mstreammap;
  }; // clas OFileUnits

  /*----------------------------------------------------------------------*/

  //! the global input stream manager
  extern ::fapidxx::IFileUnits istreammanager;

  //! the global input stream manager
  extern ::fapidxx::OFileUnits ostreammanager;

} // namespace fapidxx

#endif // TF_FILEUNIT_H_VERSION (includeguard)

/* ----- END OF fileunit.h ----- */
