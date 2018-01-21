/*! \file cmdlinefiles.cc
 * \brief containers for data files with file specific command line parameters (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 31/01/2007
 * \date 30/01/2014
 * 
 * containers for data files with file specific command line parameters
 * (implementation)
 * 
 * Copyright (c) 2007, 2010, 2011, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/01/2014   V1.0   Thomas Forbriger (thof):
 *                        copied from readtsdata.cc
 * 
 * ============================================================================
 */
#define TSIO_CMDLINEFILES_CC_VERSION \
  "TF_CMDLINEFILES_CC   2014/01/30"

#include <fstream>
#include <tsioxx/cmdlinefiles.h>
#include <tsioxx/inputoperators.h>
#include <tfxx/rangestring.h>

namespace ts {

  namespace sff {

    //! read complete SFF file
    SFile readSSFF(const std::string& filename,
                   const bool& verbose,
                   const std::string& format)
    {
      Ttracelist rangelist;
      return (readSSFF(filename, rangelist, verbose, format));
    }

    //! read complete SFF file
    DFile readDSFF(const std::string& filename,
                   const bool& verbose,
                   const std::string& format)
    {
      Ttracelist rangelist;
      return (readDSFF(filename, rangelist, verbose, format));
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    SFile readSSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose,
                   const std::string& format)
    {
      SFile retval;
      retval.arguments.name=filename;
      if (verbose) 
      { 
        std::cout << "open input file " << filename << std::endl;
      }
      std::ifstream ifs(filename.c_str(), 
                        datrw::ianystream::openmode(format));
      datrw::ianystream is(ifs, format);
      retval.data.read(is.idatstream(), selection, verbose);
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    DFile readDSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose,
                   const std::string& format)
    {
      DFile retval;
      retval.arguments.name=filename;
      if (verbose) 
      { 
        std::cout << "open input file " << filename << std::endl;
      }
      std::ifstream ifs(filename.c_str(),
                        datrw::ianystream::openmode(format));
      datrw::ianystream is(ifs, format);
      retval.data.read(is.idatstream(), selection, verbose);
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    SFile readSSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose,
                   Ttracelistkey tracekey,
                   const std::string& format)
    {
      Ttracelist selection=
        tfxx::string::rangelist<Ttracelist::Tvalue>(filename.value(tracekey));
      SFile retval=readSSFF(filename.name, selection, verbose, format);
      retval.arguments=filename;
      return(retval);
    }

    //! read complete SFF file with trace selection
    DFile readDSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose,
                   Ttracelistkey tracekey,
                   const std::string& format)
    {
      Ttracelist selection=
        tfxx::string::rangelist<Ttracelist::Tvalue>(filename.value(tracekey));
      DFile retval=readDSFF(filename.name, selection, verbose, format);
      retval.arguments=filename;
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    //! read complete list of SFF files with trace selection
    TSFileList readSSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose,
                        Ttracelistkey tracekey,
                        const std::string& format)
    {
      TSFileList retval;
      tfxx::cmdline::Tparsed::const_iterator file=flist.begin();
      while (file != flist.end())
      {
        retval.push_back(readSSFF(*file, verbose, tracekey, format));
        ++file;
      }
      return(retval);
    }

    //! read complete list of SFF files with trace selection
    TDFileList readDSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose,
                        Ttracelistkey tracekey,
                        const std::string& format)
    {
      TDFileList retval;
      tfxx::cmdline::Tparsed::const_iterator file=flist.begin();
      while (file != flist.end())
      {
        retval.push_back(readDSFF(*file, verbose, tracekey, format));
        ++file;
      }
      return(retval);
    }

    /*======================================================================*/

    //! read complete SFF file
    SFile readSSFF(const std::string& filename,
                   const bool& verbose,
                   const datrw::Eformat& format)
    {
      return (readSSFF(filename, verbose, datrw::anyID(format)));
    }

    //! read complete SFF file
    DFile readDSFF(const std::string& filename,
                   const bool& verbose,
                   const datrw::Eformat& format)
    {
      return (readDSFF(filename, verbose, datrw::anyID(format)));
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    SFile readSSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose,
                   const datrw::Eformat& format)
    {
      return (readSSFF(filename, selection, verbose, datrw::anyID(format)));
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    DFile readDSFF(const std::string& filename,
                   const Ttracelist& selection,
                   const bool& verbose,
                   const datrw::Eformat& format)
    {
      return (readDSFF(filename, selection, verbose, datrw::anyID(format)));
    }

    /*----------------------------------------------------------------------*/

    //! read complete SFF file with trace selection
    SFile readSSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose,
                   Ttracelistkey tracekey,
                   const datrw::Eformat& format)
    {
      return (readSSFF(filename, verbose, tracekey, datrw::anyID(format)));
    }

    //! read complete SFF file with trace selection
    DFile readDSFF(const tfxx::cmdline::Filename& filename,
                   const bool& verbose,
                   Ttracelistkey tracekey,
                   const datrw::Eformat& format)
    {
      return (readDSFF(filename, verbose, tracekey, datrw::anyID(format)));
    }

    /*----------------------------------------------------------------------*/

    //! read complete list of SFF files with trace selection
    TSFileList readSSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose,
                        Ttracelistkey tracekey,
                        const datrw::Eformat& format)
    {
      return (readSSFF(flist, verbose, tracekey, datrw::anyID(format)));
    }

    //! read complete list of SFF files with trace selection
    TDFileList readDSFF(const tfxx::cmdline::Tparsed& flist,
                        const bool& verbose,
                        Ttracelistkey tracekey,
                        const datrw::Eformat& format)
    {
      return (readDSFF(flist, verbose, tracekey, datrw::anyID(format)));
    }

  } // namespace sff
} // namespace ts

/* ----- END OF cmdlinefiles.cc ----- */
