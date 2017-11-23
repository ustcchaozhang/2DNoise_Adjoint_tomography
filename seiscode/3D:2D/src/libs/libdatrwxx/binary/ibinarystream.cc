/*! \file ibinarystream.cc
 * \brief input raw binary data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/10/2011
 * 
 * input raw binary data (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 18/10/2011   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_IBINARYSTREAM_CC_VERSION \
  "DATRW_IBINARYSTREAM_CC   V1.1"

#include<datrwxx/binary.h>
#include<datrwxx/error.h>
#include<datrwxx/util.h>
#include<datrwxx/debug.h>

namespace datrw {

  const std::ios_base::openmode
    ibinarystream::openmode=std::ios_base::in|std::ios_base::binary;

  /*----------------------------------------------------------------------*/

  ibinarystream::ibinarystream(std::istream& is, 
                               const bool& debug):
    Tbase(is, true, true, true, debug), 
    Mibs(is, ::datrw::binary::magic, debug)
  { 
    char flags;
    Mibs >> Mversion;
    DATRW_debug(Mdebug, "ibinarystream::ibinarystream",
                "file version " << Mversion);
    if (Mversion != datrw::binary::version)
    {
      std::cerr << "*** library version: " << datrw::binary::version <<
        std::endl;
      std::cerr << "*** file version: " << Mversion << std::endl;
    }
    DATRW_assert(Mversion == datrw::binary::version,
                 "ERROR: file version does not match library version");
    Mibs >> flags;
    binary::checkfileflags(flags);
    if (flags & ::datrw::binary::Fsrce)
    {
      DATRW_debug(Mdebug, "ibinarystream::ibinarystream",
                  "reading SRCE line");
      ::sff::SRCE srce;
      Mibs >> srce;
      this->setsrce(srce);
    }
    if (flags & ::datrw::binary::Ffree)
    {
      ::sff::FREE free;
      Mibs >> free;
      this->setfilefree(free);
      DATRW_debug(Mdebug, "ibinarystream::ibinarystream",
                  "reading file FREE block");
    }
    Mibs >> Mnextflags;
    DATRW_assert(Mis.good(),
                 "ERROR: could not read behind file header");
    binary::checkfileflags(flags);
  } // ibinarystream::ibinarystream

  /*----------------------------------------------------------------------*/

  void ibinarystream::readflags()
  {
    DATRW_debug(Mdebug, "ibinarystream::readflags()",
                "Mis.good(): " << Mis.good());
    bool rocflag=datrw::Exception::report_on_construct_flag();
    datrw::Exception::dont_report_on_construct();
    try {
      Mibs >> Mnextflags;
    } catch (datrw::Exception)
    {
      this->setlast();
    }
    datrw::Exception::report_on_construct_flag(rocflag);
    DATRW_debug(Mdebug, "ibinarystream::readflags()",
                "Mis.good(): " << Mis.good());
    if (!Mis.good()) 
    {
      this->setlast(); 
    }
    if (!this->last()) { binary::checktraceflags(Mnextflags); }
  }

  /*----------------------------------------------------------------------*/

  void ibinarystream::readheader()
  { 
    this->newtrace();
    ::sff::WID2 wid2;
    Mibs >> wid2;
    this->setwid2(wid2);
    if (Mnextflags & ::datrw::binary::Finfo)
    {
      ::sff::INFO info;
      Mibs >> info;
      this->setinfo(info);
    }
    if (Mnextflags & ::datrw::binary::Ffree)
    {
      ::sff::FREE free;
      Mibs >> free;
      this->settracefree(free);
    }
  } // void ibinarystream::readheader()

  /*----------------------------------------------------------------------*/

  namespace binary {

    namespace {

      /*! read a sequence of samples
       *
       * \param T type of sample value (double, float, int)
       *
       * \param is raw binary input stream to read from
       * \param series container to write samples to
       * \param flags indicating data type
       */
      template<typename T>
        void readany(ibinstream& is,
                     typename aff::Series<T>& series,
                     const char& flags)
        {
          // typedef typename aff::Series<T> Tinseries;
          if (flags & datrw::binary::Fdouble)
          {
            Tdseries inseries;
            is >> inseries;
            util::convert(inseries, series);
          }
          else if (flags & datrw::binary::Ffloat)
          {
            Tfseries inseries;
            is >> inseries;
            util::convert(inseries, series);
          }
          else if (flags & datrw::binary::Fint)
          {
            Tiseries inseries;
            is >> inseries;
            util::convert(inseries, series);
          }
          else
          {
            std::cerr << "flags: " << int(flags) << std::endl;
            DATRW_abort("unkown data type!");
          }
        }

    } // namespace

  } // namespace binary

  /*----------------------------------------------------------------------*/

  Tdseries ibinarystream::dseries()
  {
    this->readheader();
    Tdseries retval;
    datrw::binary::readany(Mibs, retval, Mnextflags);
    this->setnsamples(retval.size());
    this->readflags();
    return(retval);
  } // Tdseries ibinarystream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries ibinarystream::fseries()
  {
    this->readheader();
    Tfseries retval;
    datrw::binary::readany(Mibs, retval, Mnextflags);
    this->setnsamples(retval.size());
    this->readflags();
    return(retval);
  } // Tfseries ibinarystream::fseries()

  /*----------------------------------------------------------------------*/

  Tiseries ibinarystream::iseries()
  {
    this->readheader();
    Tiseries retval;
    datrw::binary::readany(Mibs, retval, Mnextflags);
    this->setnsamples(retval.size());
    this->readflags();
    return(retval);
  } // Tiseries ibinarystream::iseries()

  /*----------------------------------------------------------------------*/

  void ibinarystream::skipseries()
  {
    unsigned int nsamples;
    this->readheader();
    if (Mnextflags & ::datrw::binary::Fdouble)
    {
      nsamples=Mibs.skipdseries();
    }
    else if (Mnextflags & ::datrw::binary::Ffloat)
    {
      nsamples=Mibs.skipfseries();
    }
    else if (Mnextflags & ::datrw::binary::Fint)
    {
      nsamples=Mibs.skipiseries();
    }
    else
    {
      DATRW_abort("ERROR (ibinarystream::skipseries): data type not indicated");
    }
    this->setnsamples(nsamples);
    this->readflags();
  } // void ibinarystream::skipseries()

  /*----------------------------------------------------------------------*/

  void ibinarystream::setnsamples(const unsigned int& nsamples)
  {
    ::sff::WID2 wid2=this->wid2();
    wid2.nsamples=nsamples;
    this->setwid2(wid2);
  } // void ibinarystream::setnsamples(const unsigned int& nsamples)

  /*----------------------------------------------------------------------*/

  void ibinarystream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "BINARY reading functions" << std::endl <<
      "------------------------" << std::endl <<
      DATRW_IBINARYSTREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module reads binary data.\n"
      "See the output module for further comments.\n"
      << std::endl;
  } // void ibinarystream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF ibinarystream.cc ----- */
