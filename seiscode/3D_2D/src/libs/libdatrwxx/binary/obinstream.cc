/*! \file obinstream.cc
 * \brief basic binary output stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/11/2011
 * 
 * basic binary output stream (implementation)
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
 *  - 10/11/2011   V1.0   Thomas Forbriger
 *  - 31/01/2012   V1.1   use precisely the same variable type for
 *                        TAbsoluteTime input and output
 * 
 * ============================================================================
 */
#define DATRW_OBINSTREAM_CC_VERSION \
  "DATRW_OBINSTREAM_CC   V1.1"

#include <datrwxx/obinstream.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/bytesex.h>

namespace datrw {

  namespace binary {

    obinstream::obinstream(std::ostream& os,
                           const char* const magic,
                           const bool& debug):
      Mos(os), Mdebug(debug)
    {
      ::datrw::util::file_magic_write(Mos, magic, false);
    } // obinstream::obinstream(std::ofstream& is,

    /*----------------------------------------------------------------------*/

    void obinstream::write(const char& v)
    {
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(char));
    } // void obinstream::write(const char& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const short& v)
    {
      DATRW_debug(Mdebug, "obinstream::write(const short& v)", "v=" << v);
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(short));
    } // void obinstream::write(const short& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const int& v)
    {
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(int));
    } // void obinstream::write(const int& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const unsigned int& v)
    {
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(unsigned int));
    } // void obinstream::write(const unsigned int& v)

    /*----------------------------------------------------------------------*/
    void obinstream::write(const double& v)
    {
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(double));
    } // void obinstream::write(const double& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const float& v)
    {
      Mos.write(reinterpret_cast<const char *>(&v), sizeof(float));
    } // void obinstream::write(const float& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const std::string& v)
    {
      unsigned int length=v.length();
      this->write(length);
      Mos.write(reinterpret_cast<const char *>(v.c_str()), 
                length*sizeof(char));
    } // void obinstream::write(const std::string& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const libtime::TAbsoluteTime& v)
    {
      this->write(static_cast<unsigned int>(v.year()));
      this->write(static_cast<char>(v.month()));
      this->write(static_cast<char>(v.day()));
      this->write(static_cast<char>(v.hour()));
      this->write(static_cast<char>(v.minute()));
      this->write(static_cast<char>(v.second()));
      this->write(static_cast<short>(v.milsec()));
      this->write(static_cast<short>(v.micsec()));
    } // void obinstream::write(const libtime::TAbsoluteTime& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const ::sff::FREE& v)
    {
      const ::sff::FREE::Tlines& lines=v.lines;
      unsigned int nlines=lines.size();
      this->write(nlines);
      ::sff::FREE::Tlines::const_iterator I=lines.begin();
      while (I!=lines.end())
      {
        this->write(*I);
        ++I;
      }
    } // void obinstream::write(const ::sff::FREE& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const ::sff::WID2& v)
    {
      this->write(v.date);
      this->write(v.dt);
      this->write(v.nsamples);
      this->write(v.station);
      this->write(v.channel);
      this->write(v.auxid);
      this->write(v.instype);
      this->write(v.calib);
      this->write(v.calper);
      this->write(v.hang);
      this->write(v.vang);
    } // void obinstream::write(const ::sff::WID2& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const ::sff::SRCE& v)
    {
      this->write(v.date);
      this->write(v.type);
      char cs=::sff::coosysID(v.cs);
      this->write(cs);
      this->write(v.cx);
      this->write(v.cy);
      this->write(v.cz);
    } // void obinstream::write(const ::sff::SRCE& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const ::sff::INFO& v)
    {
      char cs=::sff::coosysID(v.cs);
      this->write(cs);
      this->write(v.cx);
      this->write(v.cy);
      this->write(v.cz);
      this->write(v.nstacks);
    } // void obinstream::write(const ::sff::INFO& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const Tdseries::Tcoc& v)
    {
      int first=v.f();
      int last=v.l();
      this->write(first);
      this->write(last);
      Mos.write(reinterpret_cast<const char *>(v.pointer()),
                v.size()*sizeof(Tdseries::Tvalue));
    } // void obinstream::write(const Tdseries::Tcoc& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const Tfseries::Tcoc& v)
    {
      int first=v.f();
      int last=v.l();
      this->write(first);
      this->write(last);
      Mos.write(reinterpret_cast<const char *>(v.pointer()),
                v.size()*sizeof(Tfseries::Tvalue));
    } // void obinstream::write(const Tfseries::Tcoc& v)

    /*----------------------------------------------------------------------*/

    void obinstream::write(const Tiseries::Tcoc& v)
    {
      int first=v.f();
      int last=v.l();
      this->write(first);
      this->write(last);
      Mos.write(reinterpret_cast<const char *>(v.pointer()),
                v.size()*sizeof(Tiseries::Tvalue));
    } // void obinstream::write(const Tiseries::Tcoc& v)

  } // namespace binary

} // namespace datrw

/* ----- END OF obinstream.cc ----- */
