/*! \file ibinstream.cc
 * \brief basic binary input stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/11/2011
 * 
 * basic binary input stream (implementation)
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
#define DATRW_IBINSTREAM_CC_VERSION \
  "DATRW_IBINSTREAM_CC   V1.1"

#include <datrwxx/ibinstream.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/bytesex.h>

namespace datrw {

  namespace binary {

    ibinstream::ibinstream(std::istream& is,
                           const char* const magic,
                           const bool& debug):
      Mis(is), Mdebug(debug)
    {
      ::datrw::util::Emagic_type type;
      type=::datrw::util::file_magic_test(Mis, magic, false);
      DATRW_assert(type!=::datrw::util::magic_nomatch,
                   "magic number in file does not match!");
      Mswap=(type==::datrw::util::magic_swap);
    } // ibinstream::ibinstream(std::ifstream& is

    /*----------------------------------------------------------------------*/

    void ibinstream::read(char& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v), sizeof(char)),
                  "ERROR (ibinstream::read): reading char value");
    } // void ibinstream::read(char& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(short& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v), sizeof(short)),
                  "ERROR (ibinstream::read): reading short value");
      if (Mswap) { v=::datrw::util::swap(v); }
      DATRW_debug(Mdebug, "ibinstream::read(short& v)", "v=" << v);
    } // void ibinstream::read(short& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(int& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v), sizeof(int)),
                  "ERROR (ibinstream::read): reading integer value");
      if (Mswap) { v=::datrw::util::swap(v); }
    } // void ibinstream::read(int& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(unsigned int& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v),
                            sizeof(unsigned int)),
                  "ERROR (ibinstream::read): reading unsigned int value");
      if (Mswap) { v=::datrw::util::swap(v); }
    } // void ibinstream::read(unsigned int& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(double& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v), sizeof(double)),
                  "ERROR (ibinstream::read): reading double value");
      if (Mswap) { v=::datrw::util::swap(v); }
    } // void ibinstream::read(double& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(float& v)
    {
      DATRW_assert(Mis.read(reinterpret_cast<char *>(&v), sizeof(float)),
                  "ERROR (ibinstream::read): reading float value");
      if (Mswap) { v=::datrw::util::swap(v); }
    } // void ibinstream::read(float& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(std::string& v)
    {
      unsigned int length;
      this->read(length);
      aff::Series<char> s(length+1);
      DATRW_assert(Mis.read(reinterpret_cast<char *>(s.pointer()),
                            length*sizeof(char)),
                  "ERROR (ibinstream::read): reading string");
      s(s.l())='\0';
      v=std::string(s.pointer());
    } // void ibinstream::read(std::string& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(libtime::TAbsoluteTime& v)
    {
      unsigned int year;
      char month, day, hour, minute, second;
      short milsec, micsec;
      this->read(year);
      this->read(month);
      this->read(day);
      this->read(hour);
      this->read(minute);
      this->read(second);
      this->read(milsec);
      this->read(micsec);
      v=libtime::TAbsoluteTime(year, month, day, hour, minute, second,
                               milsec, micsec);
    } // void ibinstream::read(libtime::TAbsoluteTime& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(::sff::FREE& v)
    {
      unsigned int nlines;
      this->read(nlines);
      for (unsigned int i=0; i<nlines; ++i)
      {
        std::string s;
        this->read(s);
        v.append(s);
      }
    } // void ibinstream::read(::sff::FREE& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(::sff::WID2& v)
    {
      this->read(v.date);
      this->read(v.dt);
      this->read(v.nsamples);
      this->read(v.station);
      this->read(v.channel);
      this->read(v.auxid);
      this->read(v.instype);
      this->read(v.calib);
      this->read(v.calper);
      this->read(v.hang);
      this->read(v.vang);
    } // void ibinstream::read(::sff::WID2& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(::sff::SRCE& v)
    {
      this->read(v.date);
      this->read(v.type);
      char cs;
      this->read(cs);
      v.cs=::sff::coosysID(cs);
      this->read(v.cx);
      this->read(v.cy);
      this->read(v.cz);
    } // void ibinstream::read(::sff::SRCE& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(::sff::INFO& v)
    {
      char cs;
      this->read(cs);
      v.cs=::sff::coosysID(cs);
      this->read(v.cx);
      this->read(v.cy);
      this->read(v.cz);
      this->read(v.nstacks);
    } // void ibinstream::read(::sff::INFO& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(Tdseries& v)
    {
      int first, last;
      this->read(first);
      this->read(last);
      v=Tdseries(first,last);
      DATRW_assert(Mis.read(reinterpret_cast<char *>(v.pointer()),
                            v.size()*sizeof(Tdseries::Tvalue)),
                  "ERROR (ibinstream::read): reading double series");
      if (Mswap)
      {
        aff::Iterator<Tdseries> I(v);
        while (I.valid())
        {
          *I=::datrw::util::swap(*I);
          ++I;
        }
      }
    } // void ibinstream::read(Tdseries& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(Tfseries& v)
    {
      int first, last;
      this->read(first);
      this->read(last);
      v=Tfseries(first,last);
      DATRW_assert(Mis.read(reinterpret_cast<char *>(v.pointer()),
                            v.size()*sizeof(Tfseries::Tvalue)),
                  "ERROR (ibinstream::read): reading float series");
      if (Mswap)
      {
        aff::Iterator<Tfseries> I(v);
        while (I.valid())
        {
          *I=::datrw::util::swap(*I);
          ++I;
        }
      }
    } // void ibinstream::read(Tfseries& v)

    /*----------------------------------------------------------------------*/

    void ibinstream::read(Tiseries& v)
    {
      int first, last;
      this->read(first);
      this->read(last);
      v=Tiseries(first,last);
      DATRW_assert(Mis.read(reinterpret_cast<char *>(v.pointer()),
                            v.size()*sizeof(Tiseries::Tvalue)),
                  "ERROR (ibinstream::read): reading integer series");
      if (Mswap)
      {
        aff::Iterator<Tiseries> I(v);
        while (I.valid())
        {
          *I=::datrw::util::swap(*I);
          ++I;
        }
      }
    } // void ibinstream::read(Tiseries& v)

    /*----------------------------------------------------------------------*/

    unsigned int ibinstream::skipdseries()
    {
      int first, last;
      this->read(first);
      this->read(last);
      DATRW_assert(last>=first,
                   "ERROR (ibinstream::skipdseries): unreasonable index range");
      unsigned int size=last-first+1;
      Mis.seekg(size*sizeof(Tdseries::Tvalue), Mis.cur);
      return(size);
    } // unsigned int ibinstream::skipdseries()

    /*----------------------------------------------------------------------*/

    unsigned int ibinstream::skipfseries()
    {
      int first, last;
      this->read(first);
      this->read(last);
      DATRW_assert(last>=first,
                   "ERROR (ibinstream::skipfseries): unreasonable index range");
      unsigned int size=last-first+1;
      Mis.seekg(size*sizeof(Tfseries::Tvalue), Mis.cur);
      return(size);
    } // unsigned int ibinstream::skipfseries()

    /*----------------------------------------------------------------------*/

    unsigned int ibinstream::skipiseries()
    {
      int first, last;
      this->read(first);
      this->read(last);
      DATRW_assert(last>=first,
                   "ERROR (ibinstream::skipiseries): unreasonable index range");
      unsigned int size=last-first+1;
      Mis.seekg(size*sizeof(Tiseries::Tvalue), Mis.cur);
      return(size);
    } // unsigned int ibinstream::skipiseries()

  } // namespace binary

} // namespace datrw

/* ----- END OF ibinstream.cc ----- */
