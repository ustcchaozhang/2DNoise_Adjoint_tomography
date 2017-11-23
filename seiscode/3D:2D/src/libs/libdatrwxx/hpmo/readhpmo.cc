/*! \file readhpmo.cc
 * \brief read data from Walter Grossmann file format (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * read data from Walter Grossmann file format (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 31/03/2004   V1.0   Thomas Forbriger
 *  - 19/07/2010          Daniel Armbruster header <stdlib.h> added
 * ============================================================================
 */
#define DATRW_READHPMO_CC_VERSION \
  "DATRW_READHPMO_CC   V1.0   "

#include <datrwxx/error.h>
#include <datrwxx/readhpmo.h>
#include<iostream>
#include<sstream>
#include <stdlib.h>

namespace datrw {

  namespace hpmo {

    void Header::readheader(std::istream& is, const bool& verbose)
    {
      std::getline(is, this->Mline);
      if (Mline.length() != 45) { throw NoHeaderException(); }
      if (verbose) { std::cout << Mline << std::endl; }
      std::string secstring=Mline.substr(17,2);
      DATRW_assert((secstring=="57"), "illegal second!");
      std::string timestring;
      const std::string sep("/");
      timestring=Mline.substr(6,4)+sep+Mline.substr(0,2)+sep+
        Mline.substr(3,2)+sep+Mline.substr(11,8);
      if (verbose) { std::cout << timestring << std::endl; }
      Mtime=libtime::TAbsoluteTime(timestring);
      if (verbose) { std::cout << Mtime.timestring() << std::endl; }
      std::string timezone=Mline.substr(23,3);
      if (verbose) { std::cout << timezone << std::endl; }
      DATRW_assert((timezone == std::string("UTC")),
                     "unexpected time code string!");
      std::string errorcode=Mline.substr(34,1);
      if (verbose) { std::cout << errorcode << std::endl; }
      Merrorflag=atoi(errorcode.c_str());
      if (verbose) { std::cout << Merrorflag << std::endl; }
    } // Header::readheader(std::istream& is)

/*----------------------------------------------------------------------*/

    void Header::dump(std::ostream& os) const
    {
      os << "Header: " << this->headerline() << std::endl;
      os << "        " << this->time().timestring() << std::endl;
      os << "        Error flag: " 
        << datrw::hpmo::quality(this->Merrorflag) << std::endl;
    } // void Header::dump(std::ostream& os) const

/*======================================================================*/

    libtime::TRelativeTime dt()
    {
      return(libtime::double2time(datrw::hpmo::sampling_interval));
    } // libtime::TRelativeTime dt() 

/*----------------------------------------------------------------------*/

    libtime::TRelativeTime toffset(const int& ichannel) 
    {
      double delay=(ichannel-1)*0.06;
      delay += 0.01;
      if (ichannel==1) { delay += 0.04; }
      else { delay += 0.41e-3; } 
      return(libtime::double2time(delay));
    } // libtime::TRelativeTime toffset(const int& ichannel) 

/*----------------------------------------------------------------------*/

    void check_channel_no(const int& ichannel)
    {
      DATRW_assert(((ichannel > 0) && (ichannel <= nchannels)),
                     "illegal channel number!");
    } // void check_channel_no(const int& ichannel)

/*----------------------------------------------------------------------*/

    std::string quality(const int& flag) 
    { 
      std::string retval("unknown flag!");
      if (flag == 0) { retval=std::string("OK"); }
      if (flag == 1) { retval=std::string("bad DCF reception"); }
      if (flag == 2) { retval=std::string("dummy block (2.999999 V)"); }
      if (flag == 3) { retval=std::string("bad DCF reception and ")
                             +std::string("dummy block (2.999999 V)"); }
      if (flag == 4) { retval=std::string("RS-232 error"); }
      if (flag == 8) { retval=std::string("possibly error in PC clock"); }
      if (flag == 16) { retval=std::string("invalid header"); }
      return(retval);
    } // std::string quality(const int& flag)

/*----------------------------------------------------------------------*/

    sff::FREE qualityreports(const MinuteBlock* blocks, const int& nblocks)
    {
      sff::FREE retval;
      for (int i=0; i<nblocks; ++i)
      {
        const int& qf=blocks[i].Mquality_flag;
        if (qf != 0)
        {
          std::ostringstream oss;
          oss << qf;
          retval.append("quality flag @ " + blocks[i].Mtime.timestring()
                        + ": " + oss.str());
          retval.append("  this means: " + quality(qf));
        }
      }
      return(retval);
    } // sff::FREE qualityreports(const MinuteBlock* blocks, const int& nblocks)

/*======================================================================*/

    SampleBlock readdata(std::istream& is, const bool& verbose)
    {
      SampleBlock retval;
      int ninsamples=nsamples*nchannels;
      for (int i=0; i<ninsamples; i++) 
      { 
        is >> retval.Msamples[i]; 
        DATRW_assert(is.good(), "ERROR: reading samples!");
      }
      std::string dummy;
      std::getline(is, dummy);
      return(retval);
    } // SampleBlock readdata(std::istream& is, const bool& verbose)

/*----------------------------------------------------------------------*/

    void dump(std::ostream& os, const SampleBlock& block) 
    {
      for (int ic=1; ic<=datrw::hpmo::nchannels; ic++)
      {
        os.width(1);
        os << "K";
        os.setf(std::ios_base::fixed,std::ios_base::basefield);
        os.width(2);
        os.fill('0');
        os << ic << " ";
        for (int is=1; is<=datrw::hpmo::nsamples/2; is++)
        {
          os.flags(std::ios_base::showpos);
          os.setf(std::ios_base::fixed,std::ios_base::floatfield);
          os.width(6);
          os << block(ic, is) << " ";
        }
        os.width(4);
        os << std::endl << "    ";
        for (int is=datrw::hpmo::nsamples/2+1;
             is<=datrw::hpmo::nsamples; is++)
        {
          os.flags(std::ios_base::showpos);
          os.setf(std::ios_base::fixed,std::ios_base::floatfield);
          os.width(6);
          os << block(ic, is) << " ";
        }
        os.unsetf(std::ios_base::showpos);
        os << std::endl;
      }
    } // void dump(std::ostream& os, const SampleBlock& block)

/*======================================================================*/

    void dump(std::ostream& os, const MinuteBlock& block) 
    {
      os << block.Mtime.timestring() << std::endl;
      os << "quality: " << block.Mquality_flag << " "
        << quality(block.Mquality_flag) << std::endl;
      os << block.Mdata;
    } // void Samples::dump(std::ostream& os) const

/*======================================================================*/

    std::istream& operator >> (std::istream& is, MinuteBlock& block)
    { 
      datrw::hpmo::Header header(is);
      block.Mtime=header.time();
      block.Mquality_flag=header.errorflag();
      is >> block.Mdata; 
      return(is); 
    }

/*----------------------------------------------------------------------*/

    Tvecofblocks readfile(std::istream& is, const bool& verbose)
    {
      Tvecofblocks retval;
      bool hot=true;
      MinuteBlock block;
      retval.clear();
      if (verbose) { std::cout << "extracting block # "; }
      int iblock=0;
      while (is.good() && (hot))
      {
        bool rocflag=datrw::Exception::report_on_construct_flag();
        Exception::dont_report_on_construct();
        try { is >> block; }
        catch ( NoHeaderException )
        { hot=false; }
        catch ( Exception& e )
        {
          e.report();
          std::cerr << "ERROR (datrw::hpmo::read): "
            << "after reading " << retval.size() << " blocks"
            << std::endl;
          if (retval.size()>0)
          {
            std::cerr << "  last successfull block @ " 
              << retval[retval.size()-1].Mtime.timestring() << std::endl;
          }
          throw;
        }
        datrw::Exception::report_on_construct_flag(rocflag);
        if (hot)
        {
          retval.push_back(block);
          if (verbose) { std::cout << ++iblock << " "; }
        }
      }
      if (verbose) { std::cout << std::endl; }
      return(retval);
    } // Tvecofblocks readfile(std::istream& is, const bool& verbose)
    
  } // namespace hpmo

} // namespace datrw

/* ----- END OF readhpmo.cc ----- */
