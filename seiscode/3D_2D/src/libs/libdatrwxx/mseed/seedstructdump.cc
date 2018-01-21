/*! \file seedstructdump.cc
 * \brief dump SEED structs in human readable form (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/03/2006
 * 
 * dump SEED structs in human readable form (implementation)
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
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 15/03/2006   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_SEEDSTRUCTDUMP_CC_VERSION \
  "DATRW_SEEDSTRUCTDUMP_CC   V1.0   "

#include<string>
#include<iomanip>
#include<datrwxx/seedstructdump.h>
#include<datrwxx/mseedread.h>

namespace datrw {

  namespace mseed {

    namespace SEED {

      namespace util {

        void print_field_name(const char* f, std::ostream& os)
        {
          os.width(30);
          os.fill(' ');
          os << f << ": ";
        } // void print_field_name(const char* f, std::ostream& os)

        void print_hex(const unsigned short int& f, std::ostream& os)
        {
          std::ostream::fmtflags flags=os.flags();
          os.setf(std::ios_base::hex, std::ios_base::basefield);
          os.setf(std::ios_base::showbase);
          os << f;
          os.flags(flags);
        } // void print_address(const unsigned short int& f, std::ostream& os)
                              
      } // namespace util

      void dump(const ControlHeader& s, std::ostream& os)
      {
        os << "Control Header:" << std::endl;
        util::print_field_name("sequence no", os);
        os << std::string(s.seqno, 6);
        os << std::endl;
        util::print_field_name("type", os);
        os << s.type;
        os << std::endl;
        util::print_field_name("cont", os);
        os << s.cont;
        os << std::endl;
      } // void dump(const ControlHeader& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const BlocketteHeader& s, std::ostream& os)
      {
      } // void dump(const BlocketteHeader& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const BTIME& s, std::ostream& os)
      {
        libtime::TAbsoluteTime thetime=convert(s);
        os << thetime.timestring();
      } // void dump(const BTIME& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const DataExtensionBlockette& s, std::ostream& os)
      {
        os << "Data Extension Blockette:" << std::endl;
        dump(s.blocketteheader, os);
        util::print_field_name("timing quality", os);
        os << s.itquality() << std::endl;
        util::print_field_name("microseconds", os);
        os << s.iusec() << std::endl;
        util::print_field_name("reserved", os);
        os << int(s.reserved) << std::endl;
        util::print_field_name("frame count", os);
        os << s.ifcount() << std::endl;
      } // void dump(const DataExtensionBlockette& s, std::ostream& os)

      /*----------------------------------------------------------------------*/
      
      void dump(const DataOnlySEEDBlockette& s, std::ostream& os)
      {
        os << "Data Only SEED Blockette:" << std::endl;
        dump(s.blocketteheader, os);
        util::print_field_name("encoding format", os);
        dump(EEncodingFormat(s.format), os); 
        os << " (" << int(s.format) << ")" << std::endl;
        util::print_field_name("word order", os);
        dump(EByteOrder(s.bytesex), os); 
        os << " (" << s.ibytesex() << ")" << std::endl;
        util::print_field_name("data record length", os);
        os << s.reclenbytes() 
           << " bytes (" << s.ireclen() << ")" << std::endl;
        util::print_field_name("reserved", os);
        os << int(s.reserved) << std::endl;
      } // void dump(const DataOnlySEEDBlockette& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const DataRecordBlocketteHeader& s, std::ostream& os)
      {
        os << "Data Record Blockette Header:" << std::endl;
        util::print_field_name("blockette type", os);
        os << s.type << std::endl;
        util::print_field_name("address of next blockette", os);
        util::print_hex(s.next, os); os << std::endl;
      } // void dump(const DataRecordBlocketteHeader& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const FixedDataRecordHeader& s, std::ostream& os)
      {
        os << "Fixed Data Record Header:" << std::endl;
        dump(s.controlheader, os);
        util::print_field_name("station identifier code", os);
        os << datrw::mseed::util::extractstring(s.stat, 5) << std::endl;
        util::print_field_name("location identifier", os);
        os << datrw::mseed::util::extractstring(s.loc, 2) << std::endl;
        util::print_field_name("channel identifier", os);
        os << datrw::mseed::util::extractstring(s.chan, 3) << std::endl;
        util::print_field_name("network code", os);
        os << datrw::mseed::util::extractstring(s.net, 2) << std::endl;
        util::print_field_name("record start time", os);
        os << convert(s.stime).timestring() << std::endl;
        util::print_field_name("number of samples", os);
        os << s.nsamp << std::endl;
        util::print_field_name("sample rate factor", os);
        os << s.srate << std::endl;
        util::print_field_name("sample rate multiplier", os);
        os << s.srmult << std::endl;
        util::print_field_name("activity flags", os);
        util::print_hex(s.aflags, os);
        os << std::endl;
        dump_aflags(s.aflags, os);
        util::print_field_name("i/o and clock flags", os);
        util::print_hex(s.ioflags, os);
        os << std::endl;
        dump_ioflags(s.ioflags, os);
        util::print_field_name("data quality flags", os);
        util::print_hex(s.qflags, os);
        os << std::endl;
        dump_qflags(s.qflags, os);
        util::print_field_name("number of blockettes", os);
        os << int(s.numblock) << std::endl;
        util::print_field_name("time correction", os);
        os << s.tcorr << std::endl;
        util::print_field_name("beginning of data", os);
        util::print_hex(s.dbeg, os);
        os << std::endl;
        util::print_field_name("first blockette", os);
        util::print_hex(s.fblock, os);
        os << std::endl;
      } // void dump(const FixedDataRecordHeader& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump_aflags(const unsigned char& s, std::ostream& os)
      {
        if (int(s) > 0) { dump(datrw::mseed::SEED::ActivityFlags(s), os); }
      } // void dump_aflags(const unsigned char& s, std::ostream& os)

      void dump_qflags(const unsigned char& s, std::ostream& os)
      {
        if (int(s) > 0) { dump(datrw::mseed::SEED::QualityFlags(s), os); }
      } // void dump_qflags(const unsigned char& s, std::ostream& os)

      void dump_ioflags(const unsigned char& s, std::ostream& os)
      {
        if (int(s) > 0) { dump(datrw::mseed::SEED::IOFlags(s), os); }
      } // void dump_ioflags(const unsigned char& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const datrw::mseed::SEED::ActivityFlags& f, std::ostream& os)
      {
        os << "Activity Flags:" << std::endl;
        if (f.calpres) 
        { os << "  calibration signals present" << std::endl; }
        if (f.tcorrapp) { os << "  time correction applied" << std::endl; }
        if (f.begevent) { os << "  beginning of event" << std::endl; }
        if (f.endevent) { os << "  end of event" << std::endl; }
        if (f.posleap) 
        { os << "  a positive leap second happened" << std::endl; }
        if (f.negleap) 
        { os << "  a negative leap second happened" << std::endl; }
        if (f.event) { os << "  event in progress" << std::endl; }
      } // void dump(const ActivityFlags& f, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const datrw::mseed::SEED::QualityFlags& f, std::ostream& os)
      {
        os << "Quality Flags:" << std::endl;
        if (f.ampsat) 
        { os << "  amplifier saturation detected" << std::endl; }
        if (f.clip) { os << "  digitizer clipping detected" << std::endl; }
        if (f.spike) { os << "  spike detected" << std::endl; }
        if (f.glitch) { os << "  glitches detected" << std::endl; }
        if (f.miss) 
        { os << "  missing/padded data present" << std::endl; }
        if (f.telsynch) 
        { os << "  telemetry synchronization error" << std::endl; }
        if (f.charging) 
        { os << "  a digital filter may be charging" << std::endl; }
        if (f.time) { os << "  time tag is questionable" << std::endl; }
      } // void dump(const QualityFlags& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const datrw::mseed::SEED::IOFlags& f, std::ostream& os)
      {
        os << "I/O Flags:" << std::endl;
        if (f.parityerr) 
        {
          os << "  station volume parity error possibly present" 
             << std::endl; 
        }
        if (f.longrec) { os << "  long record read" << std::endl; }
        if (f.shortrec) { os << "  short record read" << std::endl; }
        if (f.start) { os << "  start of time series" << std::endl; }
        if (f.end) { os << "  end of time series" << std::endl; }
        if (f.locked) { os << "  clock locked" << std::endl; }
      } // void dump(const IOFlags& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const EEncodingFormat& s, std::ostream& os)
      {
        switch (s) {
          case ascii:
            os << "ASCII"; break;
          case int16:
            os << "16 bit integer"; break;
          case int24:
            os << "24 bit integer"; break;
          case int32:
            os << "32 bit integer"; break;
          case ieeefp:
            os << "IEEE floating point"; break;
          case ieeedp:
            os << "IEEE double precision floating point"; break;
          case steim1:
            os << "Steim (1) compression"; break;
          case steim2:
            os << "Steim (2) compression"; break;
          case geoscope24:
            os << "GEOSCOPE multiplexed 24 bit integer"; break;
          case geoscope163:
            os << "GEOSCOPE multiplexed 16 bit gain ranged 3 bit exponent"; 
            break;
          case geoscope164:
            os << "GEOSCOPE multiplexed 16 bit gain ranged 4 bit exponent"; 
            break;
          case us:
            os << "US national network compression"; break;
          case cdsn:
            os << "CDSN 16 bit gain ranged"; break;
          case grf:
            os << "Graefenberg 16 bit gain ranged"; break;
          case ipg:
            os << "IPG Strasbourg 16 bit gain ranged"; break;
          case steim3:
            os << "Steim (3) compression"; break;
          case sro:
            os << "SRO format"; break;
          case hglp:
            os << "HGLP format"; break;
          case dwwssn:
            os << "DWWSSN gain ranged"; break;
          case rstn:
            os << "RSTN 16 bit gain ranged"; break;
          default:
            os << "unknown"; 
        }
      } // void dump(const EEncodingFormat& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(const EByteOrder& s, std::ostream& os)
      {
        switch (s) {
          case vax8086:
            os << "VAX or 8086"; 
            break;
          case sparc68000:
            os << "SPARC or 68000"; 
            break;
          default:
            os << "unknown"; 
        }
      } // void dump(const EByteOrder& s, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(Steim1Frame f, std::ostream& os)
      {
        os << "Steim 1 Frame:" << std::endl;
        os << "  control word: 0x";
        std::ostream::fmtflags flags=os.flags();
        os.setf(std::ios_base::hex, std::ios_base::basefield);
        // os.setf(std::ios_base::showbase);
        os.width(8);
        os.fill('0');
        os << f.control();
        os.flags(flags);
        os << std::endl;
        /*
        for (int i=0; i<Steim1Frame::nelements; ++i)
        {
          os << "  ctrl #";
          os.width(2);
          os.fill('0');
          os << i << ": " << f.ctrl(i) << std::endl;
        }
        */
        os << "  data: " << std::endl;
        for (int i=0; i<Steim1Frame::nwords; ++i)
        {
          Steim1Word w;
          w.fw=f.word(i);
          switch (f.ctrl(i)) {
            case Steim1Frame::Fspecial:
              os << "  #";
              os.width(2);
              os.fill('0');
              os << i <<   " special (" << f.ctrl(i) << "):     "  
                << w.fw << std::endl;
              break;
            case Steim1Frame::Fbyte:
              for (int j=0; j<4; ++j)
              {
                os << "  #";
                os.width(2);
                os.fill('0');
                os << i << " byte (" << f.ctrl(i) << ") #" << j << ":     " 
                  << int(w.byte[j]) << std::endl;
              }
              break;
            case Steim1Frame::Fhw:
              for (int j=0; j<2; ++j)
              {
                os << "  #";
                os.width(2);
                os.fill('0');
                os << i << " halfword (" << f.ctrl(i) << ") #" << j << ": " 
                  << w.hw[j] << std::endl;
              }
              break;
            case Steim1Frame::Ffw:
              os << "  #";
              os.width(2);
              os.fill('0');
              os << i <<   " fullword (" << f.ctrl(i) << "):   " 
                << w.fw << std::endl;
              break;
          }
        }
      } // void dump(Steim1Frame f, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(Steim2Frame f, std::ostream& os)
      {
        os << "Steim 2 Frame:" << std::endl;
        os << "  control word: 0x";
        std::ostream::fmtflags flags=os.flags();
        os.setf(std::ios_base::hex, std::ios_base::basefield);
        // os.setf(std::ios_base::showbase);
        os.width(8);
        os.fill('0');
        os << f.control();
        os.flags(flags);
        os << std::endl;
        /*
        for (int i=0; i<Steim1Frame::nelements; ++i)
        {
          os << "  ctrl #";
          os.width(2);
          os.fill('0');
          os << i << ": " << f.ctrl(i) << std::endl;
        }
        */
        os << "  data: " << std::endl;
        for (int i=0; i<Steim2Frame::nwords; ++i)
        {
          Steim1Word w;
          w.fw=f.word(i);
          switch (f.ctrl(i)) {
            case Steim2Frame::Fspecial:
              os << "  #";
              os.width(2);
              os.fill('0');
              os << i <<   " special (" << f.ctrl(i) << "):     "  
                << w.fw << std::endl;
              break;
            case Steim2Frame::Fbyte:
              for (int j=0; j<4; ++j)
              {
                os << "  #";
                os.width(2);
                os.fill('0');
                os << i << " byte (" << f.ctrl(i) << ") #" << j << ":     " 
                  << int(w.byte[j]) << std::endl;
              }
              break;
            case Steim1Frame::Fdnib1:
            case Steim1Frame::Fdnib2:
              Steim2Word word(Steim2Word::ESteim2Control(f.ctrl(i)), w.fw);
              for (int j=0; j<word.nval(); ++j)
              {
                os << "  #";
                os.width(2);
                os.fill('0');
                os << i;
                os << " c=";
                os.width(2);
                os.fill(' ');
                os << f.ctrl(i);
                os << " dnib=";
                os.width(2);
                os.fill(' ');
                os << word.dnib();
                os << " #" << j << ": " 
                  << word.value(j) << std::endl;
              }
              break;
          }
        }
      } // void dump(Steim2Frame f, std::ostream& os)

      /*----------------------------------------------------------------------*/

      void dump(SteimFrame& f, std::ostream& os)
      {
        os << "Steim Frame:" << std::endl;
        os << "  control word: 0x";
        std::ostream::fmtflags flags=os.flags();
        os.setf(std::ios_base::hex, std::ios_base::basefield);
        // os.setf(std::ios_base::showbase);
        os.width(8);
        os.fill('0');
        os << f.control();
        os.flags(flags);
        os << std::endl;
        /*
        for (int i=0; i<Steim1Frame::nelements; ++i)
        {
          os << "  ctrl #";
          os.width(2);
          os.fill('0');
          os << i << ": " << f.ctrl(i) << std::endl;
        }
        */
        os << "  data: " << std::endl;
        f.reset();
        while (f.valid())
        {
          os << "  (" << f.iword() << "," << f.idiff() << ") "
            << f.diff() << std::endl;
          f.next();
        }
      } // void dump(SteimFrame f, std::ostream& os)

    } // namespace SEED

  } // namespace mseed

} // namespace datrw

/* ----- END OF seedstructdump.cc ----- */
