/*! \file pdasread.cc
 * \brief read pdas file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read pdas file (implementation)
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
 *  - 30/03/2004   V1.0   Thomas Forbriger
 *  - 10/09/2004   V1.1   support long format
 *  - 19/10/2004   V1.2   
 *                        - long reading works now
 *                        - supports gainranged format
 *  - 28/04/2006   V1.3   - long int appears to be of 
 *                          implementation dependent size
 *                        - use long long int (8 byte) for delim
 * 
 * ============================================================================
 */
#define DATRW_PDASREAD_CC_VERSION \
  "DATRW_PDASREAD_CC   V1.3"

#include<iostream>
#include <datrwxx/pdasread.h>
#include <datrwxx/error.h>

namespace datrw {

  namespace pdas {

    struct FoundEOF { }; // struct FoundEOF

    //! function to read the file header line
    HeaderLine readline(std::istream& is, const bool& verbose)
    {
      HeaderLine retval;
      DATRW_assert(is.good(), "ERROR: unexpected end of file!");
      std::getline(is, retval.line);
      // remove CR
      unsigned long long int delim=retval.line.find_first_of(0x0D);
      retval.line.erase(delim);
      // strip value from token
      delim=retval.line.find_first_of(' ');
      if (delim!=std::string::npos)
      {
        retval.token=retval.line.substr(0,delim);
        retval.value=retval.line.substr(delim+1);
      }
      if (verbose)
      {
        std::cout << "line: " << retval.line << std::endl;
        std::cout << "  token: \"" << retval.token << "\"" << std::endl;
        std::cout << "  value: \"" << retval.value << "\"" << std::endl;
      }
      return(retval);
    }

    /*----------------------------------------------------------------------*/
    
    //! function to read the file header
    Header readheader(std::istream& is, const bool& verbose)
    {
      Header retval;
      HeaderLine hdline;
      while (hdline.line!="DATA")
      {
        hdline=readline(is, verbose);
        retval.lines.push_back(hdline.line);
        if (hdline.token=="DATASET")
        { retval.dataset=hdline.value; }
        else if (hdline.token=="SIGNAL")
        { retval.signal=hdline.value; }
        else if (hdline.token=="VERSION")
        { retval.version=hdline.value; }
        else if (hdline.token=="DATE")
        { retval.date=hdline.value; }
        else if (hdline.token=="TIME")
        { retval.time=hdline.value; }
        else if (hdline.token=="INTERVAL")
        { retval.interval=hdline.value; }
        else if (hdline.token=="VERT_UNITS")
        { retval.vertunits=hdline.value; }
        else if (hdline.token=="HORZ_UNITS")
        { retval.horzunits=hdline.value; }
        else if (hdline.token=="COMMENT")
        { 
          retval.comment=hdline.value; 
          if (retval.type == FtypeLONG)
          {
            if (hdline.value=="GAINRANGED")
            { retval.type=FtypeGAINRANGED; }
          }
        }
        else if (hdline.token=="FILE_TYPE")
        { 
          if (hdline.value=="INT")
          { retval.type=FtypeINT; }
          else if (hdline.value=="LONG")
          { retval.type=FtypeLONG; }
          else
          { DATRW_abort("ERROR: unsupported data type!"); }
        } 
      }
      if (verbose)
      {
        std::cout << "dataset:    " << retval.dataset << std::endl;
        std::cout << "signal:     " << retval.signal << std::endl;
        std::cout << "version:    " << retval.version << std::endl;
        std::cout << "date:       " << retval.date << std::endl;
        std::cout << "time:       " << retval.time << std::endl;
        std::cout << "interval:   " << retval.interval << std::endl;
        std::cout << "vert units: " << retval.vertunits << std::endl;
        std::cout << "horz units: " << retval.horzunits << std::endl;
        std::cout << "comment:    " << retval.comment << std::endl;
        std::cout << "data type:  ";
        if (retval.type==FtypeINT)
        { std::cout << "INT integer" << std::endl; }
        else
        { std::cout << "unknown!" << std::endl; }
      }
      return(retval);
    }

    /*----------------------------------------------------------------------*/
    
    /*! function to read the file data
     * passes vector by reference to avoid deep copy
     */
    void readdata(std::istream& is, Tdata& data, const Etype& type)
    {
      data.clear();
      bool hot=true;
      while(is.good()&&hot) 
      {
        Tvalue sample;
        try { sample=readsample(is, type); }
        catch (FoundEOF) { hot=false; }
        if (hot) data.push_back(sample); 
      }
    }

    /*----------------------------------------------------------------------*/
    
    //! function to skip the file data but count the samples
    int countdata(std::istream& is, const Etype& type)
    {
      int retval=0;
      bool hot=true;
      while(is.good()&&hot) 
      { 
        try { readsample(is, type); }
        catch (FoundEOF) { hot=false; --retval; }
        ++retval;
      }
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    //! function to read one sample
    Tvalue readsample(std::istream& is, const Etype& type)
    {
      int invalue;
      Tvalue value;
      union Byte {
        char inchar;
        unsigned char byte;
      }; // union byte
      if (type==FtypeINT)
      {
        Byte lbyte, hbyte;
        if (is.eof()) throw FoundEOF();
        is.get(lbyte.inchar);
        if (is.eof()) throw FoundEOF();
        is.get(hbyte.inchar);
        invalue=int(lbyte.byte)+256*int(hbyte.byte);
        if (int(hbyte.byte)>127) 
        { value=Tvalue(invalue-65536); } else { value=Tvalue(invalue); }
      }
      else if (type==FtypeLONG)
      {
        union Long {
          // long int ival;
          int ival;
          char byte[4];
          unsigned char ubyte[4];
        }; // union Long
        Long inval;
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[0]);
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[1]);
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[2]);
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[3]);
        value=inval.ival;
      }
      else if (type==FtypeGAINRANGED)
      {
        // DATRW_abort("gainranged format ist not supported");
        union Short {
          short int ival;
          char byte[2];
          unsigned char ubyte[2];
        }; // union Short
        Short inval;
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[0]);
        if (is.eof()) throw FoundEOF();
        is.get(inval.byte[1]);
        Short Smantissa=inval;
        Smantissa.ubyte[0]=(inval.ubyte[0] & 0xFC);
        int mantissa=Smantissa.ival/4;
        unsigned short int exponent=(inval.ubyte[0] & 0x03);
        int shifts=(5-exponent)*3;
        int factor=(1<<shifts);
        value=mantissa*factor;
        /*
        std::cerr 
          << inval.ival << " "
          << Smantissa.ival << " "
          << mantissa << " "
          << exponent << " "
          << shifts << " "
          << factor << " "
          << value << " " << std::endl;
        */
      }
      else
      {
        DATRW_abort("ERROR: unsupported data type!"); 
      }
      return(value);
    }

    /*----------------------------------------------------------------------*/

    using std::endl;

    void help(std::ostream& os)
    {
      os << std::endl;
      os << "PDAS data reading functions" << std::endl;
      os << "---------------------------" << std::endl;
      os << endl;
      os 
        << "The scaling of the data samples depends on the data format" << endl
        << "used store original PDAS data files. The sacling is defined" << endl
        << "in section 4.1.7.2 on pages 4-15 and 4-16 in the PDAS" << endl
        << "user's guide. The definitions are:" << endl << endl
        << "  16-bit data format" << endl
        << "    Data is stored in two byte integer two's complement format."
                << endl
        << "    m: digitized value in counts" << endl
        << "    p: pre-amplifier gain" << endl
        << "    v: signal value in volts" << endl
        << "    v = m / (32768 * p)       for high gain inputs" << endl
        << "    v = 20 * m / (32768 * p)  for low gain inputs" << endl 
        << "    where 32768 = 2**15 = 0x8000" << endl << endl
        << "  32-bit data format" << endl
        << "    Data is stored in four byte integer two's complement format."
                << endl
        << "    m: digitized value in counts" << endl
        << "    v: signal value in volts" << endl
        << "    v = m / (2147483648)       for high gain inputs" << endl
        << "    v = 20 * m / (2147483648)  for low gain inputs" << endl 
        << "    where 2147483648 = 2**31 = 0x80000000" << endl << endl
        << "  14/2 gain ranged format" << endl
        << "    The data is stored in two byte format. The gain code" << endl
        << "    is stored in the two least significant bits of the" << endl
        << "    16 bits and the digitized value is in the upper 14" << endl
        << "    bits of the 16 bits." << endl
        << "    g: gain code from the lower two bits" << endl
        << "    p: pre-amplifier gain" << endl
        << "    m: two's complement value from the upper 14 bits" << endl
        << "    v: signal value in volts" << endl
        << "    v = m * (8**(5-g)) / ( 268435456 * p)      " 
        <<        "for high gain inputs" << endl
        << "    v = 20 * m * (8**(5-g)) / ( 268435456 * p) "
        <<        "for low gain inputs" << endl
        << "    where 268435456 = 2**28 = 0x10000000" << endl << endl;
      os 
        << "Scaling applied by the reading functions:" << endl
        << "  The PDAS data files contain no information about the" << endl
        << "  pre-amplifier setting and about the input terminals" << endl
        << "  (high or low gain) used. The data can thus not be" << endl
        << "  scaled to volts. The conversion applied when reading is" << endl
        << "    m and g: as defined above" << endl 
        << "    i: value returned by integer reading function (iseries)" << endl
        << "    f: value returned by float reading functions "
                << "(fseries and dseries)" << endl
        << "    16-bit data (indicated as INT in the FILE_TYPE field)" << endl
        << "      i = m" << endl
        << "      f = m / 32768 " << endl
        << "    32-bit data (indicated as LONG in the FILE_TYPE field)" << endl
        << "      i = m" << endl
        << "      f = m / 2147483648 " << endl
        << "    14/2 bit data (indicated as GAINRANGED in the COMMENT field)" 
                << endl
        << "      i = m * (8**(5-g))" << endl
        << "      f = m * (8**(5-g)) / 268435456 " << endl << endl
        << "  Thus if extracting floating point numbers from 16-bit" << endl
        << "  or 14/2-bit data files, you just have to apply" << endl
        << "    v = f / p           for high gain inputs and" << endl
        << "    v = 20 * f / p      for low gain inputs," << endl
        << "    where p is the pre-amplifier gain" << endl
        << "  to obtain the signal in volts." << endl << endl
        << "  For 32-bit data its even easier. You obtain the" << endl
        << "  signal's voltage by" << endl
        << "    v = f               for high gain inputs and" << endl
        << "    v = 20 * f          for low gain inputs." << endl;
      os << endl;
    }

  } // namespace pdas

} // namespace datrw

/* ----- END OF pdasread.cc ----- */
