/*! \file sffxx.cc
 * \brief SFF library (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2003
 * 
 * SFF library (implementation)
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 21/12/2003   V1.0   Thomas Forbriger
 *  - 23/02/2004   V1.1   changed DAST and STAT reading code
 *                        there are not necessarily any flag characters
 *  - 07/04/2004   V1.2  
 *                        - provide debug output
 *                        - correct reading of FREE block
 *  - 23/12/2004   V1.3   added full block append to FREE
 *  - 26/01/2004   V1.4   SRCE reading and INFO reading was not satisfactory
 *  - 15/03/2005   V1.5   
 *                        - made SRCE date and time reading more robust against
 *                          whitespace 
 *                        - added some debug output
 *                        - check SFF file type version to be at least 1.10
 *  - 27/03/2006   V1.6   allow reading of V1.09 files
 *                        introduced sff::STAT::decode_libversion
 *  - 27/06/2006   V1.7   added INFO comparison
 *  - 24/07/2006   V1.8   - provide millisecond precision
 *                        - field Fseconds was removed from 
 *                          GSE2::waveform::TWID2
 *  - 19/06/2007   V1.9   - SRCE line fields were not read correctly
 *                        - modified all reading functions to use substr()
 *                          function of class string to read fields from
 *                          correct position
 *  - 29/06/2007   V1.10  - read SRCE line prior to file FREE block
 *  - 03/07/2007   V1.11  - read FREE block prior to file SRCE line
 *                          order of elements in file header should not be
 *                          defined by the order of the code characters since
 *                          reading functions in libsffxx do not respect this
 *                          order. libstuff always writes the FREE block first
 *                          in sff_WOpenFS
 *  - 17/12/2007   V1.12  - modified output format (printf), since underlying
 *                          Fortran code of libtime does no longer use 
 *                          long int
 *  - 13/04/2010   V1.13  
 *                        - round time correctly to milliseconds when writing
 *                          WID2 line
 *                        - WIDX reading is now transparent
 * -  19/07/2010   V1.14  - Daniel Armbruster: cast of values to int in 
 *                          void STAT::setstamp(const libtime::TAbsoluteTime&
 *                          date) const
 *                        - cast of values to int in std::string SRCE::line()
 *                          const 
 *                        - adding header <stdlib.h> for function atoi
 * -  18/11/2010   V1.15  - increased format version to 2.00
 *                          The C++ format with DAST being -1 is a significant
 *                          change, which was not reflected by the version
 *                          number. Version 1.10 exists in parallel at Bochum
 *                          and just indicates an increased character buffer
 *                          size in the Fortran version.
 * - 21/11/2011    V1.16  - waveform normalizer now limits the maximum value
 *                          to 2**27-1 which matches the settings in libgsexx
 * - 06/12/2011    V1.17  - waveform normalizer now limits the maximum value
 *                          to 2**25-1 which ensure that the maximum second
 *                          difference is smaller than 2**27. The settings
 *                          in V1.16 were due to a wrong interpretation of
 *                          libgsexx limits. This this provides  the full
 *                          dynamic range of 24bit data acquisition systems to
 *                          be stored without round-off.
 * - 17/02/2012    V1.18  - properly round date of first sample to
 *                          microseconds when passing to libgsexx
 * - 14/02/2017    V1.19  - limit number of characters in string fields upon
 *                          assembling SFF header lines from C++ strings
 *
 * ============================================================================
 */
#define TF_SFFXX_CC_VERSION \
  "TF_SFFXX_CC   V1.19"

#include<sstream>
#include <sffxx.h>
#include <gsexx.h>
#include <cstdio>
#include <cmath>
#include <stdlib.h>

namespace sff {

  namespace helper {
    //! Check GSE identifier at beginning of line.
    template<class C>
    bool IDmatch(const std::string& line, const bool& debug=false)
    { 
      if (debug) 
      {
        std::cerr << "DEBUG: IDmatch: "
          << line.substr(0,4) << "=?=" << std::string(C::LINEID)
          << std::endl;
      }
      return(line.substr(0,4)==std::string(C::LINEID)); 
    }
  } // namespace helper

  //! Fortran library version (to ensure compatibility)
  /*! library writes version 2.00
   *
   * This version is written to output files to indicate
   * the SFF format version in the file.
   *
   * On input files with format version larger than libversion will be rejected
   */
  const double STAT::libversion=2.00;
  /*! library decodes version 1.09
   *
   * On input files with format version smaller than decode_libversion will be
   * rejected
   */
  const double STAT::decode_libversion=1.09;
  const char* const STAT::LINEID="STAT";
  const char* const FREE::LINEID="FREE";
  const char* const SRCE::LINEID="SRCE";
  const char* const DAST::LINEID="DAST";
  const char* const INFO::LINEID="INFO";

  /*----------------------------------------------------------------------*/

  char coosysID(const Ecoosys& csid)
  {
    char retval;
    if (csid == CS_cartesian) retval='C';
    else if (csid == CS_spherical) retval='S';
    else throw
      GSE2::Terror("ERROR (sff::coosysID): library inconsistency!");
    return(retval);
  } // coosysID

  Ecoosys coosysID(const char& csid)
  {
    Ecoosys retval;
    if (csid=='C') { retval=CS_cartesian; }
    else if (csid=='S') { retval=CS_spherical; }
    else throw
      GSE2::Terror("ERROR (sff::coosysID): unknown coordinate system key!");
    return(retval);
  } // coosysID

/*======================================================================*/
// SFF structs
// -----------
//
// STAT
// ----

  /*! \struct STAT
   *
   * The STAT line is the first line in the file header.
   * It contains the version of the library that wrote the file,
   * a timestamp and flags indicating the presence of optional elements like a
   * FREE block or an SRCE line.
   *
   * \sa \ref subsec_definition_stat_line
   *
   * definition of STAT line:
   * \verbatim
c     position   format   contents
c     1-5        a5       STAT  (identifier)
c     6-12       f7.2     library version 
c                           minor versions are counted in 0.01 steps
c                           major versions are counted in 1.0 steps
c     14-26      a13      timestamp of file creation time:
c                           yymmdd.hhmmss
c     28-37      a10      code with a combination of two 
c                         possible characters:
c                          F   there follows a FREE block
c                          S   there folloes a SRCE line
\endverbatim
   *
   * 27/3/2006: Allow reading of version 1.09 files.
   */
  STAT::STAT(): hasfree(false), hassrce(false) 
  { setstamp(libtime::now()); }

  /*----------------------------------------------------------------------*/
  
  void STAT::setstamp(const libtime::TAbsoluteTime& date) const
  {
    char stamp[14];
    int yeardigits, century;
    century=int(date.year()/100);
    yeardigits=date.year()-100*century;
    sprintf(stamp, "%2.2d%2.2d%2.2d.%2.2d%2.2d%2.2d",
            static_cast<int>(yeardigits), static_cast<int>(date.month()),
            static_cast<int>(date.day()), static_cast<int>(date.hour()),
            static_cast<int>(date.minute()), static_cast<int>(date.second()));
    timestamp=std::string(stamp);
  } // void STAT::setstamp(const libtime::TAbsoluteTime& date) const

  /*----------------------------------------------------------------------*/
  
  std::string STAT::line() const
  {
    this->setstamp(libtime::now());
    char charline[40];
    std::string code("");
    if (this->hasfree) { code.append("F"); }
    if (this->hassrce) { code.append("S"); }
    sprintf(charline, "%-4s  %6.2f %-13s %-10s\n",
            STAT::LINEID, 
            STAT::libversion,
            timestamp.substr(0,13).c_str(),
            code.substr(0,10).c_str());
    std::string retval(charline);
    return(retval);
  } // std::string STAT::line() const

  /*----------------------------------------------------------------------*/
  
  void STAT::read(std::istream& fis, const bool& debug)
  {
    if (debug) { std::cerr << "DEBUG (STAT::read):" << std::endl; }
    // read complete line
    std::string theline;
    std::getline(fis, theline);

    if (debug) { std::cerr << theline << std::endl; }
    std::string lineID=theline.substr(0,4);
    if (debug) { std::cerr << lineID << std::endl; }
    if (!helper::IDmatch<STAT>(lineID)) throw
       GSE2::Terror("ERROR (STAT::read): missing STAT ID!");

    double inlibversion=atof(theline.substr(5,7).c_str());
    if (inlibversion>STAT::libversion) 
    { 
      std::cerr << "ERROR (STAT::read): "
        << "data file has format version " 
        << inlibversion << std::endl;
      std::cerr << "ERROR (STAT::read): "
        << "library can decode formats up to version " 
        << STAT::libversion << std::endl;
      throw 
        GSE2::Terror("ERROR (STAT::read): file library version too large!"); 
    }
    if (inlibversion<STAT::decode_libversion) 
    { 
//      std::cerr << "theline " << theline << std::endl;
//      std::cerr << "theline.lv " << theline.substr(5,6) << std::endl;
//      std::cerr << "inlibversion " << inlibversion << std::endl;
//      std::cerr << "decode_libversion " << decode_libversion << std::endl;
      std::cerr << "ERROR (STAT::read): "
        << "data file has format version " 
        << inlibversion << std::endl;
      std::cerr << "ERROR (STAT::read): "
        << "library can decode formats from version "
        << STAT::decode_libversion 
        << " up to version " 
        << STAT::libversion << std::endl;
      throw 
        GSE2::Terror("ERROR (STAT::read): incompatible SFF version!"); 
    }

    this->timestamp=theline.substr(13,13);

    std::string code=theline.substr(27,10);
//    std::cerr << "code " << code << std::endl;
    this->hasfree=(code.find('F')!=std::string::npos);
    this->hassrce=(code.find('S')!=std::string::npos);
    if (debug)
    {
      if (this->hasfree) 
      { std::cerr << "DEBUG (STAT::read): has FREE block" << std::endl; }
      if (this->hassrce) 
      { std::cerr << "DEBUG (STAT::read): has SRCE line" << std::endl; }
      std::cerr << "DEBUG (STAT::read): finished" << std::endl;
    }
  } // void STAT::read(std::istream& fis, const bool& debug)

/*----------------------------------------------------------------------*/
// SRCE
// ----

  /*! \struct SRCE
   *
   * This line holds information of the source that caused the
   * seismic signal.
   *
   * definition of SRCE line:
   * \verbatim
c     position   format   contents
c     1-5        a5       SRCE  (identifier)
c     6-25       a20      type of source (any string like "earthquake")
c     27         a1       type of coordinate system:
c                             C: cartesian    S: spherical
c     29-43      f15.6    c1   x               latitude
c     44-58      f15.6    c2   y               longitude
c     59-73      f15.6    c3   z               height
c                         see below for comments on coordinate specification
c     75-80      a6       date of source event: yymmdd
c     82-91      a10      time of source event: hhmmss.sss
\endverbatim
   *
   * \sa \ref subsec_definition_srce_line
   *
   * Coordinate specification
   * \verbatim
c  Coordinate Specification
c
c    Notice that given coordinates imply a spatial relation between
c    the source location and the receiver locations. While spherical
c    coordinates refer to a fixed reference frame on the earth, cartesian
c    coordinates refer to an arbitrary origin. The creator of the 
c    datafile is responsible to take care that coordinate information
c    is consistent between the SRCE line and the several possible
c    INFO lines.
c
c    cartesian coordinates
c
c      x, y and z are vector components in a right handed cartesian
c      reference frame. While x and y lie arbitrary orientated in the
c      horizontal plane, z is counted positive upwards from an arbitrary
c      reference level (preferably the free surface). All three coordinate
c      values are measured in meters.
c
c    spherical coordinates
c
c      Latitude and longitude are given in the geographical reference frame
c      and measured in degrees. The height value gives the height above
c      the geoid and is measured in meters.
\endverbatim
   *
   * \sa \ref sec_definition_coordinates
   */

  SRCE::SRCE(): 
    type("NSP"), date(libtime::now()),
    cs(CS_cartesian), cx(0.), cy(0.), cz(0.) { }

  /*----------------------------------------------------------------------*/

  std::string SRCE::line() const
  {
    char charline[95];
    int yeardigits, century;
    century=int(date.year()/100);
    yeardigits=date.year()-100*century;
    sprintf(charline, "%-4s %-20s %1c %15.6f%15.6f%15.6f "
            "%2.2i%2.2i%2.2i %2.2i%2.2i%2.2i.%3.3i\n",
            SRCE::LINEID, 
            type.substr(0,20).c_str(), 
            coosysID(cs), cx, cy, cz,
            yeardigits, static_cast<int>(date.month()), static_cast<int>(date.day()),
            static_cast<int>(date.hour()), static_cast<int>(date.minute()), 
            static_cast<int>(date.second()), static_cast<int>(date.milsec()));
    std::string retval(charline);
    return(retval);
  } // std::string SRCE::line() const

  /*----------------------------------------------------------------------*/
  
  void SRCE::read(std::istream& fis, const bool& debug)
  {
    std::string theline;
    std::getline(fis, theline);

    std::string lineID=theline.substr(0,4);
    if (!helper::IDmatch<SRCE>(lineID)) throw
       GSE2::Terror("ERROR (SRCE::read): missing SRCE ID!");

    this->type=theline.substr(5,20);
    if (debug)
    { std::cerr << "DEBUG (SRCE::read): type: " << type << std::endl; }

    this->cs=coosysID(*theline.substr(26,1).c_str());
    this->cx=atof(theline.substr(28,15).c_str());
    this->cy=atof(theline.substr(43,15).c_str());
    this->cz=atof(theline.substr(58,15).c_str());
    if (debug)
    { 
      std::cerr << "DEBUG (SRCE::read): cs,cx,cy,cz: " 
        << coosysID(cs) << "," << cx << "," << cy << "," << cz << std::endl; 
    }

    std::string datestring,timestring;
    datestring=theline.substr(74,6);
    timestring=theline.substr(81,10);
    if (debug)
    { 
      std::cerr << "DEBUG (SRCE::read): datestring: " 
        << datestring << std::endl;
      std::cerr << "DEBUG (SRCE::read): timestring: " 
        << timestring << std::endl;
    }
    std::string fulldate("");
    fulldate+=datestring.substr(0,2);
    fulldate+="/";
    fulldate+=datestring.substr(2,2);
    fulldate+="/";
    fulldate+=datestring.substr(4,2);
    fulldate+=" ";
    fulldate+=timestring.substr(0,2);
    fulldate+=":";
    fulldate+=timestring.substr(2,2);
    fulldate+=":";
    fulldate+=timestring.substr(4,6);
    if (debug)
    { 
      std::cerr << "DEBUG (SRCE::read): convert string: " 
        << fulldate << std::endl; 
    }
    this->date=libtime::TAbsoluteTime(fulldate);
    if (debug)
    { 
      std::cerr << "DEBUG (SRCE::read): time: " 
        << date.timestring() << std::endl; 
    }
  } // void SRCE::read(std::istream& fis, const bool& debug)

/*----------------------------------------------------------------------*/
// DAST
// ----

  /*! \struct DAST
   *
   * Data status line providing scaling factors and required buffer size.
   *
   * definition of DAST line:
   * \verbatim
c     position   format   contents
c     1-5        a5       DAST  (identifier)
c     7-16       i10      number of characters in encoded dataset
c                         From library version 1.10 this field may be -1.
c                         In this case the reading program has to determine
c                         the number of characters itself by detecting the
c                         CHK2 line. This change was necessary to implement
c                         the C++ version of libsff since this starts writing
c                         without having encoded the whole trace already.
c     18-33      e16.6    ampfac
c                         This is a factor to scale the (floating point)
c                         dataset to an desireable dynamic range
c                         before converting it to Fortran integer
c                         values. After reading the dataset and
c                         decoding and converting it to floating point
c                         you have to multiply each sample by ampfac
c                         to get back the original values.
c                         As the maximum range of integer values goes
c                         from -(2.**31) to (2.**31)-1 you might
c                         like to adjust the maximum integer value
c                         to 0x7FFFFFFF. This may cause problems
c                         as the second differences compressing algorithm
c                         may increase the dynamic range of your data
c                         by a factor of four in the worst case.
c                         It is save to adjust the largest absolute
c                         value in the dataset to (2.**23)-1 which
c                         is 0x7FFFFF.
c                         The underlying libgsexx limits the maximum value to
c                         2**27-1. We adopt this here (thof, 21.11.2011)
c     35-44      a10      code with a combination of three possible
c                         characters indicating possible optional blocks
c                         and a following further dataset:
c                           F    a FREE block follows after dataset
c                           I    an INFO line follows after dataset
c                           D    there is another Data Block following
c                                in this file (this must be the last
c                                character in code)
\endverbatim
   *
   * \sa \ref subsec_definition_dast_line
   */

  DAST::DAST(): 
    nchar(-1), ampfac(1.), hasfree(false), 
    hasinfo(false), last(false) { }

  /*----------------------------------------------------------------------*/

  std::string DAST::line() const
  {
    char charline[50];
    std::string code("");
    if (this->hasfree) { code.append("F"); }
    if (this->hasinfo) { code.append("I"); }
    if (!this->last) { code.append("D"); }
    // write -1 to nchar field in any case
    sprintf(charline, "%-4s  %10i %16.6E %-10s\n",
            DAST::LINEID, 
            -1, ampfac, code.substr(0,10).c_str());
    std::string retval(charline);
    return(retval);
  } // std::string DAST::line() const

  /*----------------------------------------------------------------------*/

  void DAST::read(std::istream& fis, const bool& debug)
  {
    if (debug)
    {
      std::cerr << "DEBUG (DAST::read):" << std::endl;
    }
    std::string theline;
    std::getline(fis, theline);
    if (debug) { std::cerr << theline << std::endl; }

    std::string lineID=theline.substr(0,4);
    if (debug) { std::cerr << lineID << std::endl; }
    if (!helper::IDmatch<DAST>(lineID)) throw
       GSE2::Terror("ERROR (DAST::read): missing DAST ID!");

    this->nchar=atoi(theline.substr(6,10).c_str());
    this->ampfac=atof(theline.substr(17,16).c_str());

    std::string code=theline.substr(34,10);
    if (debug)
    {
      std::cerr << "DEBUG (DAST): read nchar=" << nchar
        << " ampfac=" << ampfac << " code=" << code << std::endl;
    }
    this->hasinfo=(code.find('I')!=std::string::npos);
    this->hasfree=(code.find('F')!=std::string::npos);
    this->last=(code.find('D')==std::string::npos);
  } // DAST::read(std::istream& fis, const bool& debug)

/*----------------------------------------------------------------------*/
// FREE
// ----

  /*! \struct FREE
   *
   * A FREE block contains comments with no specific format.
   *
   * definition of FREE block:
   * \verbatim
c     This is a block of any set of 80 characters wide lines.
c     The start of this block is indicated a single line
c     containing FREE in the first 5 positions. Another line
c     of this content indicates the end of the FREE block.
c     A FREE block may contain any usefull information for
c     the user and has to follow no other standard than
c     a line length of 80 characters.
\endverbatim
   *
   * \sa \ref subsec_definition_free_block
   */

  FREE::FREE() { lines.clear(); }

  /*----------------------------------------------------------------------*/

  void FREE::write(std::ostream& os) const
  {
    os << FREE::LINEID << " " << std::endl;
    for(Tlines::const_iterator I=lines.begin();
        I != lines.end(); I++)
    { os << *I << std::endl; }
    os << FREE::LINEID << " " << std::endl;
  } // void FREE::write(std::ostream& os) const

  /*----------------------------------------------------------------------*/
    
  void FREE::read(std::istream& is, const bool& debug)
  {
    std::string lineID;
    // getline(is,lineID);
    is >> lineID;
    if (debug) 
    { std::cerr << "DEBUG (FREE): lineID=" << lineID << std::endl; }
    if (!helper::IDmatch<FREE>(lineID.substr(0,4), debug)) throw
       GSE2::Terror("ERROR (FREE::read): missing FREE ID!");
    lines.clear();
    is.ignore(10,'\n');
    getline(is,lineID);
    if (debug) { std::cerr << "FREE " << lineID << std::endl; }
    if (debug) { std::cerr << "ID  \"" << lineID.substr(0,4) << "\"" << std::endl; }
    while (!helper::IDmatch<FREE>(lineID.substr(0,4), debug))
    {
      lines.push_back(lineID);
      getline(is,lineID);
    }
  } // void FREE::read(std::istream& is, const bool& debug)

  /*----------------------------------------------------------------------*/
  
  void FREE::append(const Tlines& newlines)
  {
    Tlines::const_iterator I(newlines.begin());
    while (I != newlines.end())
    {
      this->append(*I);
      ++I;
    }
  } // void FREE::append(const Tlines& lines)

/*----------------------------------------------------------------------*/
// WID2
// ----

  /*! \struct WID2
   *
   * This waveform identification line is defined in the GSE2.0 standard.
   *
   * definition of WID2 line:
   * \verbatim
c     position   name     format           contents
c     1-4        id       a4               WID2 (identifier)
c     6-15       date     i4,a1,i2,a1,i2   date of first sample: yyyy/mm/dd
c     17-28      time     i2,a1,i2,a1,f6.3 time of first sample: hh:mm:ss.sss
c     30-34      station  a5               for a valid GSE2.0 block use
c                                          ISC station code
c     36-38      channel  a3               for a valid GSE2.0 block use
c                                          FDSN channel designator
c     40-43      auxid    a4               auxiliary identification code
c     45-47      datatype a3               must be CM6 in SFF
c     49-56      samps    i8               number of samples
c     58-68      samprat  f11.6            data sampling rate in Hz
c     70-79      calib    e10.2            calibration factor
c     81-87      calper   f7.3             calibration period where calib
c                                          is valid
c     89-94      instype  a6               instrument type (as defined
c                                          in GSE2.0
c     96-100     hang     f5.1             horizontal orientation of
c                                          sensor, measured in degrees
c                                          clockwise from North 
c                                          (-1.0 if vertical)
c     102-105    vang     f4.1             vertical orientation of sensor,
c                                          measured in degrees from vertical
c                                          (90.0 if horizontal)
\endverbatim
   *
   * \sa \ref subsec_definition_wid2_line
   *
   * Reading and writing is accomplished through the GSE++ library functions.
   */

  WID2::WID2():
    date(libtime::now()), station("NSP"), channel("NSP"), auxid("NSP"),
    nsamples(-1), dt(-1.), calib(-1.), calper(-1.), instype("NSP"),
    hang(-1.),vang(-1.) { }

  /*----------------------------------------------------------------------*/

  std::string WID2::line() const
  {
    GSE2::waveform::TWID2 wid2line;
    // apply proper rounding to nearest millisecond
    libtime::TAbsoluteTime rnddate(date.year(), date.month(),
                                   date.day(), date.hour(),
                                   date.minute(), date.second(),
                                   date.milsec());
    if (date.micsec()>499) { rnddate += libtime::TRelativeTime(0,0,0,0,1); }
    wid2line.Fyear=rnddate.year();
    wid2line.Fmonth=rnddate.month();
    wid2line.Fday=rnddate.day();
    wid2line.Fhour=rnddate.hour();
    wid2line.Fminute=rnddate.minute();
    wid2line.Fmilsec=rnddate.second()*1000+rnddate.milsec();
    wid2line.Fstation=station;
    wid2line.Fchannel=channel;
    wid2line.Fauxid=auxid;
    wid2line.Fsamps=nsamples;
    wid2line.Fsamprate=1./dt;
    wid2line.Fcalib=calib;
    wid2line.Fcalper=calper;
    wid2line.Finstype=instype;
    wid2line.Fhang=hang;
    wid2line.Fvang=vang;
    return(wid2line.line());
  } // std::string WID2::line() const

  /*----------------------------------------------------------------------*/

  void WID2::read(std::istream& is)
  {
    std::string line;
    std::getline(is, line);
    if (line.substr(0,4) == WIDXID)
    {
      *this = WIDXline(line);
    }
    else
    {
      std::istringstream iss(line);
      GSE2::waveform::TWID2 wid2line;
      wid2line.read(iss);
      int second=int(wid2line.Fmilsec/1000);
      int milsec=wid2line.Fmilsec-1000*second;
      date=libtime::TAbsoluteTime(wid2line.Fyear,
                                  wid2line.Fmonth,
                                  wid2line.Fday,
                                  wid2line.Fhour,
                                  wid2line.Fminute,
                                  second, milsec);
      this->station=wid2line.Fstation;
      this->channel=wid2line.Fchannel;
      this->auxid=wid2line.Fauxid;
      this->nsamples=wid2line.Fsamps;
      this->dt=1./wid2line.Fsamprate;
      this->calib=wid2line.Fcalib;
      this->calper=wid2line.Fcalper;
      this->instype=wid2line.Finstype;
      this->hang=wid2line.Fhang;
      this->vang=wid2line.Fvang;
    }
  } // void WID2::read(std::istream& is)

/*----------------------------------------------------------------------*/
// INFO
// ----

  /*! \struct INFO
   *
   * This line provides receiver specific parameters.
   *
   * definition of INFO line:
   * \verbatim
c     position   format   contents
c     1-5        a5       INFO  (identifier)
c     6          a1       type of coordinate system:
c                             C: cartesian    S: spherical
c     8-22       f15.6    c1   x               latitude
c     23-37      f15.6    c2   y               longitude
c     38-52      f15.6    c3   z               height
c                         see below for comments on coordinate specification
c     54-57      i4       number of stacks done during acquisition
c                         (a value of zero and a value of one both mean
c                         a single shot)
\endverbatim
   *
   * \sa \ref subsec_definition_info_line
   *
   * See SRCE line for coordinate spcifications.
   */

  INFO::INFO():
    cs(CS_cartesian), cx(0.), cy(0.), cz(0.), nstacks(0) { }

  /*----------------------------------------------------------------------*/
  
  std::string INFO::line() const
  {
    char charline[60];
    sprintf(charline, "%-4s %1c %15.6f%15.6f%15.6f %4i\n",
            INFO::LINEID, 
            coosysID(cs), cx, cy, cz, nstacks);
    std::string retval(charline);
    return(retval);
  } // std::string INFO::line() const

  /*----------------------------------------------------------------------*/
  
  void INFO::read(std::istream& fis)
  {
    std::string theline;
    std::getline(fis, theline);

    std::string lineID=theline.substr(0,4);
    if (!helper::IDmatch<INFO>(lineID)) throw
       GSE2::Terror("ERROR (INFO::read): missing INFO ID!");

    this->cs=coosysID(*theline.substr(5,1).c_str());
    this->cx=atof(theline.substr(7,15).c_str());
    this->cy=atof(theline.substr(22,15).c_str());
    this->cz=atof(theline.substr(37,15).c_str());
    this->nstacks=atoi(theline.substr(53,4).c_str());
  } // void INFO::read(std::istream& fis)

  /*----------------------------------------------------------------------*/
  
  bool INFO::operator==(const INFO& info) const
  {
    return ((info.cs==this->cs) &&
            (info.cx==this->cx) &&
            (info.cy==this->cy) &&
            (info.cz==this->cz) &&
            (info.nstacks==this->nstacks));
  } // bool INFO::operator==(const INFO& info) const

/*----------------------------------------------------------------------*/
// FileHeader
// ----------

  /*! \class FileHeader
   * \brief SFF file header elements.
   *
   * Holds all file header elements. Supports reading and writing of SFF file
   * headers.
   *
   * \sa \ref subsec_definition_file_header
   */

  void FileHeader::write(std::ostream& os) const
  {
    os << Mstat.line();
    if (Mstat.hasfree) { Mfree.write(os); }
    if (Mstat.hassrce) { os << Msrce.line(); }
  }

  /*----------------------------------------------------------------------*/
  
  void FileHeader::read(std::istream& is, const bool& debug)
  {
    // reading the FREE block first matches the order of header elements as
    // written by sff_WOpenFS in libstuff.f
    Mstat.read(is, debug);
    if (Mstat.hasfree) 
    { 
      Mfree.read(is, debug); 
      if (debug)
      { std::cerr << "DEBUG (FileHeader::read): file FREE read" << std::endl; }
    }
    if (Mstat.hassrce) 
    { 
      Msrce.read(is, debug); 
      if (debug)
      { std::cerr << "DEBUG (FileHeader::read): SRCE line read" << std::endl; }
    }
    if (debug)
    { std::cerr << "DEBUG (FileHeader::read): finished" << std::endl; }
  }

/*----------------------------------------------------------------------*/
// TraceHeader
// -----------

  /*! \class TraceHeader
   * \brief SFF trace header elements.
   *
   * Holds all trace header elements. Supports reading and writing of SFF
   * trace headers.
   *
   * \sa \ref subsec_definition_data_block
   */

  void TraceHeader::writeheader(std::ostream& os) const
  {
    if (Mdebug) { std::cerr << "DEBUG: write DAST line" << std::endl; } 
    os << Mdast.line();
    if (Mdebug) { std::cerr << "DEBUG: write WID2 line" << std::endl; } 
    os << Mwid2.line();
  }

  /*----------------------------------------------------------------------*/
  
  void TraceHeader::writetrailer(std::ostream& os) const
  {
    if (Mdast.hasfree) 
    {
      if (Mdebug) { std::cerr << "DEBUG: write FREE block" << std::endl; } 
      Mfree.write(os);
    }
    if (Mdast.hasinfo) 
    {
      if (Mdebug) { std::cerr << "DEBUG: write INFO line" << std::endl; } 
      os << Minfo.line();
    }
  }

  /*----------------------------------------------------------------------*/
  
  void TraceHeader::readheader(std::istream& is) 
  {
    if (Mdebug) { std::cerr << "DEBUG: read DAST line" << std::endl; } 
    Mdast.read(is,Mdebug);
    if (Mdebug) { std::cerr << "DEBUG: read WID2 line" << std::endl; } 
    Mwid2.read(is);
  }

  /*----------------------------------------------------------------------*/
  
  void TraceHeader::readtrailer(std::istream& is) 
  {
    if (Mdebug) { std::cerr << "DEBUG: read trace trailer" << std::endl; } 
    if (Mdast.hasfree) 
    {
      if (Mdebug) { std::cerr << "DEBUG: read FREE block" << std::endl; } 
      Mfree.read(is, Mdebug); 
      if (Mdebug) { Mfree.write(std::cerr); }
    }
    if (Mdast.hasinfo) 
    {
      if (Mdebug) { std::cerr << "DEBUG: read INFO line" << std::endl; } 
      Minfo.read(is);
    }
  }

/*----------------------------------------------------------------------*/
// WaveformNormalizer
// ------------------

  /*! \brief the absolute maximum amplitude (one-sided) to which
   * the time series will be normalized.
   *
   * GSE limits second differences to be smaller than 0x08000000
   * which is 134217728 which is 2**27. Consequently the largest amplitude of
   * data values prior to calculating second differences must not be larger
   * than 2**25-1 which is 0x1FFFFFF.
   */
  const int WaveformNormalizer::limit=0x1ffffff;

  WaveformNormalizer::WaveformNormalizer(const Enormmode& nm, 
                                         const double& maxval):
    Mmaxval(maxval), Mnorm(nm)
  {
    if (Mnorm == NM_one) 
    { 
      Mampfac=1.; 
      Mscale=false;
      if (Mmaxval > double(WaveformNormalizer::limit)) throw
        GSE2::Terror("ERROR (sff::WaveformNormalizer::scan): "
                     "dynamic range to large for non-normalizing mode");
    }
    else if (Mnorm == NM_ifneeded)
    { 
      Mampfac=1.;
      Mscale=false;
      if (Mmaxval > double(WaveformNormalizer::limit))
      {
        Mampfac=Mmaxval/double(WaveformNormalizer::limit); 
        Mscale=true;
      }
    }
    else if (Mnorm == NM_maxdyn)
    { 
      Mampfac=Mmaxval/double(WaveformNormalizer::limit); 
      Mscale=true;
    }
    else throw
      GSE2::Terror("ERROR (sff::WaveformNormalizer::scan): "
                   "library inconsistency!");
  }

/*----------------------------------------------------------------------*/
// SkipWaveform
// ------------

    void SkipWaveform::read(std::istream& is) 
    {
      Mheader.readheader(is);;
      int nsamples=Mheader.wid2().nsamples;
      GSE2::waveform::TDAT2readCM6 freader(nsamples);
      int idata;
      for (int i=0; i<nsamples; i++)
      { idata=freader(is); }
      Mheader.readtrailer(is);
      Mvalid=true;
    } // SkipWaveform::read

} // namespace sff

/* ----- END OF sffxx.cc ----- */
