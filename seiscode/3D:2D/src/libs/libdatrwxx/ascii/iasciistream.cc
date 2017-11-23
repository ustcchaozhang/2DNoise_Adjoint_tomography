/*! \file iasciistream.cc
 * \brief input raw ASCII data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/10/2011
 * 
 * input raw ASCII data (implementation)
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
 *  - 18/10/2011   V1.0   Thomas Forbriger (thof)
 *  - 12/06/2012   V1.1   settraceheader must not be called before actual
 *                        number of samples ist set
 *                        correction in skipseries()
 *  - 08/07/2016   V1.2   thof: 
 *                        - make correct use of new DATRW_report_assert
 *                        - make correct use of new DATRW_nonfatal_assert
 *  - 18/11/2016   V1.3   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_IASCIISTREAM_CC_VERSION \
  "DATRW_IASCIISTREAM_CC   V1.3"

#include<iomanip>
#include<vector>
#include<aff/subarray.h>
#include<datrwxx/ascii.h>
#include<datrwxx/formatmodifier.h>
#include<datrwxx/asciiheaderkeys.h>
#include<datrwxx/util.h>
#include<datrwxx/datatypes.h>

namespace datrw {

  const std::ios_base::openmode
    iasciistream::openmode=std::ios_base::in;

  /*----------------------------------------------------------------------*/

  iasciistream::iasciistream(std::istream& is, 
                             const std::string& modifier,
                             const bool& debug):
    Tbase(is, true, true, true, debug), Mmodifier(modifier)
  { 
    // format modifiers must be used here to set defaults
    datrw::Subformat subformat(Mmodifier);
    // WID2
    {
      libtime::TAbsoluteTime date(subformat.value(ascii::keydate,"2000/1/1"));
      this->Mdefaultwid2.date=date;
    }
    subformat(ascii::keydt, "1.") >> this->Mdefaultwid2.dt;
    subformat(ascii::keynsamples, "0") >> this->Mdefaultwid2.nsamples;
    subformat(ascii::keyhang, "-1.") >> this->Mdefaultwid2.hang;
    subformat(ascii::keyvang, "90.") >> this->Mdefaultwid2.vang;
    subformat(ascii::keycalib, "1.") >> this->Mdefaultwid2.calib;
    subformat(ascii::keycalper, "1.") >> this->Mdefaultwid2.calper;
    this->Mdefaultwid2.station=subformat.value(ascii::keystation,"NSP");
    this->Mdefaultwid2.channel=subformat.value(ascii::keychannel,"NSP");
    this->Mdefaultwid2.auxid=subformat.value(ascii::keyauxid,"NSP");
    this->Mdefaultwid2.instype=subformat.value(ascii::keyinstype,"NSP");
    // INFO
    subformat(ascii::keynstacks, "1") >> this->Mdefaultinfo.nstacks;
    subformat(ascii::keyRECVX, "0.") >> this->Mdefaultinfo.cx;
    subformat(ascii::keyRECVY, "0.") >> this->Mdefaultinfo.cy;
    subformat(ascii::keyRECVZ, "0.") >> this->Mdefaultinfo.cz;
    this->Mdefaultinfo.cs
      =::sff::coosysID(subformat.value(ascii::keyRECVCS,"C").c_str()[0]);
    // SRCE
    subformat(ascii::keySRCEX, "0.") >> this->Mdefaultsrce.cx;
    subformat(ascii::keySRCEY, "0.") >> this->Mdefaultsrce.cy;
    subformat(ascii::keySRCEZ, "0.") >> this->Mdefaultsrce.cz;
    {
      libtime::TAbsoluteTime 
        date(subformat.value(ascii::keySRCEdate,"2000/1/1"));
      this->Mdefaultsrce.date=date;
    }
    this->Mdefaultsrce.type=subformat.value(ascii::keySRCEtype,"NSP");
    this->Mdefaultsrce.cs
      =::sff::coosysID(subformat.value(ascii::keySRCECS,"C").c_str()[0]);

    this->Mnonfatal=subformat.isset(ascii::keynonfatal);

    DATRW_assert_modifiers_are_recognized(subformat, 
                                          "iasciistream");

    // read first line
    std::getline(Mis, this->Mcurrentline);
    this->readheader();
    // take FREE blocks only for traces, otherwise FREE blocks will be
    // mutliplied when reading and rewriting ASCII data
    // if (this->Mreadfree) { this->setfilefree(this->Mcurrentfree); }
    if (this->Mreadsrce) { this->setsrce(this->Mcurrentsrce); }
  } // iasciistream::iasciistream

  /*----------------------------------------------------------------------*/

  /*! this function reads all header lines until the next trace data is found
   */
  void iasciistream::readheader()
  { 
    // set defaults of header fields
    this->Mcurrentwid2=this->Mdefaultwid2;
    this->Mcurrentinfo=this->Mdefaultinfo;
    this->Mcurrentsrce=this->Mdefaultsrce;

    this->Mcurrentfree.lines.clear();

    this->Mdatatype=ascii::keydouble;

    this->Mnsamples=0;

    this->Mreadinfo=false;
    this->Mreadfree=false;
    this->Mreadsrce=false;
    // scan input lines until data is found
    while ((this->Mcurrentline.substr(0,1)=="#") && Mis.good())
    {
      if (this->Mcurrentline.substr(0,2)=="##")
      {
        this->Mreadfree=true;
        this->Mcurrentfree.append(util::trimws(this->Mcurrentline.substr(2)));
      }
      else
      {
        std::string value=this->Mcurrentline.substr(1);
        std::string key=util::clipstring(value, ":");
        value=util::trimws(value);
        key=util::trimws(key);
        if (key==ascii::keydate)
        {
          this->Mcurrentwid2.date=libtime::TAbsoluteTime(value);
        }
        else if (key==ascii::keystation)
        {
          this->Mcurrentwid2.station=value;
        }
        else if (key==ascii::keychannel)
        {
          this->Mcurrentwid2.channel=value;
        }
        else if (key==ascii::keyauxid)
        {
          this->Mcurrentwid2.auxid=value;
        }
        else if (key==ascii::keyinstype)
        {
          this->Mcurrentwid2.instype=value;
        }
        else if (key==ascii::keyRECVCS)
        {
          this->Mreadinfo=true;
          this->Mcurrentinfo.cs=::sff::coosysID(value.c_str()[0]);
        }
        else if (key==ascii::keySRCEdate)
        {
          this->Mreadsrce=true;
          this->Mcurrentsrce.date=libtime::TAbsoluteTime(value);
        }
        else if (key==ascii::keySRCEtype)
        {
          this->Mreadsrce=true;
          this->Mcurrentsrce.type=value;
        }
        else if (key==ascii::keySRCECS)
        {
          this->Mreadsrce=true;
          this->Mcurrentsrce.cs=::sff::coosysID(value.c_str()[0]);
        }
        else if (key==ascii::keydata)
        {
          this->Mdatatype=value;
        }
        else
        {
          std::istringstream iss(value);
          if (key==ascii::keynsamples)
          {
            iss >> this->Mcurrentwid2.nsamples;
          }
          else if (key==ascii::keydt)
          {
            iss >> this->Mcurrentwid2.dt;
          }
          else if (key==ascii::keyhang)
          {
            iss >> this->Mcurrentwid2.hang;
          }
          else if (key==ascii::keyvang)
          {
            iss >> this->Mcurrentwid2.vang;
          }
          else if (key==ascii::keycalib)
          {
            iss >> this->Mcurrentwid2.calib;
          }
          else if (key==ascii::keycalper)
          {
            iss >> this->Mcurrentwid2.calper;
          }
          else if (key==ascii::keynstacks)
          {
            this->Mreadinfo=true;
            iss >> this->Mcurrentinfo.nstacks;
          }
          else if (key==ascii::keyRECVX)
          {
            this->Mreadinfo=true;
            iss >> this->Mcurrentinfo.cx;
          }
          else if (key==ascii::keyRECVY)
          {
            this->Mreadinfo=true;
            iss >> this->Mcurrentinfo.cy;
          }
          else if (key==ascii::keyRECVZ)
          {
            this->Mreadinfo=true;
            iss >> this->Mcurrentinfo.cz;
          }
          else if (key==ascii::keySRCEX)
          {
            this->Mreadsrce=true;
            iss >> this->Mcurrentsrce.cx;
          }
          else if (key==ascii::keySRCEY)
          {
            this->Mreadsrce=true;
            iss >> this->Mcurrentsrce.cy;
          }
          else if (key==ascii::keySRCEZ)
          {
            this->Mreadsrce=true;
            iss >> this->Mcurrentsrce.cz;
          }
          else
          {
            if (key.length()==0)
            {
              DATRW_report_assert(key.length()>0,
                                  "keyword is missing in header line\n" <<
                                  "input line: " << Mcurrentline << "\n" <<
                                  "key: " << key);
            }
            else
            {
              if (!Mnonfatal)
              {
                std::cerr << "Illegal keyword: " << key << std::endl;
              }
              // was not recognoized
              DATRW_nonfatal_assert(Mnonfatal,
                                    false,
                                    "iasciistream: header key not recognized\n"
                                    "input line: " << Mcurrentline << "\n" <<
                                    "key: " << key);
            }
          }
        }
      }
      std::getline(Mis, this->Mcurrentline);
    } // while ((this->Mcurrentline.substr(0,1)=="#") && Mis.good())
  } // void iasciistream::readheader()

  /*----------------------------------------------------------------------*/

  void iasciistream::settraceheader()
  { 
    this->newtrace();
    if (this->Mreadfree) { this->settracefree(this->Mcurrentfree); }
    if (this->Mreadinfo) { this->setinfo(this->Mcurrentinfo); }
    if (this->Mreadsrce) { this->setsrce(this->Mcurrentsrce); }
    this->setwid2(this->Mcurrentwid2);
  } // void iasciistream::settraceheader()

  /*----------------------------------------------------------------------*/

  namespace ascii {

    namespace {

      /*! read a sequence of samples
       * \ingroup group_ascii
       *
       * Read a sequence of samples being regarded as a contiguous trace.
       *
       * \param T type of sample value (double, float, int)
       *
       * \param is input stream to read from
       * \param n expected number of samples
       * \param firstline first line from input, which is already read
       * \param series container to write samples to
       * \param nonfatal errors are made nonfatal if true
       * \return next input line
       */
      template<typename T>
        std::string readsamples(std::istream& is,
                                const unsigned int& n,
                                const std::string& firstline,
                                typename aff::Series<T>& series,
                                const bool& nonfatal)
        {
          typedef typename aff::Series<T> Tmyseries;
          std::string line=firstline;
          if (n==0)
          {
            // we do not expect a specific number of samples
            // read as many samples as can be found
            typedef typename std::vector<Tmyseries> Tvecofrecorddata;
            Tvecofrecorddata vecofrecorddata;
            unsigned int nsamples=0;
            unsigned int i=0;
            const unsigned int bufsize=5000;
            Tmyseries buffer(0,bufsize-1);
            buffer=0;
            // read data
            while ((!(line.substr(0,1)=="#")) && is.good())
            {
              if (i==bufsize)
              {
                vecofrecorddata.push_back(buffer.copyout());
                buffer=0;
                i=0;
              }
              std::istringstream iss(line);
              iss >> buffer(i);
              std::getline(is, line);
              ++i;
              ++nsamples;
            }
            if (i>0)
            {
              vecofrecorddata.push_back(aff::subarray(buffer)(0,i-1));
            }
            // data read: now copy fragments to series container
            typename Tvecofrecorddata::const_iterator I(vecofrecorddata.begin());
            series=Tmyseries(0,nsamples-1);
            nsamples=0;
            while (I != vecofrecorddata.end())
            {
              Tmyseries dest
                =aff::subarray(series)(nsamples,nsamples+I->size()-1);
              dest.copyin(*I);
              nsamples += I->size();
              ++I;
            }
          }
          else
          {
            // we expect a specfied number of samples
            series=Tmyseries(n);
            series=0; 
            unsigned int i=0;
            while ((!(line.substr(0,1)=="#")) && is.good() && (i<n))
            {
              std::istringstream iss(line);
              iss >> series(series.f()+i);
              std::getline(is, line);
              ++i;
            }
            DATRW_nonfatal_assert(nonfatal,
                                  (i==n),
                                  "readsamples (ASCII): "
                                  "found less samples than expected\n"
                                  "expected: " << n << " " <<
                                  "found: " << i);
          }
          return(line);
        } // readsamples

      /*----------------------------------------------------------------------*/

      /*! read a sequence of samples
       * \ingroup group_ascii
       *
       * Read a sequence of samples being regarded as a contiguous trace.
       * This function essential performs the correct type conversion after
       * reading the data with function readsamples().
       *
       * \param T type of sample value (double, float, int)
       *
       * \param is input stream to read from
       * \param n expected number of samples
       * \param firstline first line from input, which is already read
       * \param series container to write samples to
       * \param datatype expected type of data
       * \param nonfatal errors are made nonfatal if true
       * \return next input line
       */
      template<typename T>
        std::string readany(std::istream& is,
                            const unsigned int& n,
                            const std::string& firstline,
                            typename aff::Series<T>& series,
                            const std::string& datatype,
                            const bool& nonfatal)
        {
          //typedef typename aff::Series<T> Tinseries;
          std::string line;
          if (datatype==ascii::keydouble)
          {
            Tdseries inseries;
            line=readsamples(is, n, firstline, inseries, nonfatal); 
            util::convert(inseries, series);
          }
          else if (datatype==ascii::keyfloat)
          {
            Tfseries inseries;
            line=readsamples(is, n, firstline, inseries, nonfatal); 
            util::convert(inseries, series);
          }
          else if (datatype==ascii::keyint)
          {
            Tiseries inseries;
            line=readsamples(is, n, firstline, inseries, nonfatal); 
            util::convert(inseries, series);
          }
          else
          {
            std::cerr << "unrecognized data type: " << datatype << std::endl;
            DATRW_abort("unkown data type!");
          }
          return(line);
        }

    } // namespace

  } // namespace ascii

  /*----------------------------------------------------------------------*/

  Tdseries iasciistream::dseries()
  {
    Tdseries retval;
    this->Mcurrentline=ascii::readany(this->Mis,
                                      this->Mcurrentwid2.nsamples,
                                      this->Mcurrentline,
                                      retval,
                                      this->Mdatatype,
                                      this->Mnonfatal);
    this->Mcurrentwid2.nsamples=retval.size();
    this->settraceheader();
    this->readheader();
    if (!this->Mis.good()) { this->setlast(); }
    return(retval);
  } // Tdseries iasciistream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries iasciistream::fseries()
  {
    Tfseries retval;
    this->Mcurrentline=ascii::readany(this->Mis,
                                      this->Mcurrentwid2.nsamples,
                                      this->Mcurrentline,
                                      retval,
                                      this->Mdatatype,
                                      this->Mnonfatal);
    this->Mcurrentwid2.nsamples=retval.size();
    this->settraceheader();
    this->readheader();
    if (!this->Mis.good()) { this->setlast(); }
    return(retval);
  } // Tfseries iasciistream::fseries()

  /*----------------------------------------------------------------------*/

  Tiseries iasciistream::iseries()
  {
    Tiseries retval;
    this->Mcurrentline=ascii::readany(this->Mis,
                                      this->Mcurrentwid2.nsamples,
                                      this->Mcurrentline,
                                      retval,
                                      this->Mdatatype,
                                      this->Mnonfatal);
    this->Mcurrentwid2.nsamples=retval.size();
    this->settraceheader();
    this->readheader();
    if (!this->Mis.good()) { this->setlast(); }
    return(retval);
  } // Tiseries iasciistream::iseries()

  /*----------------------------------------------------------------------*/

  void iasciistream::skipseries()
  {
    if (this->Mcurrentwid2.nsamples==0)
    {
      while ((!(this->Mcurrentline.substr(0,1)=="#")) && Mis.good())
      {
        ++this->Mcurrentwid2.nsamples;
        std::getline(Mis, this->Mcurrentline);
      }
    }
    else
    {
      int i=0;
      while ((!(this->Mcurrentline.substr(0,1)=="#")) && Mis.good() 
             && (i<this->Mcurrentwid2.nsamples))
      {
        std::getline(Mis, this->Mcurrentline);
        ++i;
      }
    }
    this->settraceheader();
    this->readheader();
    if (!this->Mis.good()) { this->setlast(); }
  } // void iasciistream::skipseries()

  /*----------------------------------------------------------------------*/

  namespace ascii {

    namespace {

      /*!
       * \ingroup group_ascii
       * \todo
       * replace with formatmodifiers::ModifierHelp
       */
      void explainmodifier(std::ostream& os,
                           const char* const key,
                           const char* const message)
      {
        os.width(14);
        std::string keyval=key;
        keyval += "=v: ";
        os << std::left << std::setfill('.') 
          << keyval << " " << message << std::endl;
      } // void explainmodifier

      /*----------------------------------------------------------------------*/

      /*!
       * \ingroup group_ascii
       * \todo
       * replace with formatmodifiers::ModifierHelp
       */
      void explainmodifierflag(std::ostream& os,
                               const char* const key,
                               const char* const message)
      {
        os.width(14);
        std::string keyval=key;
        keyval += ": ";
        os << std::left << std::setfill('.') 
          << keyval << " " << message << std::endl;
      } // void explainmodifierflag

    } // namespace

  } // namespace ascii

  /*----------------------------------------------------------------------*/

  void iasciistream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "ASCII reading functions" << std::endl <<
      "-----------------------" << std::endl <<
      DATRW_IASCIISTREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module reads seismic time series data in the raw\n"
      "single column ASCII format. No header values are required\n"
      "but all SFF header data can be provided. Header data not\n"
      "present in the file can be set through format modifiers.\n"
      "Header data neither given in the file nor in the format are\n"
      "set to default values. The number of samples is determined from\n"
      "actual numer of samples present in the file. If a number of samples\n"
      "is defined in the header it is used as a maximum number of samples\n"
      "to be read until the next trace begins.\n"
      "Each line with a hash (#) in the first column is treated\n"
      "as a header line. Each line with a double hash (##) in the\n"
      "first two columns is treated as a free format comment. All\n"
      "other lines are treated as ASCII data.\n" 
      << std::endl;
    os << 
      "Parameter values for header fields can be set in lines which\n"
      "have a single hash (#) in the first column. The hash is expected\n"
      "to be followed by a key ID, which can be either one of the\n"
      "modifier keys listed below or can be \"" << ascii::keydata << "\".\n"
      "The key has to be separated from the value by a single colon (:).\n"
      "If you feel unsure, simply write a data file using the ASCII\n"
      "output functions and have a look at the result.\n"
      << std::endl;
    os <<
      "The data type can be selected by the key \"" 
      << ascii::keydata << "\" and one of the values:\n";
      os << "  "; os.width(7);
      os << std::left
      << ascii::keydouble << ": for double precision data\n";
      os << "  "; os.width(7);
      os << std::left
      << ascii::keyfloat << ": for single precision data\n";
      os << "  "; os.width(7);
      os << std::left
      << ascii::keyint << ": for integer data\n"
      "If no type is specified, the default will be " 
      << ascii::keydouble << "\n"
      << std::endl;
    os << 
      "The ascii input stream can be controlled by format modifiers:\n";
      ascii::explainmodifier(os, ascii::keynsamples,
        "(WID2) set number of samples");
      ascii::explainmodifier(os, ascii::keydt,
        "(WID2) set sampling interval (seconds)");
      ascii::explainmodifier(os, ascii::keydate,
        "(WID2) set time of first sample; YYYY/MM/DD/hh/mm/ss.ssssss");
      ascii::explainmodifier(os, ascii::keychannel,
        "(WID2) set channel ID");
      ascii::explainmodifier(os, ascii::keystation,
        "(WID2) set station ID");
      ascii::explainmodifier(os, ascii::keyauxid,
        "(WID2) set auxiliary ID");
      ascii::explainmodifier(os, ascii::keyinstype,
        "(WID2) set instrument type ID");
      ascii::explainmodifier(os, ascii::keycalib,
        "(WID2) set calib value");
      ascii::explainmodifier(os, ascii::keycalper,
        "(WID2) set calper value");
      ascii::explainmodifier(os, ascii::keyhang,
        "(WID2) set hang value");
      ascii::explainmodifier(os, ascii::keyvang,
        "(WID2) set vang value");
      ascii::explainmodifier(os, ascii::keySRCEdate,
        "(SRCE) set date and time of source; YYYY/MM/DD/hh/mm/ss.ssssss");
      ascii::explainmodifier(os, ascii::keySRCEtype,
        "(SRCE) set type of source");
      ascii::explainmodifier(os, ascii::keySRCEX,
        "(SRCE) set x coordinate of source");
      ascii::explainmodifier(os, ascii::keySRCEY,
        "(SRCE) set y coordinate of source");
      ascii::explainmodifier(os, ascii::keySRCEZ,
        "(SRCE) set z coordinate of source");
      ascii::explainmodifier(os, ascii::keySRCECS,
        "(SRCE) set coordinate system of source");
      ascii::explainmodifier(os, ascii::keynstacks,
        "(INFO) set number of stacks");
      ascii::explainmodifier(os, ascii::keyRECVX,
        "(INFO) set x coordinate of receiver");
      ascii::explainmodifier(os, ascii::keyRECVY,
        "(INFO) set y coordinate of receiver");
      ascii::explainmodifier(os, ascii::keyRECVZ,
        "(INFO) set z coordinate of receiver");
      ascii::explainmodifier(os, ascii::keyRECVCS,
        "(INFO) set coordinate system of receiver");
    os << "Further modifiers:\n";
      ascii::explainmodifierflag(os, ascii::keynonfatal,
        "make errors non-fatal is possible (only a warning will be issued)");
  } // void iasciistream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF iasciistream.cc ----- */
