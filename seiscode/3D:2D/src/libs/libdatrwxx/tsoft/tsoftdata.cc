/*! \file tsoftdata.cc
 * \brief class to store TSOFT data (implementation)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/09/2009
 * 
 * class to store TSOFT data (implementation)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/09/2009   V1.0   Thomas Forbriger
 *  - 02/12/2011   V1.1   prepared for input stream modifiers
 *  - 08/07/2016   V1.2   make correct use of new DATRW_report_assert
 * 
 * ============================================================================
 */
#define DATRW_TSOFTDATA_CC_VERSION \
  "DATRW_TSOFTDATA_CC   V1.2"

#include <datrwxx/error.h>
#include <datrwxx/tsoftdata.h>
#include <sstream>

namespace datrw {

  namespace tsoft {

    /*======================================================================*/
    // string constants

    const char* tagbegin="[";
    const char* tagend="]";

    const char* tagdata="DATA";
    const char* tagchannels="CHANNELS";
    const char* tagunits="UNITS";
    const char* tagfileid="TSF-file";
    const char* tagtimeformat="TIMEFORMAT";
    const char* tagincrement="INCREMENT";
    const char* tagundetval="UNDETVAL";

    const char* NSP="NSP";
    
    /*======================================================================*/
    // class Line

    //! read one line from data file
    Line::Line(const std::string& line): 
      Mline(line), Mhastag(false), Mhascontent(false)
    {
      //! check existence of tag
      if (Mline.substr(0,1) == tagbegin)
      {
        Mhastag=true;
        // length without trailing \0
        const string::size_type len=line.length();
        // find end of tag
        string::size_type i=1;
        string::size_type j=line.find_first_of(tagend, i);
        Mtag=line.substr(i,j-i);
        j++;
        if (j<len)
        {
          i=j;
          j=line.find_first_not_of (" ", i);
          if (j <len)
          {
            Mhascontent=true;
            Mcontent=line.substr(j,len-j+1);
            trimws(Mcontent);
          }
        }
      }
    } // Line::Line(const std::string& line)

    /*======================================================================*/
    // class Channelinfo

    //! constructor for Channelinfo
    Channelinfo::Channelinfo():
      Mlocation(NSP), Minstrument(NSP), Mdatatype(NSP), Munits(NSP)
    {
    } // Channelinfo::Channelinfo()

    /*----------------------------------------------------------------------*/

    //! extract channel info from TSOFT line
    void Channelinfo::setchannelinfo(const std::string& line)
    {
      Tvos vos=stringtovec(line);
      Tvos::const_iterator I=vos.begin();
      if (I != vos.end()) { Mlocation = *I; ++I; }
      if (I != vos.end()) { Minstrument = *I; ++I; }
      if (I != vos.end()) { Mdatatype = *I; ++I; }
      // std::cout << "+++" << Mdatatype << "+++" << std::endl;
    } // Channelinfo::setchannelinfo(const std::string& line)

    /*======================================================================*/
    // class Dataline

    //! read one line of samples from data file
    Dataline::Dataline(const std::string& line):
      Mline(line)
    {
      int year,month,day,hour,minute ,second;
      std::istringstream iss(Mline);
      iss >> year >> month >> day >> hour >> minute >> second;
      DATRW_report_assert(iss.good(), 
         "read beyond end of data line when reading date and time" << line);
      DATRW_assert(iss.good(), "empty data line or read beyond end of file");
      Mtime=libtime::TAbsoluteTime(year,month,day,hour,minute,second);
      double sample;
      while (iss.good())
      {
        iss >> sample;
        //if (iss.good()) { Msamples.push_back(sample); }
        Msamples.push_back(sample); 
        // std::cout << "sample " << sample << std::endl;
      }
    } // Dataline::Dataline(const std::string& line)

    /*----------------------------------------------------------------------*/

    //! return one sample and check index
    double Dataline::sample(const int& i) const
    {
      DATRW_assert(((i>=0)&&(i<this->nsamples())),
        "index out of range");
      return(Msamples[i]);
    } // std::string Dataline::sample(const int& i) const

      
    /*======================================================================*/
    // class Datasequence

    Datasequence::Datasequence()
    {
      this->initialize();
    } // Datasequence::Datasequence()

    /*----------------------------------------------------------------------*/

    //! return time of next sample after last samples
    libtime::TAbsoluteTime Datasequence::timeofnextsample(const bool&
                                                          debug) const
    {
      if (debug)
      {
        std::cout << "   date: " << this->date().timestring() << std::endl;
        std::cout << "   interval: " << this->interval().timestring() << std::endl;
        std::cout << "   nsamples: " << this->nsamples() << std::endl;
      }
      libtime::TAbsoluteTime retval=this->date();
      retval += (this->nsamples()*this->interval());
      return(retval);
    } // libtime::TAbsoluteTime Datasequence::timeofnextsample() const

    /*----------------------------------------------------------------------*/

    void Datasequence::initialize()
    {
      Msamples.reserve(86500);
      Msamples.clear();
      Mdate=libtime::TAbsoluteTime();
      Minterval=libtime::double2time(0);
    } // void Datasequence::initialize()

    /*----------------------------------------------------------------------*/

    Tdseries Datasequence::series() const
    {
      Tdseries series(Msamples.size());
      Tvod::const_iterator I=Msamples.begin();
      for (aff::Tsubscript i=series.f(); 
           ((i<=series.l()) && (I!=Msamples.end()));
           ++i, ++I)
      {
        series(i)= (*I);
      }
      return(series);
    } // Tdseries Datasequence::series() const

    /*======================================================================*/
    // class Channeldata
    
    Channeldata::Channeldata()
    {
      Mcollector.initialize();
    } // Channeldata::Channeldata()

    /*----------------------------------------------------------------------*/
    
    void Channeldata::flushcollector()
    {
      if (Mcollector.nsamples()>0)
      {
        Datatrace trace;
        trace.date(Mcollector.date());
        trace.interval(Mcollector.interval());
        trace.series(Mcollector.series());
        Mtraces.push_back(trace);
        Mcollector.initialize();
      }
    } // Channeldata::flushcollector()

    /*----------------------------------------------------------------------*/

    void Channeldata::push_sample(const libtime::TAbsoluteTime& intime,
                                  const double& invalue,
                                  const double& undetval,
                                  const libtime::TRelativeTime& dt,
                                  const ReaderConfig& rc,
                                  const bool& debug)
    {
      // copy value to allow modification
      double value=invalue;
      libtime::TAbsoluteTime time=intime;
      // replace flag value if requested
      if ((value == undetval) && (rc.setundetval))
      {
        value=rc.flagvalue;
      }
      // handle value if valid or if UNDETVAL values are requested to be read
      if ((value != undetval) || (rc.keepundetval))
      {
        if (debug)
        {
          std::cout << "  yes, take this sample" << std::endl;
        }
        // check, whether collector is already in use
        bool insequence = (Mcollector.nsamples() > 0);
        // bridge sample if requested
        if (rc.bridgesamples)
        {
          if (time == rc.bridgetime)
          {
            if (insequence)
            {
              time = Mcollector.timeofnextsample();
              if (rc.flagbridged)
              {
                value=rc.bridgeflagvalue;
              }
            }
            else
            {
              DATRW_report_assert(insequence,
                "WARNING (Channeldata::push_sample): TSOFT reading\n"
                "User requested to bridge data entries labelled with time: "
                << rc.bridgetime.timestring() << "\n"
                << "This time label is present in the first entry of the"
                << "input data.\n"
                << "First input lines cannot be bridged.");
            }
          }
        }
        // if collector is in use, then check is current value matches
        if (insequence)
        {
          insequence = ((time == Mcollector.timeofnextsample(debug))
            && (dt == Mcollector.interval()));
        }
        // if current value does not match: flush collector and start new
        if (!insequence)
        {
          this->flushcollector();
          Mcollector.date(time);
          Mcollector.interval(dt);
        }
        Mcollector.append(value);
      }
    } // void Channeldata::push_sample

    /*======================================================================*/
    // class Datacontainer

    void Datacontainer::push_data(const std::string& line,
                                  const double& theundetval,
                                  const libtime::TRelativeTime& dt,
                                  const bool& debug)
    {
      Dataline thedata(line);
      if (debug)
      {
        std::cout << "the line: " << line << std::endl;
        std::cout << "  time: " << thedata.time().timestring() << std::endl;
        std::cout << "  dt: " << dt.timestring() << std::endl;
        std::cout << "  undetval: " << theundetval << std::endl;
      }
      // std::cout << "nsamples " << thedata.nsamples() << std::endl;
      for (int ich=0; ich<thedata.nsamples(); ++ich)
      {
        double thesample=thedata.sample(ich);
        // std::cout << "ich " << ich << " sample " << thesample << std::endl;
        if (debug)
        {
          std::cout << "  sample: " << thesample << std::endl;
        }
        this->channel(ich).push_sample(thedata.time(),
                                        thesample, 
                                        theundetval, 
                                        dt, Mreaderconfig,
                                        debug);
      }
    } // Datacontainer::push_data

    /*----------------------------------------------------------------------*/

    void Datacontainer::flushchannels()
    {
      for (unsigned int ich=0; ich<Mchannels.size(); ++ich)
      {
        Mchannels[ich].flushcollector();
      }
    } // void Datacontainer::flushchannels()

    /*======================================================================*/
    // functions

    //! helper function stringtok
    Tvos stringtovec(const std::string& line, 
                     const std::string& delimiters)
    {
      Tvos result;
      const string::size_type len = line.length();
      string::size_type i = 0;

      while ( i < len )
      {
        // eat leading whitespace
        i = line.find_first_not_of (" ", i);
        if (i == string::npos)
          return result;   // nothing left but white space

        // find the end of the token
        string::size_type j = line.find_first_of (delimiters, i);

        // push token
        if (j == string::npos) {
          result.push_back (line.substr(i));
          return result;
        } else
          result.push_back (line.substr(i, j-i));

        // set up for next loop
        i = j + 1;
      }
      return result;
    } // Tvos stringtovec(const std::string& line,
      //        const std::string& delimiter=":")

    /*----------------------------------------------------------------------*/

    //! helper function trimws
    void trimws(std::string& line)
    {
      // std::cout << "<" << line << ">" << std::endl;
      if (line.length()>0)
      {
        // std::cout << line.length() << " > 0" << std::endl;
        string::size_type ib=line.find_first_not_of(" ", 0);
        if (ib==string::npos)
        {
          line="";
        }
        else
        {
          string::size_type il=line.find_last_not_of(" \r", line.length());
          string::size_type n=il>=ib ? il-ib+1 : 0;
          if (n==0) { ib = 0; }
          /*
          std::cout << "ib = " << ib << "; il = " << il << 
            "; n = " << n << std::endl;
            */
          if ((ib!=0) || (n!=line.length())) { line=line.substr(ib,n); }
        }
      }
    } // trimws(std::string& line)

    /*----------------------------------------------------------------------*/

    //! prepare free comment block from channel info
    Tlos channelinfofree(const Channelinfo& ci)
    {
      Tlos retval;
      std::ostringstream oss;
      oss << "location:   " << ci.thelocation();
      retval.push_back(oss.str());
      oss.str("");
      oss << "instrument: " << ci.theinstrument();
      retval.push_back(oss.str());
      oss.str("");
      oss << "datatype:   " << ci.thedatatype();
      retval.push_back(oss.str());
      oss.str("");
      oss << "units:      " << ci.theunits();
      retval.push_back(oss.str());
      return (retval);
    } // Tvos channelinfofree(const channelinfo& ci)

    /*----------------------------------------------------------------------*/

    std::string getDOSline(std::istream& is)
    {
      std::string inputline;
      getline(is, inputline);
      // check for DOS file
      // inputline.replace(inputline.find("\r"), 1, "");
      trimws(inputline);
      return(inputline);
    } // std::string getDOSline(std::istream& is)

  } // namespace tsoft

} // namespace datrw

/* ----- END OF tsoftdata.cc ----- */
