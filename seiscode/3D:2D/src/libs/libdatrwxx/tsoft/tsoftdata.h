/*! \file tsoftdata.h
 * \brief class to store TSOFT data (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/09/2009
 * 
 * class to store TSOFT data (prototypes)
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
 *  - 02/12/2011   V1.1   prepared for input stream modifiers;
 *                        provides ReaderConfig
 *                 V1.2   moved ReaderConfig to tsoftconfig.h
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TSOFTDATA_H_VERSION

#define DATRW_TSOFTDATA_H_VERSION \
  "DATRW_TSOFTDATA_H   V1.2"

#include<iostream>
#include<string>
#include<vector>
#include<list>
#include<libtime++.h>
#include<datrwxx/error.h>
#include<datrwxx/datread.h>
#include<datrwxx/tsoftconfig.h>

namespace datrw {


  /*! \brief All classes and functions to extract data from TSOFT files
   * 
   * TSOFT is available from
   * http://seismologie.oma.be/TSOFT/tsoft.html
   *
   * \defgroup group_tsoft Reading module for: TSOFT data
   */

  /*! \brief All classes and functions to extract data from TSOFT files
   *
   * \ingroup group_tsoft
   */
  namespace tsoft {

    /*! \brief string constants 
     *
     * \defgroup group_tsoftconst TSOFT string constants
     * @{
     * \ingroup group_tsoft
     */
    //! \brief first character of TSOFT tag
    extern const char* tagbegin;
    //! \brief last character of TSOFT tag
    extern const char* tagend;

    //! \brief TSOFT data tag
    extern const char* tagdata;
    //! \brief TSOFT channels tag
    extern const char* tagchannels;
    //! \brief TSOFT units tag
    extern const char* tagunits;
    //! \brief TSOFT fileid tag
    extern const char* tagfileid;
    //! \brief TSOFT timeformat tag
    extern const char* tagtimeformat;
    //! \brief TSOFT increment tag
    extern const char* tagincrement;
    //! \brief TSOFT undetval tag
    extern const char* tagundetval;
      
    //! \brief value not specified
    extern const char* NSP;
    //@}

    /*! \brief vector of strings.
     *\ingroup group_tsoft
     */
    typedef std::vector<std::string> Tvos;
    /*! \brief list of strings.
     *\ingroup group_tsoft
     */
    typedef std::list<std::string> Tlos;
    /*! \brief vector of doubles.
     *\ingroup group_tsoft
     */
    typedef std::vector<double> Tvod;

    /*----------------------------------------------------------------------*/

    /*! \brief helper function stringtok
     * \ingroup group_tsoft
     */
    Tvos stringtovec(const std::string& line, 
                     const std::string& delimiter=":");

    /*----------------------------------------------------------------------*/

    /*! \brief helper function trimws
     * \ingroup group_tsoft
     */
    void trimws(std::string& line);

    /*----------------------------------------------------------------------*/

    /*! \brief helper function getDOSline
     * \ingroup group_tsoft
     */
    std::string getDOSline(std::istream& is);

    /*----------------------------------------------------------------------*/

    /*! \brief contains takes one line and splits it into tag and information.
     * \ingroup group_tsoft
     */
    class Line {
      public:
        //! constructor: swallow line
        Line(const std::string &line);
        //! return line
        std::string theline() const { return Mline; }
        //! return tag
        std::string thetag() const { return Mtag; }
        //! return content
        std::string thecontent() const { return Mcontent; }
        //! check whether this line has a tag
        bool hastag() const { return Mhastag; }
        //! check whether this line has a content
        bool hascontent() const { return Mhascontent; }
      private:
        //! the actual line
        std::string Mline;
        //! the tag part of the line
        std::string Mtag;
        //! the content part of the line
        std::string Mcontent;
        //! true if this line contains a tag
        bool Mhastag;
        //! true if this line has a content
        bool Mhascontent;
    }; // class Line

    /*----------------------------------------------------------------------*/

    /*! \brief contains channel info.
     * \ingroup group_tsoft
     */
    class Channelinfo {
      public:
        //! standard constructor
        Channelinfo();
        //! set channel info
        void setchannelinfo(const std::string& line);
        //! set unit info
        void setunits(const std::string& line) 
        { 
          Munits=line; 
          trimws(Munits); 
        }
        //! return location
        const std::string& thelocation() const { return Mlocation; }
        //! return instrument
        const std::string& theinstrument() const { return Minstrument; }
        //! return datatype
        const std::string& thedatatype() const { return Mdatatype; }
        //! return units
        const std::string& theunits() const { return Munits; }
      private:
        //! location field in channel name
        std::string Mlocation;
        //! instrument field in channel name
        std::string Minstrument;
        //! data type field in channel name
        std::string Mdatatype;
        //! units
        std::string Munits;
    }; // class Channelinfo

    /*----------------------------------------------------------------------*/

    /*! \brief vector of channel info.
     * \ingroup group_tsoft
     */
    typedef std::vector<Channelinfo> Tvoci;

    /*----------------------------------------------------------------------*/

    /*! \brief contains one data line.
     * \ingroup group_tsoft
     */
    class Dataline {
      public:
        //! swallow a line
        Dataline(const std::string& line);
        //! return time
        libtime::TAbsoluteTime time() const { return Mtime; }
        //! return vector of samples
        Tvod vectorofsamples() const { return Msamples; }
        //! return number of samples in line
        int nsamples() const { return Msamples.size(); }
        //! return specific sample
        double sample(const int& i) const;
      private:
        //! the data line
        std::string Mline;
        //! time
        libtime::TAbsoluteTime Mtime;
        //! vector of data values
        Tvod Msamples;
    }; // class Dataline

    /*----------------------------------------------------------------------*/

    /*! \brief trace of contiguous data.
     * \ingroup group_tsoft
     */
    class Datatrace {
      public:
        //! set time of first sample
        void date(const libtime::TAbsoluteTime d) { Mdate=d; }
        //! set sampling interval
        void interval(const libtime::TRelativeTime i) { Minterval=i; }
        //! set series
        void series(const Tdseries& s) { Mseries=s; }
        //! return time of first sample
        libtime::TAbsoluteTime date() const { return Mdate; }
        //! return sampling interval
        libtime::TRelativeTime interval() const { return Minterval; }
        //! return series 
        Tdseries series() const { return Mseries; }
      private:
        //! samples
        Tdseries Mseries;
        //! time of first sample
        libtime::TAbsoluteTime Mdate;
        //! sampling interval
        libtime::TRelativeTime Minterval;
    }; // class Datatrace

    /*----------------------------------------------------------------------*/

    /*! \brief sequence of contiguous data.
     * \ingroup group_tsoft
     */
    class Datasequence {
      public:
        //! initialize
        Datasequence();
        //! append sample 
        void append(const double& v) { Msamples.push_back(v); }
        //! set time of first sample
        void date(const libtime::TAbsoluteTime d) { Mdate=d; }
        //! set sampling interval
        void interval(const libtime::TRelativeTime i) { Minterval=i; }
        //! return sample
        double sample(const unsigned int& i) const
        {
          DATRW_assert(((i<Msamples.size()) && (i>=0)),
                          "illegal sample index");
          return Msamples[i];
        }
        //! return number of samples
        int nsamples() const { return Msamples.size(); }
        //! return time of first sample
        libtime::TAbsoluteTime date() const { return Mdate; }
        //! return time of next sample after last samples
        libtime::TAbsoluteTime timeofnextsample(const bool& debug=false) const;
        //! return sampling interval
        libtime::TRelativeTime interval() const { return Minterval; }
        //! initialize data sequence
        void initialize();
        //! return samples in a series container
        Tdseries series() const;
      private:
        //! samples
        Tvod Msamples;
        //! time of first sample
        libtime::TAbsoluteTime Mdate;
        //! sampling interval
        libtime::TRelativeTime Minterval;
    }; // class Datasequence

    /*----------------------------------------------------------------------*/

    /*! \brief vector of sequences.
     * \ingroup group_tsoft
     */
    typedef std::vector<Datatrace> Tvodt;

    /*----------------------------------------------------------------------*/

    /*! \brief date for one channel.
     * \ingroup group_tsoft
     */
    class Channeldata {
      public:
        //! constructor to initialize
        Channeldata();
        //! push a sample
        void push_sample(const libtime::TAbsoluteTime& time,
                         const double& value,
                         const double& undetval,
                         const libtime::TRelativeTime& dt,
                         const ReaderConfig& rc,
                         const bool& debug=false);
        //! return data trace
        const Datatrace& trace(const unsigned int& i) const
        {
          DATRW_assert(((i<Mtraces.size()) && (i>=0)),
                          "illegal trace index");
          return Mtraces[i];
        }
        //! return data trace
        Datatrace& trace(const unsigned int& i) 
        {
          DATRW_assert(((i<Mtraces.size()) && (i>=0)),
                          "illegal trace index");
          return Mtraces[i];
        }
        //! set channel info
        void chinfo(const Channelinfo& ci) { Mchannelinfo=ci; }
        //! return channel info
        const Channelinfo& chinfo() const { return(Mchannelinfo); }
        //! return channel info
        Channelinfo& chinfo() { return(Mchannelinfo); }
        //! number of data traces
        int ntraces() const { return Mtraces.size(); }
        //! function to flush collector to vector of sequences
        void flushcollector();
      private:
        //! function to reset collector
        void resetcollector();
        //! data traces
        Tvodt Mtraces;
        //! vector of samples to collect from file
        Datasequence Mcollector;
        //! channel info
        Channelinfo Mchannelinfo;
    }; // class Channeldata

    /*----------------------------------------------------------------------*/

    /*! \brief prepare free comment block from channel info.
     * \ingroup group_tsoft
     */
    Tlos channelinfofree(const Channelinfo& ci);

    /*----------------------------------------------------------------------*/

    /*! \brief vector of channels.
     * \ingroup group_tsoft
     */
    typedef std::vector<Channeldata> Tvocd;

    /*----------------------------------------------------------------------*/

    /*! \brief data container.
     * \ingroup group_tsoft
     */
    class Datacontainer {
      public:
        //! Default constructor
        Datacontainer() { }
        //! Config constructor
        Datacontainer(const ReaderConfig& rc): Mreaderconfig(rc) { }
        //! push a data line
        void push_data(const std::string& line,
                       const double& undetval,
                       const libtime::TRelativeTime& dt,
                       const bool& debug=false);
        //! return data for specific channel
        Channeldata& channel(const unsigned int& i,
                             const bool& debug=false) 
        {
          this->resizetoindex(i);
          return(Mchannels[i]);
        }
        //! return data for specific channel
        const Channeldata& channel(const unsigned int& i,
                                   const bool& debug=false) const
        {
          this->checkindex(i);
          return(Mchannels[i]);
        }
        //! return data for specific channel
        Channeldata& operator[](const unsigned int& i) 
        {
           return (this->channel(i));
        }
        //! return data for specific channel
        const Channeldata& operator[](const unsigned int& i) const
        {
           return (this->channel(i));
        }
        //! return number of channels
        int nchannels() const { return Mchannels.size(); }
        //! flush all channels
        void flushchannels();
      private:
        //! resize to index
        void resizetoindex(const unsigned int& i)
        {
          if (i>=Mchannels.size()) 
          { 
            Mchannels.resize(i+1); 
          }
          this->checkindex(i);
        }
        //! check index
        void checkindex(const unsigned int& i) const
        {
          DATRW_assert(((i<Mchannels.size()) && (i>=0)),
                          "illegal channel index");
        }
        //! reader configuration
        ReaderConfig Mreaderconfig;
        //! data sequences
        Tvocd Mchannels;
    }; // class Datacontainer

  } // namespace tsoft

} // namespace datrw

#endif // DATRW_TSOFTDATA_H_VERSION (includeguard)

/* ----- END OF tsoftdata.h ----- */
