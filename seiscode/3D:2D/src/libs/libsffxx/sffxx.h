/*! \file sffxx.h
 * \brief SFF library (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2003
 * 
 * SFF library (prototypes)
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
 *  - 23/12/2003   V1.1
 *                         - first version writing SFF successfully
 *                         - starting with reading code
 *  - 11/01/2004   V1.2  
 *                         - FileHeader modification functions
 *                         - TraceHeader modification functions
 *  - 23/12/2004   V1.3    added full block append to FREE
 *  - 27/03/2006   V1.4    introduced sff::STAT::decode_libversion
 *  - 27/06/2006   V1.5   added INFO comparison
 *  - 02/03/2007   V1.6   added offset calculation
 *  - 26/06/2007   V1.7   added verbose output
 *  - 13/04/2010   V1.8   
 *                        - start implementing WIDX
 *                        - provide error handling macros
 *  - 03/05/2010   V1.9   provide debug output for WIDX compilation
 *  - 29/07/2013   V1.10  added function sourcedistance
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SFFXX_H_VERSION

#define TF_SFFXX_H_VERSION \
  "TF_SFFXX_H   V1.10"

#include<string>
#include<cmath>
#include<list>
#include<iostream>
#include<sstream>
#include<libtime++.h>
#include<aff/iterator.h>
//#include<tfxx/commandline.h>
#include<gsexx.h>

/*! \brief all SFF modules
 */
namespace sff {

/*======================================================================*/
// basic error handling
// --------------------

  class Terror: public GSE2::Terror 
  {
    public:
      Terror(const std::string& message): GSE2::Terror(message) { }
  }; // class Terror

//! check condition
#define SFF_assert( C , M ) \
  if (!(C)) { \
    std::ostringstream os; \
    os << "Condition " << #C << " at line #" << __LINE__  \
      << " in " << __FILE__ << " is false: " << M; \
    throw(Terror(os.str())); \
  }

//! abort by throwing an exception
#define SFF_abort( M ) \
  { \
    std::ostringstream os; \
    os << "ABORT at line #" << __LINE__  \
      << " in " << __FILE__ << " because of " << M; \
    throw(Terror(os.str())); \
  }

/*======================================================================*/
// enum
// ----
  
  //! valid coordinate systems
  enum Ecoosys {
    CS_cartesian,
    CS_spherical
  }; // enum Ecoosys

  char coosysID(const Ecoosys& csid);
  Ecoosys coosysID(const char& csid);

/*----------------------------------------------------------------------*/

  enum Enormmode {
    NM_one,       //!< do not scale
    NM_maxdyn,    //!< scale for maximum dynamic range
    NM_ifneeded   //!< scale if largest amplitude larger than limit
  }; // enum Enormmode

/*======================================================================*/
// constants
// ---------

  /*! \brief ID for extended WID2 format.
   *
   * This WID format is an extension with respect to SFF and breaks the SFF
   * format definition. 
   *
   * \sa WIDXline(std::istream& is)
   * \sa WIDXline(const sff::WID2& wid2)
   */
  const char *const WIDXID = "WIDX";

/*======================================================================*/
// SFF structs
// -----------

  struct STAT {
    static const double libversion;
    static const double decode_libversion;
    static const char* const LINEID;
    STAT();
    STAT(std::istream& is, const bool& debug=false) { read(is, debug); }
    std::string line() const;
    void read(std::istream& is, const bool& debug=false);
    void setstamp(const libtime::TAbsoluteTime& date) const;
    public:
      mutable std::string timestamp;
      bool hasfree;
      bool hassrce;
  }; // struct STAT

  struct FREE {
    typedef std::list<std::string> Tlines;
    static const char* const LINEID;
    FREE();
    FREE(std::istream& is) { read(is); }
    void write(std::ostream& os) const;
    void read(std::istream& is, const bool& debug=false);
    void append(const std::string& line) { lines.push_back(line); }
    void append(const Tlines& newlines);
    void append(const FREE& free) { this->append(free.lines); }
    Tlines lines;
  }; // struct FREE

  struct SRCE {
    static const char* const LINEID;
    SRCE();
    SRCE(std::istream& is) { read(is); }
    std::string line() const;
    void read(std::istream& is, const bool& debug=false);
    public:
      std::string type;
      libtime::TAbsoluteTime date;        //!< time of source
      Ecoosys cs;
      double cx, cy, cz;
  }; // struct SRCE

  struct DAST {
    static const char* const LINEID;
    DAST();
    DAST(std::istream& is) { read(is); }
    std::string line() const;
    void read(std::istream& is, const bool& debug=false);
    public:
      int nchar;
      double ampfac;
      bool hasfree;
      bool hasinfo;
      bool last;
  }; // struct DAST

  struct INFO {
    static const char* const LINEID;
    INFO();
    INFO(std::istream& is) { read(is); }
    std::string line() const;
    void read(std::istream& is);
    bool operator==(const INFO& info) const;
    public:
      Ecoosys cs;
      double cx, cy, cz;
      int nstacks;
  }; // struct INFO

  /*! \brief Waveform Header
   * 
   */
  struct WID2 {
    WID2();
    WID2(std::istream& is) { read(is); }
    std::string line() const;
    void read(std::istream& is);
    public:
      libtime::TAbsoluteTime date;       //!< time of first sample
      std::string           station;     //!< Station code
      std::string           channel;     //!< FDSN channel code
      std::string           auxid;       //!< Auxiliary identification code
      int                   nsamples;    //!< number of samples
      double                dt;          //!< sampling interval (sec)
      double                calib;       //!< calibration factor
      double                calper;      //!< calibration reference period
      std::string           instype;     //!< instrument type
      double                hang;        //!< horizontal orientation
      double                vang;        //!< veritcal orientation
  }; // struct WID2

/*======================================================================*/
// file I/O classes
// ----------------

  class FileHeader {
    public:
      FileHeader()
      { Mstat.hasfree=false; Mstat.hassrce=false; }
      FileHeader(const FREE& free): 
        Mfree(free) 
      { Mstat.hasfree=true; Mstat.hassrce=false; }
      FileHeader(const SRCE& srce): 
        Msrce(srce)
      { Mstat.hasfree=false; Mstat.hassrce=true; }
      FileHeader(const SRCE& srce, const FREE& free):
        Mfree(free), Msrce(srce)
      { Mstat.hasfree=true; Mstat.hassrce=true; }
      FileHeader(std::istream& is, const bool& debug=false) 
      { read(is, debug); }
      void write(std::ostream&) const;
      void read(std::istream&, const bool& debug=false);
      const STAT& stat() const { return(Mstat); }
      const FREE& free() const { return(Mfree); }
      const SRCE& srce() const { return(Msrce); }
      const bool& hassrce() const { return(Mstat.hassrce); }
      const bool& hasfree() const { return(Mstat.hasfree); }
      void appendfree(const std::string& line)
      { Mstat.hasfree=true; Mfree.lines.push_back(line); }
      void setfree(const FREE& free)
      { Mstat.hasfree=true; Mfree=free; }
      void setsrce(const SRCE& srce)
      { Mstat.hassrce=true; Msrce=srce; }
    private:
      STAT Mstat;
      FREE Mfree;
      SRCE Msrce;
  }; // class FileHeader

  /*! \brief class to normalize waveforms
   */
  class WaveformNormalizer {
    public:
      static const int limit;
      WaveformNormalizer(const Enormmode& nm, const double& maxval); 
      const double& maxval() const { return(Mmaxval); }
      const double& ampfac() const { return(Mampfac); }
      const bool& scale() const { return(Mscale); }
    private:
      double Mampfac;
      double Mmaxval;
      bool Mscale;
      Enormmode Mnorm;
  }; // class WaveformNormalizer

  class TraceHeader {
    public:
      TraceHeader(): Mdebug(false) { }
      TraceHeader(const WID2& wid2, const bool& last=false): 
        Mwid2(wid2), Mdebug(false)
      { Mdast.hasfree=false; Mdast.hasinfo=false; Mdast.last=last; }
      TraceHeader(const WID2& wid2, const FREE& free, const bool& last=false): 
        Mwid2(wid2), Mfree(free), Mdebug(false)
      { Mdast.hasfree=true; Mdast.hasinfo=false; Mdast.last=last; }
      TraceHeader(const WID2& wid2, const INFO& info, const bool& last=false): 
        Mwid2(wid2), Minfo(info), Mdebug(false)
      { Mdast.hasfree=false; Mdast.hasinfo=true; Mdast.last=last; }
      TraceHeader(const WID2& wid2, const INFO& info, 
                  const FREE& free, const bool& last=false):
        Mwid2(wid2), Mfree(free), Minfo(info), Mdebug(false)
      { Mdast.hasfree=true; Mdast.hasinfo=true; Mdast.last=last; }
      void writeheader(std::ostream&) const;
      void writetrailer(std::ostream&) const;
      void readheader(std::istream&);
      void readtrailer(std::istream&);
      bool last() const { return(Mdast.last); }
      template<class C> void scanseries(const C&, 
                                        const Enormmode& nm=NM_maxdyn);
      const WID2& wid2() const { return(Mwid2); }
      const DAST& dast() const { return(Mdast); }
      const FREE& free() const { return(Mfree); }
      const INFO& info() const { return(Minfo); }
      const bool& scale() const { return(Mscale); }
      const bool& hasinfo() const { return(Mdast.hasinfo); }
      const bool& hasfree() const { return(Mdast.hasfree); }
      void setlast(const bool& flag) { Mdast.last=flag; }
      void setdebug(const bool& flag) { Mdebug=flag; }
      void setwid2(const WID2& wid2line) { Mwid2=wid2line; }
      void setnsamples(const long int& n) { Mwid2.nsamples=n; }
      void setinfo(const INFO& infoline) 
      { Mdast.hasinfo=true; Minfo=infoline; }
      void setfree(const FREE& free)
      { Mdast.hasfree=true; Mfree=free; }
      void appendfree(const std::string& line)
      { Mdast.hasfree=true; Mfree.lines.push_back(line); }
    private:
      WID2 Mwid2;
      DAST Mdast;
      FREE Mfree;
      INFO Minfo;
      bool Mscale;
      bool Mdebug;
  }; // class TraceHeader

  template<class C>
    class OutputWaveform {
      public:
        typedef typename C::Tcoc Tcoc;
        OutputWaveform(const Tcoc& c, const TraceHeader& th,
                       const Enormmode& nm=NM_maxdyn):
          Mseries(c), Mheader(th) 
          { Mheader.scanseries(Mseries,nm); }
        void write(std::ostream& os) const;
      private:
        Tcoc Mseries;
        mutable TraceHeader Mheader;
    }; // class Waveform

  template<class C>
    class InputWaveform {
      public:
        typedef C Tcontainer;
        InputWaveform(const bool& debug=false): 
          Mvalid(false), Mdebug(debug) { }
        InputWaveform(std::istream& is, const bool& debug=false):
          Mdebug(debug) { this->read(is); }
        void read(std::istream& is);
        const bool& valid() const { return(Mvalid); }
        Tcontainer series() const { return(Mseries); }
        TraceHeader header() const { return(Mheader); }
        const bool& last() const { return(Mheader.dast().last); }
      private:
        bool Mvalid;
        Tcontainer Mseries;
        TraceHeader Mheader;
        bool Mdebug;
    }; // class InputWaveform

  class SkipWaveform {
    public:
      SkipWaveform(): Mvalid(false) { }
      SkipWaveform(std::istream& is) { this->read(is); }
      void read(std::istream& is);
      const bool& valid() const { return(Mvalid); }
      TraceHeader header() const { return(Mheader); }
      const bool& last() const { return(Mheader.dast().last); }
    private:
      bool Mvalid;
      TraceHeader Mheader;
  }; // class SkipWaveform

/*======================================================================*/
// I/O operators
// -------------

  inline std::istream& operator >> (std::istream& is, FileHeader& fh)
  { fh.read(is); return(is); }
  template<class C>
  inline std::istream& operator >> (std::istream& is, InputWaveform<C>& wf)
  { wf.read(is); return(is); }
  inline std::istream& operator >> (std::istream& is, SkipWaveform& swf)
  { swf.read(is); return(is); }
  inline std::ostream& operator << (std::ostream& os, const FileHeader& fh)
  { fh.write(os); return(os); }
  inline std::ostream& operator << (std::ostream& os, const TraceHeader& th)
  { 
    th.writeheader(os);
    os << "DATA!" << std::endl;
    th.writetrailer(os);
    return(os);
  }
  template<class C> 
    inline std::ostream& operator << (std::ostream& os, const
                                      OutputWaveform<C>& wf)
  { wf.write(os); return(os); }

/*======================================================================*/
// WaveformNormalizer template functions
// -------------------------------------

  template<class C>
    inline
    void TraceHeader::scanseries(const C& c,
                                 const Enormmode& nm)
    {
      Mwid2.nsamples=0;
      double maxval=0.;
      double absval, value;
      double null(0);
      for(aff::Browser<C> i(c); i.valid(); ++i)
      { 
        Mwid2.nsamples++;
        value= *i;
        absval= (value < null) ? -value : value;
        maxval= (maxval < absval) ? absval : maxval;
      }
      double dmaxval=double((maxval==0) ? WaveformNormalizer::limit : maxval);
      WaveformNormalizer normalizer(nm, dmaxval);
      Mdast.ampfac=normalizer.ampfac();
      Mscale=normalizer.scale();
    } // TraceHeader::scanseries

/*----------------------------------------------------------------------*/
// template OutputWaveform functions
// ---------------------------------

  template<class C>
    inline
    void OutputWaveform<C>::write(std::ostream& os) const
    {
      Mheader.setnsamples(Mseries.size());
      Mheader.writeheader(os);;
      GSE2::waveform::TDAT2writeCM6 fwriter(Mheader.wid2().nsamples);
      int idata;
      typename C::Tvalue data;
      for(aff::Browser<Tcoc> i(Mseries); i.valid(); ++i)
      { 
        data= *i;
        if (Mheader.scale())
        { idata=int(round(data/Mheader.dast().ampfac)); }
        else
        { idata=int(round(data)); }
        os << fwriter(idata);
      }
      Mheader.writetrailer(os);
    } // OutputWaveform::write

/*----------------------------------------------------------------------*/
// template InputWaveform functions
// --------------------------------

  template<class C>
    inline
    void InputWaveform<C>::read(std::istream& is) 
    {
      typedef typename C::Tvalue Tvalue;
      if (Mdebug)
      {
        std::cerr << "DEBUG (InputWaveform<C>::read): " << std::endl
          << "  calling function readheader() of member Mheader." 
          << std::endl;
      }
      Mheader.setdebug(Mdebug);
      Mheader.readheader(is);
      int nsamples=Mheader.wid2().nsamples;
      GSE2::waveform::TDAT2readCM6 freader(nsamples);
      try {
        Mseries=C(nsamples);
      }
      catch(...) {
        std::cerr << "ERROR (InputWaveform::read): "
          << "allocating series for " << nsamples << " samples!" << std::endl;
        throw;
      }
      for(aff::Iterator<C> i(Mseries); i.valid(); ++i)
      { (*i) = Tvalue(freader(is)*Mheader.dast().ampfac); }
      Mheader.readtrailer(is);
      Mvalid=true;
    } // InputWaveform::read

/*======================================================================*/
// some utilities
// --------------


  /*----------------------------------------------------------------------*/
  // compare WID2 headers
  // --------------------
  /*! \brief bit values to select WID2 fields to be compared
   * \sa sff::WID2compare
   */
  enum Ewid2field {
    //! compare dates of first sample
    Fdate    =1<<0,
    //! compare station IDs
    Fstation =1<<1,
    //! compare channel IDs
    Fchannel =1<<2,
    //! compare auxilliary IDs
    Fauxid   =1<<3,
    //! compare numbers of samples
    Fnsamples=1<<4,
    //! compare sampling intervals
    Fdt      =1<<5,
    //! compare calib fields
    Fcalib   =1<<6,
    //! compare calper fields
    Fcalper  =1<<7,
    //! compare instrument type strings
    Finstype =1<<8,
    //! compare hang fields
    Fhang    =1<<9,
    //! compare vang fields
    Fvang    =1<<10
  }; // enum Ewid2field

  /*! \brief compares selected fields from two WID2 objects
   * \sa sff::Ewid2field
   */
  class WID2compare {
    public:
      /*! \brief create compare object for comparison of selected fields
       *
       * \sa sff::Ewid2field
       */
      WID2compare(const int& flags=(Fstation | Fchannel | Fdt)):
        Mflags(flags), Mdttolerance(0.), Mdatetolerance(0.) { }
      void set(const int& flags) { Mflags=Mflags | flags; }
      void clear(const int& flags) { Mflags=Mflags & (0xffffffff ^ flags); }
      //! tolerance when comparing dt (as a fraction of the sampling interval)
      void setdttolerance(const double& tol) { Mdttolerance=tol; }
      //! tolerance when comparing date (as a fraction of the sampling interval)
      void setdatetolerance(const double& tol) { Mdatetolerance=tol; }
      bool operator()(const WID2& hd1, const WID2& hd2) const;
      int flags() const { return(Mflags); }
      double dttolerance() const { return(Mdttolerance); }
      double datetolerance() const { return(Mdatetolerance); }
    private:
      int Mflags;
      double Mdttolerance;
      double Mdatetolerance; //!< relative to mean sampling interval
  }; // class WID2compare

  /*======================================================================*/
  // functions
  // ---------
  
  //! return time of last sample in waveform
  libtime::TAbsoluteTime wid2lastsample(const WID2& wid2);
  //! return time of next first sample for contiguous data
  libtime::TAbsoluteTime wid2nextdate(const WID2& wid2);
  //! return index for sample at given date
  long int wid2isample(const WID2& wid2, 
                       const libtime::TAbsoluteTime& idate);
  //! return time for sample at given index
  libtime::TAbsoluteTime wid2isample(const WID2& wid2, 
                                     const long int& i);
  //! return time interval between idate and sample sample next to idate
  libtime::TRelativeTime wid2isamplerest(const WID2& wid2, 
                                         const libtime::TAbsoluteTime& idate);

  //! return ID string for synthtic time reference
  std::string srce_reference_ID();
  //! return synthetic time reference from nothing
  sff::SRCE srce_reference();

  //! return offset in meters
  double offset(const SRCE& srce, const INFO& info,
                const double& radius=6371.);
  //! return spatial distance between source and receiver in meters
  double sourcedistance(const SRCE& srce, const INFO& info);
  //! return offset in degrees
  double offsetdeg(const SRCE& srce, const INFO& info, 
                   const double& radius=6371.);

  //! write WID2 information in extended format
  std::string WIDXline(const sff::WID2& wid2, const bool& debug=false);

  //! read WID2 information with extended format
  sff::WID2 WIDXline(const std::string& line);

  /*======================================================================*/
  // verbose output
  // --------------

  void verbose(std::ostream& os, const WID2& wid2);
  void verbose(std::ostream& os, const SRCE& srce);
  void verbose(std::ostream& os, const DAST& dast);
  void verbose(std::ostream& os, const INFO& info);
  void verbose(std::ostream& os, const FREE& free);
  void verbose(std::ostream& os, const STAT& stat);
  void verbose(std::ostream& os, const FileHeader& fh);
  void verbose(std::ostream& os, const TraceHeader& th);

} // namespace sff

#endif // TF_SFFXX_H_VERSION (includeguard)

/* ----- END OF sffxx.h ----- */
