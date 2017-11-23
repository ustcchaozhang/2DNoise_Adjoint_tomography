/*! \file sffostream.h
 * \brief SFF output stream (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/02/2004
 * 
 * SFF output stream (prototypes)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/02/2004   V1.0   Thomas Forbriger
 *  - 21/04/2004   V1.1   some improvements - provide definition of use
 *  - 08/09/2004   V1.2   notice: documentaion gave wrong order for
 *                        SFFostream operators
 *  - 27/07/2005   V1.3   use a deep copy - that's the save way to postpone
 *                        the writing of a time series, it decouples our copy
 *                        from references hold in the calling program (which
 *                        might modify the samples)
 *  - 28/04/2006   V1.4   added using declaration for standard output streams
 *                        for debug code
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SFFOSTREAM_H_VERSION

#define TF_SFFOSTREAM_H_VERSION \
  "TF_SFFOSTREAM_H   V1.4"

#include<sffxx.h>
#include<iostream>
#include<new>

namespace sff {

namespace helper {

  class ostream_manager {
    public:
      ostream_manager(std::ostream& os, const bool& debug=false);
      void setfileheader(const FileHeader&);
      void settraceheader(const TraceHeader&);
      void setinfo(const INFO&);
      void setwid2(const WID2&);
      void setsrce(const SRCE&);
      void setfree(const FREE&);
      void setnormmode(const Enormmode&);
      void nextislast();
      void clearlast();
      TraceHeader traceheader() const;
      const FileHeader& fileheader() const;
      const Enormmode& normmode() const;
    protected:
      //! flush file header to output (if not done yet)
      void flushheader();
      std::ostream& Mos;
    private:
      FileHeader Mfileheader;
      TraceHeader Mtraceheader;
      bool Mlast;
      bool Mfileheaderwritten;
      bool Mfilefreedefined;
      bool Mtraceinprogress;
      Enormmode Mnormmode;
    protected:
      bool Mdebug;
  }; // class ostream_manager

} // namespace helper

  /*! SFF output stream
   *
   * \note
   * You have to call the member function setseries() first upon writing a new
   * trace. This will check for a pending trace to be flushed to disc. If
   * there is one, it will be written to file together with all stored header
   * information first. Then the passed series will be copied to a local
   * member data. Notice, that this will be a shallow copy, when using
   * aff::Series objects. The data may still be modified through another
   * handle from the outside, before it is written to the file.
   * After setting the series itself, header information should be passed
   * through the inherited member functions provided by ostream_manager.
   *
   * \deprecated
   * The use of class SFFostream is deprecated.
   * It should be replaced by the corresponding output class in libdatrwxx.
   * SFFostream will no longer be maintained.
   * Thomas Forbriger, 22.3.2013.
   */
  template<class C>
  class SFFostream: public helper::ostream_manager {
    public:
      typedef C Tseries;
      typedef typename C::Tcoc Tcseries;
      typedef helper::ostream_manager Tbase;
      SFFostream(std::ostream& os, const bool& debug=false): 
        Tbase(os, debug), Mhasseries(false) { }
      ~SFFostream()
      { 
        if (!Mhasseries) throw
          Terror("ERROR (~SFFostream): no waveform!");
        this->nextislast();
        write();
      }
      //! finish previous trace and start with a new one
      void setseries(const Tcseries& series);
    private:
      void write();
      Tcseries Mseries;
      bool Mhasseries;
  }; // class SFFostream

  /*----------------------------------------------------------------------*/

  template<class C>
  inline
  void
  SFFostream<C>::write()
  {
    using std::cout;
    using std::endl;
    // flush file header (in case we have to write the first trace)
    this->flushheader();
    if (Mhasseries) 
    {
      Mos << OutputWaveform<Tseries>(Mseries, 
                                     this->traceheader(),
                                     this->normmode());
      Mhasseries=false;
      if (Mdebug) 
      { 
        cout << "DEBUG (SFFostream::write): trace written:" << endl; 
        cout << "      " << this->traceheader().wid2().line().substr(0,70) 
          << endl;
        cout << "      index range: " << Mseries.f() << " - "  
                                      << Mseries.l() << "; ";
        cout << "some values: " << Mseries(Mseries.f()) 
                        << ", " << Mseries(Mseries.f()+1)
                        << ", " << Mseries(Mseries.f()+2) 
                        << ", " << Mseries(Mseries.f()+3) 
                        << endl;
      }
    }
    if (Mdebug) { cout << "DEBUG (SFFostream::write): finished" << endl; }
  } // SFFostream<C>::write()

  /*----------------------------------------------------------------------*/

  template<class C>
  inline
  void
  SFFostream<C>::setseries(const Tcseries& series)
  {
    using std::cout;
    using std::endl;
    this->write();
    // using a deep copy here is the save way!
    Mseries=series.copyout();
    Mhasseries=true;
    if (Mdebug) 
    { 
      cout << "DEBUG (SFFostream::setseries) finished:" << endl; 
      cout << "      index range: " << Mseries.f() << " - "  
                                    << Mseries.l() << endl;
      cout << "      some values: " << Mseries(Mseries.f()) 
                     << ", " << Mseries(Mseries.f()+1)
                     << ", " << Mseries(Mseries.f()+2) 
                     << ", " << Mseries(Mseries.f()+3) 
                     << endl;
    }
  } // SFFostream<C>::setseries(const Tcseries& series)

  /*----------------------------------------------------------------------*/

  //! FIRST(!) operator to be called for each trace
  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const typename C::Tcoc& c)
  { os.setseries(c); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const WID2& wid2)
  { os.setwid2(wid2); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const INFO& info)
  { os.setinfo(info); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const FREE& free)
  { os.setfree(free); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const SRCE& srce)
  { os.setsrce(srce); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const FileHeader& fh)
  { os.setfileheader(fh); return(os); }

  template<class C>
  SFFostream<C>& operator<<(SFFostream<C>& os, const TraceHeader& th)
  { os.settraceheader(th); return(os); }

} // namespace sff

#endif // TF_SFFOSTREAM_H_VERSION (includeguard)

/* ----- END OF sffostream.h ----- */
