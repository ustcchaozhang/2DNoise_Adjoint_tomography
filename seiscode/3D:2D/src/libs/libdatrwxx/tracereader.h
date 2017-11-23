/*! \file tracereader.h
 * \brief provide more efficient reading of sequential traces (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/07/2008
 * 
 * provide more efficient reading of sequential traces (prototypes)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 08/07/2008   V1.0   Thomas Forbriger
 *  - 07/09/2011   V1.1   support format modifier strings
 *  - 29/11/2011   V1.2   delegate to ianystream not idatstream
 *  - 29/11/2011   V1.3   introduced assertopen; query functions must not be
 *                        called with no file being open
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TRACEREADER_H_VERSION

#define DATRW_TRACEREADER_H_VERSION \
  "DATRW_TRACEREADER_H   V1.3"

#include <datrwxx/readany.h>
#include <fstream>

namespace datrw {

  /*! \brief Sequential trace reader
   *
   * \ingroup group_readany
   *
   * this is a wrapper for ianystream reading
   *
   * You can select a specific trace for a given filename. The reader keeps
   * track of the position within the file. If the previous reading operation
   * was for the same file and the selected trace is a successor of the
   * current trace the reader will only skip to the new trace and provide it
   * through its interface. Only if the selected file is not yet open or if
   * the trace is a predecessor of the selected trace, the file read from the
   * beginning.
   *
   * The trace reader must provide an explicite interface to the underlying
   * ianyread object. This is necessary to keep track of reading operations.
   * This class provides the same access interface that idatstream provides,
   * including input operators. You can easily replace instances if ianystream
   * and idatstream in your code with instances of sequentialtracereader.
   */
  class sequentialtracereader {
    public:
      //! constructor
      sequentialtracereader(const bool& debug=false): 
        Mdebug(debug), Mopen(false) { }
      //! destructor
      ~sequentialtracereader();
      /*! select a specific trace from a given file
       *
       * return true if successful
       */
      bool select(const std::string& filename,
                  const int& itrace,
                  const Eformat& format);
      bool select(const std::string& filename,
                  const int& itrace,
                  const std::string& format);
      bool last() const { this->assertopen(); return(MPias->last()); }
      bool good() const { this->assertopen(); return(MPias->good()); }
      bool hasfree() const { this->assertopen(); return(MPias->hasfree()); }
      bool hassrce() const { this->assertopen(); return(MPias->hassrce()); }
      bool hasinfo() const { this->assertopen(); return(MPias->hasinfo()); }
      bool providesd() const { this->assertopen(); return(MPias->providesd()); }
      bool providesf() const { this->assertopen(); return(MPias->providesf()); }
      bool providesi() const { this->assertopen(); return(MPias->providesi()); }
      sff::FREE free() const { this->assertopen(); return(MPias->free()); }
      sff::SRCE srce() const { this->assertopen(); return(MPias->srce()); }
      sff::INFO info() const { this->assertopen(); return(MPias->info()); }
      sff::WID2 wid2() const { this->assertopen(); return(MPias->wid2()); }
      void skipseries() 
      { this->assertopen(); ++Mindex; return(MPias->skipseries()); }
      Tdseries dseries() 
      { this->assertopen(); ++Mindex; return(MPias->dseries()); }
      Tfseries fseries() 
      { this->assertopen(); ++Mindex; return(MPias->fseries()); }
      Tiseries iseries() 
      { this->assertopen(); ++Mindex; return(MPias->iseries()); }
    private:
      // assert that file is open
      void assertopen() const;
      // operate in debug mode
      bool Mdebug;
      // is there an open file?
      bool Mopen;
      // input stream
      std::istream* MPis;
      // series input stream
      datrw::ianystream* MPias;
      // name of open file
      std::string Mfilename;
      // index of current trace
      int Mindex;
  }; // class sequentialtracereader

  /*----------------------------------------------------------------------*/

  //@{
  /*! \brief explicite input operator for sequentialtracereader
   * \ingroup group_readany
   */
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           sff::WID2& wid2)
  { wid2=is.wid2(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           sff::SRCE& srce)
  { srce=is.srce(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           sff::INFO& info)
  { info=is.info(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           sff::FREE& free)
  { free=is.free(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           Tdseries& series)
  { series=is.dseries(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           Tfseries& series)
  { series=is.fseries(); return(is); }

  //! \ingroup group_readany
  inline sequentialtracereader& operator>>(sequentialtracereader& is,
                                           Tiseries& series)
  { series=is.iseries(); return(is); }
  //@}

} // namespace datrw

#endif // DATRW_TRACEREADER_H_VERSION (includeguard)

/* ----- END OF tracereader.h ----- */
