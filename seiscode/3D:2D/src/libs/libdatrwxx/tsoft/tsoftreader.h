/*! \file tsoftreader.h
 * \brief module to read TSOFT data (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/09/2009
 * 
 * module to read TSOFT data (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TSOFTREADER_H_VERSION

#define DATRW_TSOFTREADER_H_VERSION \
  "DATRW_TSOFTREADER_H   V1.0   "

#include<datrwxx/tsoftdata.h>
#include<datrwxx/tsoftconfig.h>

namespace datrw {

  namespace tsoft {

    /*! \brief string constant to check TSOFT file id 
     * \ingroup group_tsoftconst
     */
    extern const char* TSOFTID;

    /*----------------------------------------------------------------------*/

    /*! \brief check list for requiered fields
     * \ingroup group_tsoft
     */
    struct Checklist {
      public:
        //! default constructor
        Checklist();
        //! check whether all are checked
        bool allchecked() const;
        //! report check status
        void report(std::ostream& os) const;
      public:
        bool fileid;
        bool timeformat;
        bool increment;
        bool channels;
        bool units;
        bool undetval;
        bool data;
      private:
        static void reportitem(std::ostream& os,
                               const char* tag,
                               const bool& flag);
    }; // struct Checklist

    /*----------------------------------------------------------------------*/

    /*! \brief read a complete file
     * \ingroup group_tsoft
     */
    class TSOFTfile {
      public:
        //! create by reading
        TSOFTfile(std::istream& is, 
                  const bool& debug=false)
        { 
          this->read(is, debug); 
        }
        //! create by reading
        TSOFTfile(std::istream& is, 
                  const ReaderConfig& rc,
                  const bool& debug=false):
          Mdatacontainer(rc), Mreaderconfig(rc)
        { 
          this->read(is, debug); 
        }
        //! read from stream
        void read(std::istream& is, 
                  const bool& debug=false);
        //! return number of channels
        int nchannels() const { return Mdatacontainer.nchannels(); }
        //! return number of channels
        const Datacontainer& dc() const { return Mdatacontainer; }
        //! return number of channels
        Datacontainer& dc() { return Mdatacontainer; }
        //! return free lines
        const Tlos& free() const { return Mfree; }
      private:
        //! store header
        Tlos Mfree;
        //! store channel info
        Datacontainer Mdatacontainer;
        //! sampling interval
        libtime::TRelativeTime Mincrement;
        //! store undetval
        double Mundetval;
        //! my checklist
        Checklist Mchecklist;
        //! Reader configuration
        ReaderConfig Mreaderconfig;
    }; // class TSOFTfile

  } // namespace tsoft

} // namespace datrw

#endif // DATRW_TSOFTREADER_H_VERSION (includeguard)

/* ----- END OF tsoftreader.h ----- */
