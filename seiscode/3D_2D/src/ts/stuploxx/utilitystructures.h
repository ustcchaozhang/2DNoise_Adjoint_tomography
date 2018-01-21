/*! \file utilitystructures.h
 * \brief some useful structures (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/01/2008
 * 
 * some useful structures (prototypes)
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
 *  - 29/01/2008   V1.0   Thomas Forbriger (thof)
 *  - 08/08/2008   V1.1   moved global settings to its own translation unit
 *  - 01/02/2014 thof:    use tsioxx/cmdlinefiles.h (successor of
 *                        tsxx/sffheaders.h and tfxx/readtsdata.h)
 *  - 17/03/2015 thof:    adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_UTILITYSTRUCTURES_H_VERSION

#define STUPLO_UTILITYSTRUCTURES_H_VERSION \
  "STUPLO_UTILITYSTRUCTURES_H   2015-03-17"

#include <pgplotxx/xpgplotxx.h>

#include <tsioxx/cmdlinefiles.h>

namespace stuplo {

  //! use floats for PGPLOT functions
  typedef float Tvalue;
  //! use single precision (float) SFF file
  typedef ts::sff::SFile TSFile;
  //! time series type of SFF file
  typedef TSFile::Tfile::Ttimeseries Ttimeseries;
  //! series type of SFF file
  typedef Ttimeseries::Tseries Tseries;

  // TSFile is a vector of traces in instances of type Ttimeseries

  /*----------------------------------------------------------------------*/

  /*! \brief struct to store command line options and arguments
   * these are general options not directly related to PGPLOT styles
   *
   * \deprecated this is deprecated; use GlobalSettings instead
   */
  struct Options {
    //! control verbosity
    bool verbose, debug;
    //! interactive mode
    bool interactive;
    //! repeating mode
    bool repeat;
    //! repeat interval
    int repeatinterval;
    //! issue a command prior to repetition
    bool issuerepcmd;
    //! command string
    std::string repcmd;
    //! PGPLOT device name
    std::string device;
    //! PGPLOT hardcopy device name
    std::string hdevice;
    //! command to issue after hardcopy
    std::string hardcopycommand;
    //! minimum number of panels to plot
    int npanels;
  }; // struct Options

  /*----------------------------------------------------------------------*/

  /*! \brief struct to hold style options for a graph label
   *
   * \deprecated this is deprecated; use GlobalSettings instead
   */
  struct GLstyle {
    GLstyle(): underlinelabel(false), colourlabel(false),
      eraselabelbox(false) { }
    //! underline graph label with appropriate color
    bool underlinelabel;
    //! underline graph label with appropriate line style
    bool colourlabel;
    //! erase bbox prior to plottin label
    bool eraselabelbox;
  }; // struct GLstyle

  //! compare
  bool operator!=(const GLstyle& a, const GLstyle& b);

  /*----------------------------------------------------------------------*/

  /*! \brief struct to hold PGPLOT style options
   *
   * \deprecated this is deprecated; use GlobalSettings instead
   */
  struct PGstyle {
    //! default line style
    pgplot::Tlinestyle ls;
    //! title for plot
    std::string title;
    //! text scaling factor for title
    float tstitle;
    //! graph label style
    GLstyle glstyle;
    //! graph label height as a fraction of panel height
    float graphlabelheight;
    //! reserve space for graphlabl
    bool graphlabelreserve;
    //! separator for date values
    std::string datesep;
  }; // struct PGstyle

} // namespace stuplo

#endif // STUPLO_UTILITYSTRUCTURES_H_VERSION (includeguard)

/* ----- END OF utilitystructures.h ----- */
