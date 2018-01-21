/*! \file datafile.h
 * \brief all stuff to handle data files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle data files (prototypes)
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
 *  - 28/01/2008   V1.0   Thomas Forbriger (thof)
 *  - 09/09/2011   V1.1   support file format modifiers
 *  - 01/02/2014 thof:    use tsioxx/cmdlinefiles.h (successor of
 *                        tsxx/sffheaders.h and tfxx/readtsdata.h)
 *  - 17/03/2015 thof:    adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_DATAFILE_H_VERSION

#define STUPLO_DATAFILE_H_VERSION \
  "STUPLO_DATAFILE_H   2015-03-17"

#include<list>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <pgplotxx/xpgplotxx.h>
#include <datrwxx/readany.h>
#include <tsioxx/cmdlinefiles.h>

#include "constants.h"
#include "utilitystructures.h"

namespace stuplo {

  /*! \brief struct to hold data file parameters.
   *
   * this essentially is a copy of the settings passed on the command line
   */
  struct FileParameters {
    FileParameters():
      dochartstepping(false), 
      chartsteppingwidth(0.9), 
      chartsteppinghystheresis(0.4),
      fixedordinatelimits(false),
      ordinatescalefactor(1.),
      units(Cstringnotspecified)
    { }
    //! complete file specific settings as obtained from command line
    tfxx::cmdline::Filename filename;
    //! file type (dataformat)
    std::string fileformat;
    //! label for trace captions (possibly containing patterns)
    std::string label;
    //! graph line style setting
    pgplot::Tlinestyle graphls;
    //! panel to plot all traces in
    int ipanel;
    //! activate chart stepping function
    bool dochartstepping;
    //! width of chart stepping plot region
    float chartsteppingwidth;
    //! hystheresis for chart stepping
    float chartsteppinghystheresis;
    //! activate fixed ordinatelimits
    bool fixedordinatelimits;
    //! values for fixed ordinate limits
    pgplot::Trange ordinatelimits;
    //! scaling factor for ordinate limits, if no fixed limits are used
    float ordinatescalefactor;
    //! units to annotate plot
    std::string units;
    //! numer of file on command line
    int ifile;
    //! additional annotation to graph (append)
    std::string annotation;
    //! additional annotation to graph (prepend)
    std::string preannotation;
  }; // struct FileParameters

  typedef std::list<FileParameters> FileParametersList;

  /*----------------------------------------------------------------------*/

  // struct to hold data together with file parameters
  struct DataFile {
    FileParameters para;
    TSFile file;
    // return time range for data in this file
    libtime::TRange timerange() const;
  }; // struct DataFile

  /*----------------------------------------------------------------------*/
  
  class DataFileList: public std::list<DataFile> {
    public:
      typedef std::list<DataFile> Tbase;
      // return time range for data in this file
      libtime::TRange timerange() const;
  }; // class DataFileList

} // namespace stuplo

#endif // STUPLO_DATAFILE_H_VERSION (includeguard)

/* ----- END OF datafile.h ----- */
