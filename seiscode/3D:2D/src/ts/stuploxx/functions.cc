/*! \file functions.cc
 * \brief external functions and structures (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * external functions and structures (implementation)
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
 *  - 20/11/2009   V1.1   replace is declared in algorithm header
 *  - 09/09/2011   V1.2   support file format modifiers
 *  - 01/02/2014 thof:    use modules from ts:sff presented by libtsioxx
 *                        as a replacement for tfxx::tsio which has vanished
 * 
 * ============================================================================
 */
#define STUPLO_FUNCTIONS_CC_VERSION \
  "STUPLO_FUNCTIONS_CC   V1.2"

#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>

#include <tfxx/error.h>
#include <tfxx/misc.h>

#include "functions.h"
#include "constants.h"

namespace stuplo {

  /*======================================================================*/

  // ======================
  // function setparameters
  // ======================

  /*! \brief function to prepare list of file parameters.
   *
   * use default values from pgs
   *
   * All information from the command line (data file names, file specific
   * parameters, plot parameters) are compiled into an instance of
   * TFileParametersList. This is only done once. Upon rereading the data, this
   * list is used again for the following steps.
   */
  FileParametersList setparameters(const tfxx::cmdline::Tparsed& fns,
                                   const PGstyle& pgs)
  {
    FileParametersList retval;
    tfxx::cmdline::Tparsed::const_iterator I=fns.begin();
    int ifile=0;
    while (I!=fns.end())
    {
      FileParameters fp;
      fp.filename = *I;

      // set defaults
      fp.label = fp.filename.name;
      fp.fileformat=datrw::anyID(datrw::Fsff);
      fp.graphls=pgs.ls;
      fp.ipanel=Cnopanelselected;
        
      // remember number of file
      fp.ifile=++ifile;
      // set selected parameters
      if (I->haskey(panelkey))
      {
        std::string vs=I->value(panelkey);
        std::istringstream iss(vs);
        iss >> fp.ipanel;
        TFXX_assert((fp.ipanel>0), "illegal panel number on command line");
        --fp.ipanel;
      }
      if (I->haskey(colorindexkey))
      {
        std::string vs=I->value(colorindexkey);
        std::istringstream iss(vs);
        int ci;
        iss >> ci;
        fp.graphls.setci(ci);
      }
      if (I->haskey(linestylekey))
      {
        std::string vs=I->value(linestylekey);
        std::istringstream iss(vs);
        int ls;
        iss >> ls;
        fp.graphls.setls(ls);
      }
      if (I->haskey(linewidthkey))
      {
        std::string vs=I->value(linewidthkey);
        std::istringstream iss(vs);
        int lw;
        iss >> lw;
        fp.graphls.setlw(lw);
      }
      if (I->haskey(setrgbkey))
      {
        pgplot::Tcol col;
        std::string vs=I->value(setrgbkey);
        std::replace(vs.begin(), vs.end(), ',', ' ');
        std::istringstream iss(vs);
        iss >> col.r >> col.g >> col.b;
        fp.graphls.setrgb(col);
      }
      if (I->haskey(sethlskey))
      {
        pgplot::Tcol hls;
        std::string vs=I->value(sethlskey);
        std::replace(vs.begin(), vs.end(), ',', ' ');
        std::istringstream iss(vs);
        iss >> hls.h >> hls.l >> hls.s;
        fp.graphls.sethls(hls);
      }
      if (I->haskey(labelkey))
      { fp.label=I->value(labelkey); }
      if (I->haskey(formatkey))
      { fp.fileformat=I->value(formatkey); }
      if (I->haskey(scalerangekey))
      {
        fp.fixedordinatelimits=true;
        std::string vs=I->value(scalerangekey);
        std::replace(vs.begin(), vs.end(), ',', ' ');
        std::istringstream iss(vs);
        iss >> fp.ordinatelimits.min >> fp.ordinatelimits.max;
      }
      if (I->haskey(scalerangefactorkey))
      {
        std::string vs=I->value(scalerangefactorkey);
        std::istringstream iss(vs);
        iss >> fp.ordinatescalefactor;
        TFXX_assert((fp.ordinatescalefactor>=0.5),
                    "unreasonable scale factor for ordinate range");
      }
      if (I->haskey(chartstepperkey))
      {
        fp.dochartstepping=true;
        std::string vs=I->value(chartstepperkey);
        std::istringstream iss(vs);
        iss >> fp.chartsteppingwidth;
        TFXX_assert(((fp.chartsteppingwidth>=0.5)
                    && (fp.chartsteppingwidth<=1.)),
                    "unreasonable width for chart stepping");
      }
      if (I->haskey(chartstepperhystkey))
      {
        std::string vs=I->value(chartstepperhystkey);
        std::istringstream iss(vs);
        iss >> fp.chartsteppinghystheresis;
        TFXX_assert(((fp.chartsteppinghystheresis<=0.5)
                    && (fp.chartsteppinghystheresis>0.)),
                    "unreasonable hystheresis for chart stepping");
      }
      if (I->haskey(ordinatelabelkey))
      { fp.units=I->value(ordinatelabelkey); }
      if (I->haskey(annotationkey))
      { fp.annotation=I->value(annotationkey); }
      if (I->haskey(preannotationkey))
      { fp.preannotation=I->value(preannotationkey); }

      retval.push_back(fp);
      ++I;
    }
    return(retval);
  } // FileParametersList setparameters(const tfxx::cmdline::Tparsed& fns,
    //                                  const PGstyle& pgs)


  /*----------------------------------------------------------------------*/

  // =================
  // function readdata
  // =================

  /* function to read data
   *
   * data file names are passed through an instance of TFileParametersList as
   * created by function setparameters. The output of this function is an
   * instance of TDataFileList that contains all settings as passed by
   * TFileParametersList together with the actual time series as read from file.
   */
  DataFileList readdata(const FileParametersList& fpl)
  {
    DataFileList retval;
    FileParametersList::const_iterator I=fpl.begin();
    while(I != fpl.end())
    {
      DataFile df;
      df.para = *I;
      try {
        df.file = ts::sff::readSSFF(I->filename, false, 
                                    tracekey, I->fileformat);
        retval.push_back(df);
      }
      catch (...) {
        std::cerr << "Warning (stuploxx): "
          << "caught exception while reading file" << std::endl;
        std::cerr << "  " << I->filename.name << std::endl;
        std::cerr << "  skipping apparently broken file" << std::endl;
      }
      ++I;
    }
    return(retval);
  } // DataFileList readdata(const FileParametersList fpl)

  /*----------------------------------------------------------------------*/

  // ====================
  // function setuppanels
  // ====================

  /* function to set up panels on view surface
   *
   * This function takes settings and file data together through an instance of
   * TDataFileList. It redistributes the parameters together with the time
   * series into plot panels and returns an instance of type PanelVector.
   */
  PanelVector setuppanels(const DataFileList& fpl,
                          const int& npanels,
                          const bool& verbose,
                          const bool& debug)
  {
    TFXX_debug(debug, "setuppanels", 
               "create list of panels and distribute traces");
    // create target data to be returned by this function
    PanelVector retval;
    retval.resize(npanels);

    // set first panel index to be used
    int nextpanel=Cfirstpanel;

    // cycle through fpl list and put each trace into the appropriate panel
    TFXX_debug(debug, "setuppanels", 
               "cycle through input list of data files");
    DataFileList::const_iterator IF=fpl.begin();
    while (IF != fpl.end())
    {
      // local references to input data
      const FileParameters& para=IF->para;
      const TSFile& file=IF->file;

      TFXX_debug(debug, "setuppanels", 
                 "file: " << para.filename.name);
      // cycle through all traces
      TSFile::Tfile::Ttracevector::const_iterator IT=file.data.begin();
      while (IT != file.data.end())
      {
        TFXX_debug(debug, "setuppanels", 
                   "trace: " << IT->header.wid2().station 
                   << " " << IT->header.wid2().channel);
        // local reference to trace data
        const Ttimeseries& timeseries = *IT;

        // set panel index to be used and next panel index to be used
        unsigned int ipanel=para.ipanel;
        if (ipanel==Cnopanelselected)
        {
          ipanel=nextpanel;
          ++nextpanel;
        }
        else
        {
          nextpanel=ipanel+1;
        }

        // local reference to output data
        TFXX_debug(debug, "setuppanels", 
                   "create reference to selected panel: " << ipanel);
        TFXX_assert(ipanel>=0,"illegal panel index");
        if (ipanel>=retval.size()) { retval.resize(ipanel+1); }
        PanelVector::iterator IPV=retval.begin();
        for (unsigned int i=0; i< ipanel; ++i) { ++IPV; }

        // create data to be added to list of traces
        TFXX_debug(debug, "setuppanels", 
                   "copy data to DataTrace object");
        DataTrace trace;
        trace.para=para;
        trace.ts=timeseries;
        
        // copy data to panel
        TFXX_debug(debug, "setuppanels", 
                   "copy data to panel");
        IPV->dtl.push_back(trace);

        // go to next trace in this file
        TFXX_debug(debug, "setuppanels", 
                   "next trace in this file");
        ++IT;
      }

      // go to next file
      TFXX_debug(debug, "setuppanels", 
                 "next file");
      ++IF;
    }
    TFXX_debug(debug, "setuppanels", 
               "return list of panels");
    return(retval);
  } // PanelVector readdata(const TDataFileList& fpl)

  /*----------------------------------------------------------------------*/

  // =========================
  // function setinitialranges
  // =========================

  /* function to set initial ranges of world coordinates in each panel
   *
   * This function takes all data in an instance of PanelVector together with
   * additional plot style settings and defines the initial Plot layout in terms
   * of world coordinates in each panel.
   */
  void setinitialranges(PanelVector& pl, const PGstyle&)
  {
    PanelVector::iterator I=pl.begin();
    while (I != pl.end()) 
    {
      ++I;
    }
  } // set initialranges

  /*----------------------------------------------------------------------*/

  // =================
  // function plotdata
  // =================

  /* function to plot data
   *
   * This is the general plot function. It takes settings and time series
   * already sorted to plot panels through an instance of PanelVector. Further
   * it takes plot settings through an instance of PGstyle. The latter might be
   * modified interactively by the user. An the function takes the actual plot
   * device.
   */
  void plotdata(pgplot::basic_device& dev,
                const PanelVector& pl, 
                const PGstyle&)
  {
    // setup panels
    pgplot::Ttiledpanels tiledpanels(pgplot::c_rect1919,
                                     pgplot::c_rect1919,
                                     pgplot::c_rect0101, 
                                     1, pl.size());
    for (unsigned int ipanel=0; ipanel<pl.size(); ++ipanel)
    {
      Panel panel=pl[ipanel];
      //libtime::TRange trange=panel.timerange();
      //libtime::TAbsoluteTime basedate(first.year(), first.month(), first.day());
    }
    dev.env(pgplot::c_rect1919).arro(float(0.2),float(0.2),float(0.8),float(0.8));
  } // plotdata

} // namespace stuplo

/* ----- END OF functions.cc ----- */
