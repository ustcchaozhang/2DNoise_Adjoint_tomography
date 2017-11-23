/*! \file tsoftsffcontainer.cc
 * \brief a container to hold SFF data for one file (implementation)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/11/2009
 * 
 * a container to hold SFF data for one file (implementation)
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
 *  - 11/11/2009   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_TSOFTSFFCONTAINER_CC_VERSION \
  "DATRW_TSOFTSFFCONTAINER_CC   V1.0   "

#include <datrwxx/tsoftsffcontainer.h>
#include <datrwxx/tsoftreader.h>
#include <datrwxx/channeltranslation.h>
#include <aff/iterator.h>

namespace datrw {

  namespace tsoft {

    //! read complete file
    File readfile(std::istream& is, const ReaderConfig& rc)
    {
      TSOFTfile infile(is, rc);
      File outfile;
      outfile.Mfree.append(infile.free());
      const Datacontainer& dc=infile.dc();

      // count traces
      int ntraces=0;
      for (int ich=0; ich<infile.nchannels(); ++ich)
      {
        const Channeldata& cd=dc[ich];
        /*
        std::cout << "ich " << ich << 
          " has " << cd.nsequences() << " sequences" << std::endl;
          */
        ntraces += cd.ntraces();
      }
      // std::cout << "found " << ntraces << " traces" << std::endl;

      // reserve space for n traces
      outfile.Mtraces=Ttraceseries(ntraces);

      // cycle trough all input traces
      aff::Iterator<Ttraceseries> I(outfile.Mtraces);
      for (int ich=0; ich<infile.nchannels(); ++ich)
      {
        const Channeldata& cd=dc[ich];
        const Channelinfo& ci=cd.chinfo();
        sff::FREE channelfree;
        channelfree.append(channelinfofree(ci));
        for (int itr=0; itr<cd.ntraces(); ++itr)
        {
          I->Mfree=channelfree;
          const Datatrace& ds=cd.trace(itr);

          // extract samples
          I->Mseries=ds.series();

          // extract header
          I->Mwid2.date=ds.date();
          I->Mwid2.dt=libtime::time2double(ds.interval());
          I->Mwid2.nsamples=I->Mseries.size();

          TSOFTchannelid tci;
          tci.location=ci.thelocation();
          tci.instrument=ci.theinstrument();
          tci.datatype=ci.thedatatype();
          /*
          std::cout << "translate: " 
            << tci.location << ":" 
            << tci.instrument << ":"
            << tci.datatype << ":" << std::endl;
            */
          SFFchannelid sci=translate(tci);

          I->Mwid2.station=sci.station;
          I->Mwid2.channel=sci.channel;
          I->Mwid2.instype=sci.instrument;
          I->Mwid2.auxid=sci.auxid;

          ++I;
        }
      }

      return(outfile);
    } // File readfile(const std::istream& is)

  } // namespace tsoft

} // namespace datrw

/* ----- END OF tsoftsffcontainer.cc ----- */
