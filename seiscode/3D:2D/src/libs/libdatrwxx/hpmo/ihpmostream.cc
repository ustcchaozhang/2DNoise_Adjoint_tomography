/*! \file ihpmostream.cc
 * \brief provide data from HP Mo (BFO data acquisition system) (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * provide data from HP-MO (BFO data acquisition system) (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 31/03/2004   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static members
 * 
 * ============================================================================
 */
#define DATRW_HPMO_CC_VERSION \
  "DATRW_HPMO_CC   V1.1"

#include <datrwxx/datread.h>
#include <datrwxx/util.h>
#include <datrwxx/hpmo.h>
#include <datrwxx/readhpmo.h>
#include <sffxx.h>
#include <iostream>
#include <sstream>

namespace datrw {

  const std::ios_base::openmode ihpmostream::openmode=std::ios_base::in;

  //@{
  /*! \brief Format properties
   * \ingroup group_hpmo
   */
  const bool hpmo::isbinary=false;
  const char* const hpmo::streamID="hpmo";
  //@}

  /*----------------------------------------------------------------------*/

  ihpmostream::ihpmostream(std::istream& is, const bool& verbose):
    Tbase(is, true, true, false),
    Mnblocks(0), Mnextblock(0), Mnextchannel(0)
  { this->read(is, verbose); }

/*----------------------------------------------------------------------*/

  Tdseries ihpmostream::dseries()
  { 
    const int firstblock=Mindex[Mnextblock];
    const int lastblock=Mindex[Mnextblock+1]-1;
    const int nblocks=lastblock-firstblock+1;
    const int nsamp=nblocks*datrw::hpmo::nsamples;
    Tdseries retval(nsamp);
    int isample=0;
    for (int iblock=firstblock; iblock<=lastblock; ++iblock)
    {
      for (int i=1; i<=datrw::hpmo::nsamples; ++i)
      {
        retval(isample)=Mdatafile[iblock].Mdata(Mnextchannel, i);
        ++isample;
      }
    }
    this->set_next_header();
    return(retval);
  } 

/*----------------------------------------------------------------------*/

  Tfseries ihpmostream::fseries()
  { return(datrw::util::convert<Tdseries, Tfseries>(this->dseries())); } 

/*----------------------------------------------------------------------*/

  void ihpmostream::skipseries()
  { 
    set_next_header();
  } 

/*----------------------------------------------------------------------*/

  void ihpmostream::read(std::istream& is, const bool& verbose)
  {
    // simply read the thing
    datrw::hpmo::Tvecofblocks blocks(datrw::hpmo::readfile(is));
    if (verbose)
    {
      std::cout << "(ihpmostream::read): read "
        << blocks.size() << " blocks of data" << std::endl;
    }
    int nblocksread=blocks.size();
    DATRW_assert((nblocksread <= datrw::hpmo::nminutes),
                   "ERROR (ihpmostream::read): " 
                   "illegal number of minute blocks in file!" "\n"
                   "  Use ihpmostream only together with original HPMO data");
    // extract the stuff now
    if (verbose) { std::cout << "(ihpmostream::read): copy block #"; }
    libtime::TRelativeTime length(datrw::hpmo::dt()*datrw::hpmo::nsamples);
    for (int iblock=0; iblock<nblocksread; iblock++)
    {
      this->Mdatafile[iblock]=blocks[iblock];
      datrw::hpmo::MinuteBlock& block=this->Mdatafile[iblock];
      if (iblock > 0)
      {
        datrw::hpmo::MinuteBlock& prevblock=this->Mdatafile[iblock-1];
        if (prevblock.Mtime+length != block.Mtime)
        {
          Mindex[Mnblocks]=iblock;
          ++Mnblocks;
          if (verbose) { 
            std::cout << std::endl 
              << "(ihpmostream::read): found gap" << std::endl; 
            std::cout << "  between block # " << iblock-1 << " @ "
              << prevblock.Mtime.timestring() << std::endl;
            std::cout << "      and block # " << iblock << " @ "
              << block.Mtime.timestring() << std::endl;
            std::cout << "(ihpmostream::read): copy block #"; 
          }
        }
      }
      else
      {
        Mindex[Mnblocks]=0;
        ++Mnblocks;
      }
      if (verbose) { std::cout << " " << iblock; }
    }
    if (verbose) 
    { 
      std::cout << std::endl; 
      std::cout << "(ihpmostream::read): extracted "
        << Mnblocks << " contiguous data block(s)" << std::endl; 
    }
    // marks last block
    Mindex[Mnblocks]=nblocksread;
    // init reading pointers
    Mnextblock=0;
    Mnextchannel=1;
    // prepare file FREE block
    std::ostringstream oss;
    oss << nblocksread;
    sff::FREE filefree;
    filefree.append("ihpmostream read " + oss.str() + " blocks from file");
    oss.str("");
    oss << Mnblocks;
    filefree.append("found " + oss.str() + " contiguous sets of blocks");
    filefree.append(qualityreports(Mdatafile, nblocksread));
    this->setfilefree(filefree);
  } // void ihpmostream::read(std::istream& is, const bool& verbose)

/*----------------------------------------------------------------------*/

  void ihpmostream::set_next_header()
  {
    const int firstblock=Mindex[Mnextblock];
    const int lastblock=Mindex[Mnextblock+1]-1;
    const int nblocks=lastblock-firstblock+1;
    const int nsamp=nblocks*datrw::hpmo::nsamples;
    // DEBUG
    // std::cout << firstblock << " " << lastblock << " "
    //   << Mnextblock << " " << Mnextchannel << std::endl;
    this->newtrace();
    // set WID2 header
    sff::WID2 wid2line;
    wid2line.station="BFO";
    wid2line.auxid="HPMO";
    wid2line.dt=datrw::hpmo::sampling_interval;
    std::ostringstream oss;
    oss << "K";
    oss.width(2); 
    oss.fill('0');
    oss << Mnextchannel;
    wid2line.channel=oss.str();
    wid2line.nsamples=nsamp;
    // calculate time of first sample for this channel
    wid2line.date=Mdatafile[firstblock].Mtime;
    wid2line.date -= libtime::TRelativeTime(0,0,0,57);
    wid2line.date += datrw::hpmo::toffset(Mnextchannel);
    this->setwid2(wid2line);
    // set FREE block of header
    oss.str("");
    oss << "channel " << Mnextchannel << " extracted from "
      << lastblock-firstblock+1 << " contiguous minute blocks";
    sff::FREE tracefree;
    tracefree.append(oss.str());
    tracefree.append(qualityreports(&Mdatafile[firstblock],
                                    lastblock-firstblock+1));
    this->settracefree(tracefree);
    // proceed pointers to next trace
    ++Mnextchannel;
    if (Mnextchannel > datrw::hpmo::nchannels)
    {
      Mnextchannel=1;
      ++Mnextblock;
      if (Mnextblock >= Mnblocks)
      {
        this->setlast();
      }
    }
  } // void ihpmostream::set_next_header()

} // namespace datrw

/* ----- END OF ihpmostream.cc ----- */
