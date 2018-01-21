/*! \file hpmodata.h
 * \brief structures and functions to hold hpmo data and to handle it (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/12/2004
 * 
 * structures and functions to hold hpmo data and to handle it (prototypes)
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
 *  - 22/12/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_HPMODATA_H_VERSION

#define DATRW_HPMODATA_H_VERSION \
  "DATRW_HPMODATA_H   V1.0   "

#include<datrwxx/datread.h>
#include<string>

namespace datrw {

  namespace hpmo {

    //! number of channels in HP MO data acquisition system
    const int nchannels=20;
    //! number of samples per minute block and channel
    const int nsamples=12;
    //! number of minute blocks per file (1h)
    const int nminutes=60;
    //! sampling interval in seconds
    const double sampling_interval=5.;

    //! hold samples of one minute-block
    struct SampleBlock {
      double Msamples[nchannels*nsamples];
      double value(const int& ichannel, const int& isample) const
      { return(Msamples[nchannels*(isample-1)+(ichannel-1)]); }
      double operator() (const int& ichannel, const int& isample) const
      { return(this->value(ichannel, isample)); }
    }; // struct SampleBlock

    //! hold one minute block
    struct MinuteBlock {
      libtime::TAbsoluteTime Mtime;
      int Mquality_flag;
      SampleBlock Mdata;
    }; // struct MinuteBlock

  } // namespace hpmo

} // namespace datrw

#endif // DATRW_HPMODATA_H_VERSION (includeguard)

/* ----- END OF hpmodata.h ----- */
