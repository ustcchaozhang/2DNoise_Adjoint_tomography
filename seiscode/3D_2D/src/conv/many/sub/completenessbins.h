/*! \file completenessbins.h
 * \brief class to define completenessbins (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * class to define completenessbins (prototypes)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of the conv/many suite.
 *
 * The conv/many suite is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 12/02/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_COMPLETENESSBINS_H_VERSION

#define TF_COMPLETENESSBINS_H_VERSION \
  "TF_COMPLETENESSBINS_H   V1.0   "

#include <tfxx/error.h>
#include <libtime++.h>

/*! \brief class to define the time axis of the completeness time series.
 *
 * This class provides a time axis for the temporal distirbution of gaps or a
 * statistic of completeness.
 * For the reason of good taste the time axis is aligned to 0 UT. 
 * The first bin is aligned at an integer multiple of the bin size after 0 UT
 * which is equal to or earlier than the earliest sample under consideration.
 * The last bin is adjusted such that it contains the latest time under
 * consideration.
 * This means, that the beginning of the last bin plus the binsize is greater
 * than the lastet time under consideration.
 *
 * To check whether the time of a sample falls in a bin, check whether its
 * time is larger than or equal the beginning of the bin and smaller than the
 * beginning of the next bin.
 *
 * For the calculation of completeness, we just consider samples which are
 * expected within the earliest sample and the latest sample under
 * consideration.
 * Hence the first and the last bin will be considered complete even if not
 * the whole binsize is filled with samples.
 */
class CompletenessBins {
  public:
    CompletenessBins(const libtime::TAbsoluteTime& earliest,
                     const libtime::TAbsoluteTime& latest,
                     const libtime::TRelativeTime& binsize);
    /*! \name global query functions
     *@{
     */
    libtime::TAbsoluteTime earliest() const  { return(Mearliest); }
    libtime::TAbsoluteTime latest() const  { return(Mlatest); }
    libtime::TAbsoluteTime firstbin() const  { return(Mfirstbin); }
    libtime::TRelativeTime binsize() const  { return(Mbinsize); }
    unsigned int nbins() const  { return(Mnbins); }
    /*! \brief number of samples to be expected in a bin
     * \param dt sampling interval
     * \return number of samples in this bin
     */
    unsigned int samplesinbin(const libtime::TRelativeTime dt) const;
    //@}
      
    //! \name bin specific query functions
    //@{
    //! \brief beginnig of bin (according to bin raster)
    libtime::TAbsoluteTime bin(const unsigned int& i) const;
    //! \brief end of bin (will be latest for last bin)
    libtime::TAbsoluteTime endofbin(const unsigned int& i) const;
    //! \brief start of bin (will be earliest for fisrt bin)
    libtime::TAbsoluteTime nextbin(const unsigned int& i) const;
    /*! check if d is in bin
     * \param d date and time to check
     * \param i bin number to check for
     * \return true if \a d falls into bin \a i
     */
    bool isinbin(const libtime::TAbsoluteTime& d,
                 const unsigned int& i) const;
    /*! \brief number of samples to be expected in this bin where absolute
     * limits of the considered time window is earliest and latest.
     * \param i bin index
     * \param dt sampling interval
     * \return number of samples in this bin
     */
    unsigned int samplesinbin(const unsigned int& i,
                              const libtime::TRelativeTime dt) const;
    //@}

    /*! find appropriate bin
     * \param d date and time to find appropriate bin for
     * \return bin number into which \a falls
     */
    unsigned int bin(const libtime::TAbsoluteTime& d) const;
  private:
    libtime::TAbsoluteTime Mfirstbin;
    libtime::TRelativeTime Mbinsize;
    libtime::TAbsoluteTime Mearliest;
    libtime::TAbsoluteTime Mlatest;
    unsigned int Mnbins;
}; // class CompletenessBins

#endif // TF_COMPLETENESSBINS_H_VERSION (includeguard)

/* ----- END OF completenessbins.h ----- */
