/*! \file completenessbins.cc
 * \brief class to define completenessbins (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/02/2012
 * 
 * class to define completenessbins (implementation)
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
#define TF_COMPLETENESSBINS_CC_VERSION \
  "TF_COMPLETENESSBINS_CC   V1.0   "

#include <tfxx/error.h>
#include "completenessbins.h"

//! constructor
CompletenessBins::CompletenessBins(const libtime::TAbsoluteTime& earliest,
                                   const libtime::TAbsoluteTime& latest,
                                   const libtime::TRelativeTime& binsize)
{
  Mlatest=latest;
  Mearliest=earliest;
  this->Mbinsize=binsize;

  // initialize beginning of first bin
  // align to midnight
  this->Mfirstbin=libtime::TAbsoluteTime(earliest.year(),
                                         earliest.month(),
                                         earliest.day());
  // find appropriate first bin
  while (Mfirstbin < earliest)
  {
    Mfirstbin += binsize;
  }

  // find number of bins required to span the entire interval
  Mnbins=1; 
  libtime::TAbsoluteTime current=Mfirstbin+binsize;
  while (current <= latest)
  {
    Mnbins++;
    current += binsize;
  }
} // CompletenessBins::CompletenessBins

/*----------------------------------------------------------------------*/

libtime::TAbsoluteTime CompletenessBins::bin(const unsigned int& i) const
{
  TFXX_assert((i>=0) && (i<this->nbins()), "illegal bin index");
  libtime::TAbsoluteTime retval=this->firstbin();
  retval += i*this->binsize();
  return(retval);
} // libtime::TAbsoluteTime CompletenessBins::bin(const unsigned int& i) const

/*----------------------------------------------------------------------*/

libtime::TAbsoluteTime CompletenessBins::nextbin(const unsigned int& i) const
{
  libtime::TAbsoluteTime nextbin=this->bin(i)+this->binsize();
  return(nextbin);
} // libtime::TAbsoluteTime CompletenessBins::nextbin(const unsigned int& i)
  //  const
    
/*----------------------------------------------------------------------*/

bool CompletenessBins::isinbin(const libtime::TAbsoluteTime& d,
                               const unsigned int& i) const
{
  libtime::TAbsoluteTime thisbin=this->bin(i);
  libtime::TAbsoluteTime nextbin=this->nextbin(i);
  return((d>=thisbin) && (d<nextbin));
} // bool CompletenessBins::isinbin(const libtime::TAbsoluteTime& d, 
  // const unsigned int& i) const

/*----------------------------------------------------------------------*/

unsigned int CompletenessBins::bin(const libtime::TAbsoluteTime& d) const
{
  TFXX_assert((d >= this->earliest()) && (d <= this->latest()),
              "date is out of range");
  unsigned int retval=0;
  while ((retval<Mnbins) && !this->isinbin(d, retval)) { ++retval; }
  return(retval);
} // unsigned int CompletenessBins::bin(const libtime::TAbsoluteTime& d) const

/*----------------------------------------------------------------------*/

unsigned int
CompletenessBins::samplesinbin(const unsigned int& i,
                               const libtime::TRelativeTime dt) const
{
  libtime::TRelativeTime binsize=this->binsize();
  if (i==0)
  {
    binsize=this->nextbin(i)-this->earliest();
  }
  else if ((i+1)==this->nbins())
  {
    binsize=dt+this->latest()-this->bin(i);
  }
  unsigned int nsamples=binsize/dt;
  nsamples = nsamples > 0 ? nsamples : 1;
  return(nsamples);
} // unsigned int
  // CompletenessBins::samplesinbin(const unsigned int& i,
  //   const libtime::TRelativeTime dt) const

/*----------------------------------------------------------------------*/

unsigned int
CompletenessBins::samplesinbin(const libtime::TRelativeTime dt) const
{
  libtime::TRelativeTime binsize=this->binsize();
  unsigned int nsamples=binsize/dt;
  nsamples = nsamples > 0 ? nsamples : 1;
  return(nsamples);
} // unsigned int
  // CompletenessBins::samplesinbin(const libtime::TRelativeTime dt) const

/* ----- END OF completenessbins.cc ----- */
