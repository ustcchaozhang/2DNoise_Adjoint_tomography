/*! \file greenspec.cc
 * \brief class handling greens spectra (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * class handling greens spectra (implementation)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 04/01/2003   V1.0   Thomas Forbriger
 *  - 09/02/2010   V1.1   moved initialization of magic code
 * 
 * ============================================================================
 */
#define TF_GREENSPEC_CC_VERSION \
  "TF_GREENSPEC_CC   V1.2"
#define TF_GREENSPEC_CC_CVSID \
  "$Id$"

#include <gremlin1/greenspec.h>
#include <tfxx/fortranio.h>
#include <tfxx/complexio.h>
#include <tfxx/error.h>
#include <aff/array.h>

namespace gremlin1 {

  const char GreenSpectrum::Mcmagic[]="1234";

  std::istream& operator>>(std::istream& is, GreenSpectrum& green)
  {
    using tfxx::fortranio::FortranBinInput;
    // check magic number
    tfxx::ioswap::Emagic_type magic=
      tfxx::ioswap::file_magic_test(is, GreenSpectrum::Mcmagic, true);
    TFXX_assert(((magic==tfxx::ioswap::magic_match)||
           (magic==tfxx::ioswap::magic_swap)),
             "Green data contains incorrect magic number");
    // attach to stream
    FortranBinInput fis(is, (magic==tfxx::ioswap::magic_swap));

    int nfre,nslo;
    fis >> nfre >> nslo;

    aff::Array<float> omega(nfre),slow(nslo);

    for (int i=omega.f(0); i<=omega.l(0); ++i)
    { fis >> omega(i); } 
    for (int i=slow.f(0); i<=slow.l(0); ++i)
    { fis >> slow(i); } 

    const double pi2=2.*3.141592653589793115997;
    double fmin=omega(omega.f(0))/pi2;
    double fmax=omega(omega.l(0))/pi2;
    double pmin=slow(slow.f(0));
    double pmax=slow(slow.l(0));

    green=GreenSpectrum(fmin,fmax,nfre,pmin,pmax,nslo);

    GreenSpectrum::Tcarray values=green.green();

    for (int i=values.f(0); i<=values.l(0); ++i)
    {
      for (int j=values.f(1); j<=values.l(1); ++j)
      {
        fis >> values(i,j);
      }
    }

    return(is);
  }

  /*----------------------------------------------------------------------*/

  std::ostream& operator<<(std::ostream& os, const GreenSpectrum& green)
  {
    // attach to stream
    tfxx::fortranio::FortranBinOutput fos(os);
    // write magic number
    fos << tfxx::ioswap::magic(GreenSpectrum::Mcmagic);
    fos.end_block();

    fos << green.nf() << green.np();
    fos.end_block();

    GreenSpectrum::Tcarray::Tcoc values=green.green();

    const double pi2=2.*3.141592653589793115997;
    for (int i=values.f(1); i<=values.l(1); ++i)
    { fos << green.f(i)*pi2; } 
    for (int i=values.f(0); i<=values.l(0); ++i)
    { fos << green.p(i); } 
    fos.end_block();

    for (int i=values.f(0); i<=values.l(0); ++i)
    {
      for (int j=values.f(1); j<=values.l(1); ++j)
      {
        fos << values(i,j);
      }
    }
    fos.end_block();

    return(os);
  }

}

/* ----- END OF greenspec.cc ----- */
