/*! \file hack_blitztogrepg.cc
 * \brief a quick hack to create a grepg file from any 2D blitz array (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/11/2002
 * 
 * a quick hack to create a grepg file from any 2D blitz array (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 29/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_HACK_BLITZTOGREPG_CC_VERSION \
  "TF_HACK_BLITZTOGREPG_CC   V1.0   "

#include <fstream>
#include <tfxx/misc.h>
#include <tfxx/fortranio.h>
#include <tfxx/complexio.h>
#include <tfxx/blitzutil.h>
#include <tfxx/hack_blitztogrepg.h>

namespace tfxx {
namespace hacks {

//! create grepg file from complex array
void write_grepg_c(const blitz::Array<std::complex<float>, 2>& array,
                   const char* filename,
                   const double& dx=1., const double& dy=1.)
{
  std::ofstream os(filename);
  tfxx::ioswap::file_magic_write(os, "1234", true);
  tfxx::fortranio::FortranBinOutput fos(os);
  int nom=array.extent(blitz::firstDim);
  int fom=array.lbound(blitz::firstDim);
  int nslo=array.extent(blitz::secondDim);
  int fslo=array.lbound(blitz::secondDim);
  fos << nom << nslo;
  fos.end_block();
  for (int i=0; i<nom; i++) 
  { fos << float(3.14159265358979311*2.*(i+fom-1)*dx); }
  for (int i=0; i<nslo; i++) { fos << float(1.e-3*(i+fslo-1)*dy); }
  fos.end_block();
  for (int i=0; i<nslo; i++)
  {
    for (int j=0; j<nom; j++)
    {
      fos << array(j+fom,i+fslo);
    }
  }
  fos.end_block();
}

/*----------------------------------------------------------------------*/

//! create grepg file from float array
void write_grepg_f(const blitz::Array<float, 2>& array,
                   const char* filename,
                   const double& dx=1., const double& dy=1.)
{
  // create intermediate complex data
  blitz::Array<std::complex<float>, 2> tmp(blitz::fortranArray);
  tfxx::blitzutil::setToSameDomain(tmp, array.domain());
  tmp=blitz::cast<std::complex<float> >(array);
  write_grepg_c(tmp, filename, dx, dy);
}

} // namespace hacks
} // namespace tfxx

/* ----- END OF hack_blitztogrepg.cc ----- */
