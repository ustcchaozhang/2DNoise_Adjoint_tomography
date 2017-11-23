/*! \file fapid_sff_close.cc
 * \brief mimic Fortran subroutine sff_close (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * mimic Fortran subroutine sff_close (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 18/11/2010   V1.0   Thomas Forbriger
 *  - 23.12.2010   V1.1   close input and output streams
 *  - 30/09/2011   V1.2   no error must be returned if no file for this file
 *                        unit is open, since all reading and writing
 *                        functions automatically close the file upon the last
 *                        trace is processed
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_CLOSE_CC_VERSION \
  "TF_FAPID_SFF_CLOSE_CC   V1.2"

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>

using namespace fapidxx;

/*! \brief Close SFF file
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_close(lu, ierr)
c
c  Calling this subroutine instead of the Fortran close functions
c  provides interface compatibility to libfapidxx
c
c  Input:
c        lu:      Fortran file unit
c  Output:
c        ierr:    error status (ok: ierr=0)
c
      integer lu, ierr
c----------------------------------------------------------------------
 * \endcode
 */
int sff_close__(integer *lu, integer *ierr)
{
  int retval=0;
  *ierr=0;
  try {
    if (istreammanager.isopen(static_cast<int>(*lu)))
    { istreammanager.close(static_cast<int>(*lu)); }
    if (ostreammanager.isopen(static_cast<int>(*lu)))
    { ostreammanager.close(static_cast<int>(*lu)); }
  }
  catch(...) {
    *ierr=1;
  }
  return retval;
} // int sff_close__

/* ----- END OF fapid_sff_close.cc ----- */
