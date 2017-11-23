/*! \file offset.cc
 * \brief offset calculation (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/03/2007
 * 
 * offset calculation (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 *  - 02/03/2007   V1.0   Thomas Forbriger
 *  - 13/04/2010   V1.1   use my own error handling code (not libtfxx)
 *  - 29/07/2013   V1.2   added function sourcedistance
 * 
 * ============================================================================
 */
#define TF_OFFSET_CC_VERSION \
  "TF_OFFSET_CC   V1.2"

#include <cmath>
#include <sffxx.h>

namespace sff {

  //! return offset in meters
  double offset(const SRCE& srce, const INFO& info,
                const double& radius)
  {
    double retval;
    SFF_assert((srce.cs == info.cs),
                "ERROR (offset): inconsistent coordinate systems!");
    if (srce.cs == CS_spherical)
    {
      const double convfac=0.017453293;
      double rtet=(90.-info.cx)*convfac;
      double stet=(90.-srce.cx)*convfac;
      double rphi=info.cy*convfac;
      double sphi=srce.cy*convfac;
      double cosepi=cos(stet)*cos(rtet)+sin(stet)*sin(rtet)*cos(sphi-rphi);
      retval=acos(cosepi)*radius*1.e3;
    }
    else if (srce.cs == CS_cartesian)
    {
      double dx=srce.cx-info.cx;
      double dy=srce.cy-info.cy;
      retval=sqrt(dx*dx+dy*dy);
    }
    else
    {
      SFF_abort("ERROR (offset): unknown coordinate system!");
    }
    return(retval);
  } // double offset(const SRCE& srce, const INFO& info;
    //               const double& radius=6371.)

  /*----------------------------------------------------------------------*/

  //! return spatial distance between source and receiver in meters
  double sourcedistance(const SRCE& srce, const INFO& info)
  {
    double retval;
    SFF_assert((srce.cs == info.cs),
                "ERROR (sourcedistance): inconsistent coordinate systems!");
    SFF_assert((srce.cs == CS_cartesian),
                "ERROR (sourcedistance): "
                "only provided in Cartesian coordinates!");
    if (srce.cs == CS_cartesian)
    {
      double dx=srce.cx-info.cx;
      double dy=srce.cy-info.cy;
      double dz=srce.cz-info.cz;
      retval=sqrt(dx*dx+dy*dy+dz*dz);
    }
    else
    {
      SFF_abort("ERROR (sourcedistance): unknown coordinate system!");
    }
    return(retval);
  } // double sourcedistance(const SRCE& srce, const INFO& info)

  /*----------------------------------------------------------------------*/

  //! return offset in degrees
  double offsetdeg(const SRCE& srce, const INFO& info, 
                   const double& radius)
  {
    double retval=offset(srce, info, radius)*1.e-3*360./(2.*M_PI*radius);
    return(retval);
  } // double offsetdeg(const SRCE& srce, const INFO& info,
    //                  const double& radius=6371.)

} // namespace sff

/*----------------------------------------------------------------------*/

/* 
 * Fortran function
 *
      character*1 cs,rs
      real c1,c2,c3,r1,r2,r3
cE
      real distance
      real rtet, stet, rphi, sphi
      real convfac, earthradius, cosepi
      parameter(convfac=0.017453293, earthradius=6371.)
c
      if (cs.ne.rs) stop 'ERROR (sffu_offset): different reference frames'
      if (cs.eq.'C') then
        distance=sqrt((c1-r1)**2+(c2-r2)**2+(c3-r3)**2)
      elseif (cs.eq.'S') then
c        print *,'source ',c1,c2,c3
c        print *,'receiver ',r1,r2,r3
        rtet=(90.-r1)*convfac 
        stet=(90.-c1)*convfac 
        rphi=r2*convfac
        sphi=c2*convfac
c        print *,'source ',stet,sphi
c        print *,'receiver ',rtet,rphi
        cosepi=cos(stet)*cos(rtet)+sin(stet)*sin(rtet)*cos(sphi-rphi)
        distance=acos(cosepi)*earthradius*1.e3
c        print *,'distance ',distance
      else
        stop 
     &'ERROR (sffu_offset): reference frame is neither cartesian nor spherical'
      endif
      sffu_offset=distance
      return
      end
cS
*/

/* ----- END OF offset.cc ----- */
