/*! \example pgplotxxdemo.cc
 * \brief example code for the PGPLOT C++ interface
 *
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 2001 by Thomas Forbriger (IMGF Frankfurt)
 *
 * eXample for pgplot C++ interface
 *
 * \sa pgplotxxdemo.cc
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
 *  - 05/06/2001   V1.0   Thomas Forbriger
 *  - 15/03/2011   V1.1   test symbol plotting
 *  - 17/03/2015   V1.1a  rename file to adopt naming convention in Seitosh
 *
 * ============================================================================
 */

#include<iostream>
#include<string>
#include<cmath>
#include<pgplotxx/pgplotxx.h>
#include<pgplotxx/affpgplot.h>
#include<aff/shaper.h>
#include<aff/subarray.h>
#include<aff/seriesoperators.h>

using std::cout;
using std::endl;

/*! \brief example program
 *
 * Just a simple example to show how the pgplot classes may be used.
 *
 * \sa pgplot::device
 */
main()
{
  cout << "pgplotxxdemo" << endl;
  cout << "(example program for the C++ interface for PGPLOT)" << endl;

  /*
  pgplot::device d("/xserve");
  d.env(1.,5.,1.,10.,0,2);
  */
  pgplot::device d2("/xserve");
  d2.subp(2,1);
  /*
  d2.env(1.,5.,1.,10.,1,2);
  pgplot::basic_device::ldev();
  */

  //----------------------------------------------------------------------
  // go for gray plot

  pgplot::pgaff::Tarray A(aff::Shaper(0,6)(-3,3));
  for (int i=A.first(0); i<=A.last(0); ++i)
  {
    for (int j=A.first(1); j<=A.last(1); ++j)
    {
      A(i,j)=float(5*i+j);
    }
  }

  pgplot::pgaff::Tarray B1=aff::subarray(A)(4,6)(1,2);
  pgplot::pgaff::Tarray B2=aff::subarray(A)(0,4)(1,3);

  pgplot::Trect cwin(pgplot::Trange(-1.,3.),pgplot::Trange(-5.,5.));

  /*
  d2.ask();
  d2.env(cwin);
  pgplot::pgaff::Ttransform tr=pgplot::pgaff::create_transform(A,cwin);
  pgplot::Trange vrange=pgplot::pgaff::array_value_range(A).swap();
  pgplot::pgaff::gray(d2, A, vrange, tr).gwedg(vrange);
  */
  d2.env(cwin);
  pgplot::Trange vrange=pgplot::pgaff::array_value_range(A).swap();
  pgplot::pgaff::gray(d2, B1, vrange).gwedg(vrange);
  d2.env(cwin);
  pgplot::pgaff::gray(d2, B1, false).gwedg(vrange.swap());
  d2.env(cwin);
  pgplot::pgaff::gray(d2, B2);
  d2.env(cwin);
  pgplot::pgaff::gray(d2, A);
  /*
  d2.env(cwin);
  tr=pgplot::pgaff::create_transform(B2,cwin);
  vrange=pgplot::pgaff::array_value_range(B2).swap();
  pgplot::pgaff::gray(d2, B2, vrange, tr).gwedg(vrange);
  */

  //----------------------------------------------------------------------

  const int imax=100;
  pgplot::pgaff::Tseries x(0,imax-1);
  pgplot::pgaff::Tseries y(0,imax-1);
  for (int i=0; i<imax; ++i)
  {
    x(i)=std::sin(2.*3.14159265*i/imax);
    y(i)=std::cos(2.*3.14159265*i/imax);
  }
  d2.subp(1,1).env(-1.,1.,-1.,1.,1,2);
  pgplot::pgaff::line(d2,x,y);
  pgplot::pgaff::pt(d2,0.6*x,0.6*y,-3);
  for (int i=0; i<imax; ++i)
  {
    d2.pt1(x(i),y(i),i);
    d2.pt1(0.8*x(i),0.8*y(i),-i);
  }
  
}
 
/* ----- END OF pgplotxxdemo.cc ----- */
