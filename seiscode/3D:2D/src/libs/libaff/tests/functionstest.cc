/*! \file functionstest.cc
 * \brief test function templates
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/03/2005
 * 
 * test function templates
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
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 20/03/2005   V1.0   Thomas Forbriger
 *  - 19/06/2006   V1.1   check functions with containers of const
 * 
 * ============================================================================
 */
#define FUNCTIONSTEST_VERSION \
  "FUNCTIONSTEST   V1.1   test function templates"

#include <iostream>
#include <aff/dump.h>
#include <aff/array.h>
#include <aff/shaper.h>
#include <aff/subarray.h>
#include <aff/functions/avg.h>
#include <aff/functions/min.h>
#include <aff/functions/max.h>
#include <aff/functions/absmax.h>
#include <aff/functions/rms.h>
#include <aff/functions/sum.h>
#include <aff/functions/sqrsum.h>
#include <aff/functions/histo.h>
#include <aff/functions/valmap.h>

using namespace aff;

using std::cout;
using std::cerr;
using std::endl;

typedef std::map<double, int> Tvalmap;

int main(int iargc, char* argv[])
{
  CODE( typedef aff::Array<double> Tarray );
  CODE( Tarray A(3,4) );
  CODE( A=5 );
  CODE( Tarray::Tcoc AC=A );
  CODE( subarray(A)(1,1)(2,4)=2. );
  CODE( subarray(A)(2,3)(1,2)=1. );
  CODE( subarray(A)(2,3)(3,4)=-7. );
  CODE( dump_array(A) );
  CODE( cout << aff::func::avg(A) << endl );
  CODE( cout << aff::func::rms(A) << endl );
  CODE( cout << aff::func::max(A) << endl );
  CODE( cout << aff::func::min(A) << endl );
  CODE( cout << aff::func::absmax(A) << endl );
  CODE( cout << aff::func::min(AC) << endl );
  CODE( cout << aff::func::sum(A) << endl );
  CODE( cout << aff::func::sqrsum(A) << endl );
  CODE( typedef aff::func::util::Extracthisto<Tarray>::Tmap Tmap );
  CODE( Tmap mymap=aff::func::histo(A) );
  CODE( typedef Tmap::const_iterator Tmapit );
  CODE( for (Tmapit i=mymap.begin(); i!=mymap.end(); ++i) \
        { cout << i->first << "\t" << i->second << endl; } );
  CODE( Tvalmap avalmap );
  CODE( avalmap[-4.]=1 );
  CODE( avalmap[1.]=2 );
  CODE( avalmap[2.]=3 );
  CODE( avalmap[5.]=4 );
  CODE( aff::Array<Tvalmap::mapped_type> B=
          aff::func::valmap<aff::Array>(A, avalmap) );
  CODE( dump_array(B) );
}

/* ----- END OF functionstest.cc ----- */
