/*! \file rangetest.cc
 * \brief test range code
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/04/2009
 * 
 * test range code
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 27/04/2009   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define RANGETEST_VERSION \
  "RANGETEST   V1.0   test range code"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/range.h>
#include <tfxx/rangelist.h>
#include <tfxx/rangestring.h>

using std::cout;
using std::cerr;
using std::endl;
 
//! Show code along with output of executed code.
#define CODE(line) cout.width(50); cout << endl << #line << ";  -->  "; line

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    RANGETEST_VERSION "\n"
    "usage: rangetest [-v] [-step s] [-list l]" "\n"
    "   or: rangetest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    RANGETEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: step size
    {"step",arg_yes,"1"},
    // 3: step size
    {"list",arg_yes,"1-10"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<1) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  int step=cmdline.int_arg(2);
  std::string list=cmdline.string_arg(3);

  CODE( tfxx::Range<int> Tirange(5,12) );
  CODE( tfxx::RangeStepper<int> RS(Tirange) );
  CODE( while (RS.more()) { cout << ++RS << ", "; });
  cout << endl;

  CODE( tfxx::Range<int> R1(5,12) );
  CODE( tfxx::Range<int> R2(50) );
  CODE( tfxx::Range<int> R3(18,29) );
  CODE( tfxx::RangeList<int> RL );
  CODE( RL.append(R1) );
  CODE( RL.append(R2) );
  CODE( RL.append(R3) );
  CODE( tfxx::RangeListStepper<int> RLS(RL, 2) );
  CODE( while (RLS.valid()) { cout << RLS << ", "; ++RLS; });
  cout << endl;

  CODE( cout << step << endl );
  CODE( cout << list << endl );
  // tfxx::RangeListStepper<int> RLSsa(tfxx::string::rangelist<int>(list), step);
  CODE( tfxx::RangeListStepper<int> RLSs(tfxx::string::rangelist<int>(list), step) );
  CODE( while (RLSs.valid()) { cout << RLSs << ", "; ++RLSs; });
  cout << endl;
}

/* ----- END OF rangetest.cc ----- */
