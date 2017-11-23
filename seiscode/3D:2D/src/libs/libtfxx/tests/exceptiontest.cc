/*! \file exceptiontest.cc
 * \brief test exception classes
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/12/2012
 * 
 * test exception classes
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of libtfxx.
 *
 * libtfxx is free software; you can redistribute it and/or modify
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
 *  - 17/12/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define EXCEPTIONTEST_VERSION \
  "EXCEPTIONTEST   V1.0   test exception classes"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/fs.h>

using std::cout;
using std::cerr;
using std::endl;

/*======================================================================*/

void throwb()
{
        throw tfxx::error::FSException("simulate fs error", 
                                    __FILE__, __LINE__, 5);
}

void throwa()
{
  TFXX_abort("abort anyway");
}

class A {
  public:
    A(const bool& throwbase): Mtb(throwbase) { }
    void setflag(const bool& tb) { Mtb=tb; }
    void throwit() const
    {
      if (Mtb)
      {
        throwa();
      }
      else
      {
        throwb();
      }
    }
  private:
    bool Mtb;
};

struct Options {
  bool throwbase, noresume;
  bool reportonconstruct;
}; 

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    EXCEPTIONTEST_VERSION "\n"
    "usage: exceptiontest [-b] [-nr] [-roc]" "\n"
    "   or: exceptiontest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    EXCEPTIONTEST_CVSID
    "\n"
    "-b     throw base class\n"
    "-nr    do not resume\n"
    "-roc   report on construct\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: throw base class
    {"b",arg_no,"-"},
    // 3: no resume
    {"nr",arg_no,"-"},
    // 4: no resume
    {"roc",arg_no,"-"},
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

  Options opt;
  opt.throwbase=cmdline.optset(2);
  opt.noresume=cmdline.optset(3);
  opt.reportonconstruct=cmdline.optset(4);

  /*
  // dummy operation: print option settings
  for (int iopt=0; iopt<2; iopt++)
  {
    cout << "option: '" << options[iopt].opt_string << "'" << endl;
    if (cmdline.optset(iopt)) {  cout << "  option was set"; }
    else { cout << "option was not set"; }
    cout << endl;
    cout << "  argument (string): '" << cmdline.string_arg(iopt) << "'" << endl;
    cout << "     argument (int): '" << cmdline.int_arg(iopt) << "'" << endl;
    cout << "    argument (long): '" << cmdline.long_arg(iopt) << "'" << endl;
    cout << "   argument (float): '" << cmdline.float_arg(iopt) << "'" << endl;
    cout << "  argument (double): '" << cmdline.double_arg(iopt) << "'" << endl;
    cout << "    argument (bool): '";
    if (cmdline.bool_arg(iopt))
    { cout << "true"; } else { cout << "false"; }
    cout << "'" << endl;
  }
  while (cmdline.extra()) { cout << cmdline.next() << endl; }

  // dummy operation: print rest of command line
  while (cmdline.extra()) { cout << cmdline.next() << endl; }
  */

  A a(opt.throwbase);

  if (opt.reportonconstruct)
  {
    tfxx::error::Exception::report_on_construct();
  }
  else
  {
    tfxx::error::Exception::dont_report_on_construct();
  }

  try {
    a.throwit();
  } catch (tfxx::error::Exception& e) {
    cout << "caught exception" << endl;
    e.report();
    if (opt.noresume)
    {
      throw(e);
    }
  }

  cout << "again" << endl;
  tfxx::error::Exception::restore_report_state();

  try {
    a.throwit();
  } catch (tfxx::error::Exception& e) {
    cout << "caught exception" << endl;
    e.report();
    if (opt.noresume)
    {
      throw(e);
    }
  }

  cout << "survived" << endl;
}

/* ----- END OF exceptiontest.cc ----- */
