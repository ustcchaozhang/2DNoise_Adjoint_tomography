/*! \file tester.cc
 * \brief a small program to test some functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/03/2014
 * 
 * a small program to test some functions
 * 
 * Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 28/11/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TESTER_VERSION \
  "TESTER   V2014-03-24   a small program to test some functions"
#define TESTER_CVSID \
  "$Id$"

#include <iostream>
#include <list>
#include <tfxx/commandline.h>
#include <tfxx/stringfunc.h>
#include <tfxx/filestatus.h>
#include "thiesdl1.h"
#include "functions.h"
#include "memory.h"

typedef std::list<std::string> Tlistofstring;

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

void checkresult(const std::string& path,
                 const std::string& test,
                 const bool& result)
{
  cout << path << " is ";
  if (!result) { cout << "NOT "; }
  cout << test << endl;
}

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TESTER_VERSION "\n"
    "usage: tester" "\n"
    "   or: tester --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    TESTER_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: test pattern
    {"pattern",arg_yes,"-"},
    // 3: report source code ids
    {"Id",arg_no,"-"},
    // 4: check data pattern
    {"checkdatepattern",arg_yes,"-"},
    // 5: check if file exists
    {"checkfileexists",arg_yes,"-"},
    // 6: prepare file path
    {"preppath",arg_yes,"-"},
    // 7: set active flag
    {"active",arg_no,"-"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    cerr << "the program expects at least one argument to continue" << endl;
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

  /*
  std::ostringstream oss;
  oss << "This is my first item";
  oss << dl1::DL1::EOL;
  oss << "This is my second item" << dl1::DL1::EOL;
  oss << "and here are some numbers: " << 3 << 6.5 << 1 << dl1::DL1::EOL;

  std::string eol(dl1::DL1::EOL);
  std::string msg(oss.str());
  Tlistofstring v=tfxx::string::split<std::list>(msg, eol, true);
  for (Tvecofstring::iterator i=v.begin(); i!=v.end(); ++i)
  {
    cout << *i << endl;
  }
  */

  // cout << dl1::dl1samplinginterval.timestring() << endl;

  /*
  if (cmdline.extra())
  {
    std::string filename=cmdline.next();
    cout << "file " << filename << endl;
    dl1::Memory mem(filename);
    mem.setdefault("key","Wert");
    mem.setdefault("schluessel","Wert und wert und wert");
    mem.setdefault("datum","2008/12/05");
  }
  */

  /*======================================================================*/

  if (cmdline.optset(3))
  {
    cout << "source code version" << endl;
    int i=0;
    while (dl1::CVSIDS[i]!=0)
    {
      cout << dl1::CVSIDS[i] << endl;
      ++i;
    }
  }

  /*======================================================================*/

  if (cmdline.optset(2))
  {
    std::string pattern=cmdline.string_arg(2);
    cout << "pattern with templates to be replaced: ";
    cout << pattern << endl;
    pattern=dl1::patsubstdate(pattern, libtime::utc());
    cout << "result: " << pattern << endl;
  }

  /*======================================================================*/

  if (cmdline.optset(4))
  {
    std::string pattern=cmdline.string_arg(4);
    cout << "pattern with templates: ";
    cout << pattern << endl;
    cout << "pattern ";
    if (dl1::datetemplatespresent(pattern))
    {
      cout << "contains";
    }
    else
    {
      cout << "does not contain";
    }
    cout << " all required date templates." << endl;
  }

  /*======================================================================*/

  if (cmdline.optset(5))
  {
    std::string pattern=cmdline.string_arg(5);
    checkresult(pattern, "existing", tfxx::file::exists(pattern.c_str()));
    checkresult(pattern, "creatable", tfxx::file::creatable(pattern.c_str()));
    checkresult(pattern, "writable", tfxx::file::writable(pattern.c_str()));
    checkresult(pattern, "readable", tfxx::file::readable(pattern.c_str()));
  }

  /*======================================================================*/

  if (cmdline.optset(6))
  {
    std::string pattern=cmdline.string_arg(6);
    cout << "file path pattern: " << pattern << endl;
    std::string pathname=dl1::mkpathname(pattern, libtime::utc(), "test", 
                                         cmdline.optset(7));
    cout << "file path: " << pathname << endl;
  }
}

/* ----- END OF tester.cc ----- */
