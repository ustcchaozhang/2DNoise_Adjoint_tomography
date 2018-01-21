/*! \file filetest.cc
 * \brief test file access functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/12/2008
 * 
 * test file access functions
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 04/12/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define FILETEST_VERSION \
  "FILETEST   V1.0   test file access functions"

#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/filestatus.h>

using std::cout;
using std::cerr;
using std::endl;

void applytest(const bool& f, const char* a, const char* b)
{
  cout << "The file " << a << " ";
  if (!f) { cout << "NOT "; }
  cout << b << endl;
}

void testfile(const std::string& s)
{
  cout << endl;
  cout << "testing file " << s << endl;
  applytest(tfxx::file::exists(s.c_str()), "does", "exist");
  applytest(tfxx::file::regular(s.c_str()), "is", "regular");
  applytest(tfxx::file::readable(s.c_str()), "is", "readable");
  applytest(tfxx::file::writable(s.c_str()), "is", "writable");
  applytest(tfxx::file::creatable(s.c_str()), "is", "creatable");
  cout << "a unique new filename would be " 
    << tfxx::file::uniquenew(s.c_str()) << endl;
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FILETEST_VERSION "\n"
    "usage: filetest" "\n"
    "   or: filetest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    FILETEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
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
  */

  while (cmdline.extra()) { testfile(cmdline.next()); }
}

/* ----- END OF filetest.cc ----- */
