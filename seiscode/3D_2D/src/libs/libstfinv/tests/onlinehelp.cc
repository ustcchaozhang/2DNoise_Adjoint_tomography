/*! \file onlinehelp.cc
 * \brief print online help
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 09/05/2011
 * 
 * print online help
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 09/05/2011   V1.0   Thomas Forbriger
 *  - 14/10/2015   V1.1   address new end-user usage functions
 * 
 * ============================================================================
 */
#define ONLINEHELP_VERSION \
  "ONLINEHELP   V1.1   print online help"

#include <iostream>
#include <tfxx/commandline.h>
#include <stfinv/stfinvany.h>

using std::cout;
using std::cerr;
using std::endl;

bool selectprocedure;
std::string procedure_id;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ONLINEHELP_VERSION "\n"
    "usage: onlinehelp [-procedure p] [usage]" "\n"
    "   or: onlinehelp --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "usage        any non-empty parameter lets the program output\n"
    "             the usage summary of libstfinv\n"
    "-procedure p prints the detailed description of procedure \"p\"\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: verbose mode
    {"procedure",arg_yes,"-"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    stfinv::engines(cout);
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl << endl;
    cerr << help_text << endl;
    stfinv::engines(cout);
    exit(0);
  }

  selectprocedure=cmdline.optset(2);
  procedure_id=cmdline.string_arg(2);

  if (selectprocedure)
  {
    stfinv::usage(procedure_id, cout);
  }
  else
  {
    stfinv::help(cout);
  }
}

/* ----- END OF onlinehelp.cc ----- */
