/*! \file libdatrwxxinfo.cc
 * \brief present all online help to the user
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/09/2011
 * 
 * present all online help to the user
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
 * REVISIONS and CHANGES 
 *  - 07/09/2011   V1.0   Thomas Forbriger
 *  - 29/11/2011   V1.1   remove dependency to libtfxx
 *  - 24/01/2012   V1.2   increase granularity of receivable texts
 *  - 23/07/2017   V1.3   report library version
 * 
 * ============================================================================
 */
#define LIBDATRWXXINFO_VERSION \
  "LIBDATRWXXINFO   V1.3   present all online help to the user"

#include <iostream>
#include <string>
#include <datrwxx/formats.h>
#include <datrwxx/formatmodifier.h>
#include <datrwxx/aalibdatrwxx.h>

using std::cout;
using std::cerr;
using std::endl;

int main(int iargc, char* argv[])
{
  cout << "This is libdatrwxxinfo" << endl;
  cout << LIBDATRWXXINFO_VERSION << endl;
  cout << datrw::libversion << endl;
  cout << endl;
  if (iargc>1)
  {
    if (argv[1]==std::string("all"))
    {
      datrw::supported_data_types();
      cout << endl;
      datrw::online_help();
    }
    if (argv[1]==std::string("formats"))
    {
      datrw::supported_data_types();
      cout << endl;
      datrw::formatmodifiers::online_help(cout);
    }
    else
    {
      datrw::online_help(argv[1], cout);
    }
  }
  else
  {
    cout << "The program takes one argument to control its output:" << endl;
    cout << endl;
    cout << "libdatrwxxinfo all\n"
      <<    "  print all existing online help texts" << endl;
    cout << endl;
    cout << "libdatrwxxinfo formats\n"
      <<    "  summarize supported formats" << endl;
    cout << endl;
    cout << "libdatrwxxinfo <format>\n"
      <<    "  print online help text for format <format>" << endl;
  }
}

/* ----- END OF libdatrwxxinfo.cc ----- */
