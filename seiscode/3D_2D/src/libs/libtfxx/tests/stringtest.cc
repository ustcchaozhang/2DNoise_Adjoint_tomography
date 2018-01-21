/*! \file stringtest.cc
 * \brief test string functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * test string functions
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/06/2005   V1.0   Thomas Forbriger
 *  - 11/11/2009   V1.1   test split function
 * 
 * ============================================================================
 */
#define STRINGTEST_VERSION \
  "STRINGTEST   V1.1   test string functions"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/stringfunc.h>
#include <tfxx/rangestring.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool rangetest, patsubsttest, splittest;
  std::string rangestring;
}; // struct Options

/*======================================================================*/

template<class T>
std::ostream& operator<< (std::ostream& os, const tfxx::Range<T>& r)
{
  os << r.first() << " - " << r.last();
  return os;
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    STRINGTEST_VERSION "\n"
    "usage: stringtest [-r r] [-p] [-s]" "\n"
    "   or: stringtest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    STRINGTEST_CVSID
    "-r r         read ranges from \'r\'" "\n"
    "-p           test pattern substitution" "\n"
    "-s           split functions" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: test range reading
    {"r",arg_yes,"-"},
    // 3: pattern substitution
    {"p",arg_no,"-"},
    // 4: string split function
    {"s",arg_no,"-"},
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

  Options opt;
  opt.rangetest=cmdline.optset(2);
  opt.rangestring=cmdline.string_arg(2);
  opt.patsubsttest=cmdline.optset(3);
  opt.splittest=cmdline.optset(4);

  /*----------------------------------------------------------------------*/
  if (opt.splittest)
  {
    std::string a("a:bc:def:ghij");
    std::list<std::string> splitted;
    tfxx::string::gen_split(splitted, a, ":", true);
    cout << a << " is splitted to:" << endl;
    for (std::list<std::string>::const_iterator I=splitted.begin();
         I!=splitted.end(); ++I)
    {
      cout << *I << endl;
    }
  }

  /*----------------------------------------------------------------------*/
  if (opt.rangetest)
  {
    typedef tfxx::RangeList<int> Trangelist;
    Trangelist
      rnglist=tfxx::string::rangelist<Trangelist::Tvalue>(opt.rangestring);
    Trangelist::Tlist rlist=rnglist.list();
    Trangelist::Tlist::const_iterator i=rlist.begin();
    while (i!=rlist.end())
    {
      std::cout << *i << std::endl;
      ++i;
    }
    std::cout << "values contained in all ranges:" << std::endl;
    for (int i=1; i<=100; ++i)
    {
      if (rnglist.contains(i)) { std::cout << i << " "; }
    }
    std::cout << std::endl;
  }
  
  /*----------------------------------------------------------------------*/

  // pattern substitution test
  if (opt.patsubsttest)
  {
    {
      std::string s=
        "This is a string that will be modified by string functions";
      std::string p="string";
      std::string r="nonsense";
      std::cout << s << std::endl;
      std::cout << tfxx::string::patsubst(s,p,r) << std::endl;
    }
    {
      std::string s=
        "% <- front     middle -> %    and end -> %";
      std::string p="%";
      std::string r="****";
      std::cout << s << std::endl;
      std::cout << tfxx::string::patsubst(s,p,r) << std::endl;
    }
  } // pattern substitution test
}

/* ----- END OF stringtest.cc ----- */
