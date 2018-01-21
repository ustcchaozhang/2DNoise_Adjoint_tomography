/*! \file pgtestxx.cc
 * \brief test functions within the PGPLOT++ library
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/12/2008
 * 
 * test functions within the PGPLOT++ library
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
 * REVISIONS and CHANGES 
 *  - 28/12/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define PGTESTXX_VERSION \
  "PGTESTXX   V1.0   test functions within the PGPLOT++ library"

#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <pgplotxx/pghandle.h>
#include <pgplotxx/pgplotxx.h>

using std::cout;
using std::cerr;
using std::endl;

/*! \example pgtestxx.cc
 *
 * This file provides test functions for modules in the pgplot
 * library.
 */

/*======================================================================*/

void printtitle(const std::string& s, const char& c)
{
  cout << endl;
  cout << s << endl;
  for (int i=0; i<s.length(); ++i) { cout << c; }
  cout << endl;
}

void section(const std::string& s) { printtitle(s, '='); }
void subsection(const std::string& s) { printtitle(s, '-'); }

#define CODE( line ) \
  cout.width(40); \
  cout.setf(std::ios_base::left, std::ios_base::adjustfield); \
  cout << endl << std::string(#line)+std::string(";") << ": "; line

#define ILLEGAL( line ) \
  cout << "ILLEGAL (does not compile): " << #line << ";" << endl;

/*======================================================================*/
// code to test handle class template

//! \brief Base class for testing handles.
class Handletestbaseclass {
  public:
    Handletestbaseclass(const int& i): Mval(i) { }
    virtual ~Handletestbaseclass()
    { cout << "destructor of Handletestbaseclass: "; this->print(cout); }
    virtual void print(std::ostream& os) const
    { cout << "Handletestbaseclass(" << Mval << ")" << endl; }
    int basevalue() const { return Mval; }
    int& basevalue() { return Mval; }
  private:
    int Mval; 
}; // class Handletestbaseclass

//! \brief Test class for handles.
class Handletestclass: public Handletestbaseclass {
  public:
    typedef Handletestbaseclass Tbase;
    Handletestclass(const int& i, const int& j): Tbase(i), Mval(j) { }
    virtual ~Handletestclass()
    { cout << "destructor of Handletestclass: "; this->print(cout); }
    virtual void print(std::ostream& os) const
    {
      cout << "Handletestclass(" << this->basevalue() << ","
      << Mval << ")" << endl; 
    }
    int value() const { return Mval; }
    int& value() { return Mval; }
  private:
    int Mval; 
}; // class Handletestclass

//! \brief Test function for inheritance transparency.
void testhandlefunction(const pgplot::Handle<Handletestbaseclass>::Tcoc& h)
{
  subsection("in function that accepts an instance of Handle<Handletestbaseclass>::Tcoc");
  CODE( h->print(cout) );
  ILLEGAL( h->basevalue()=60; )
}

//! \brief Test handles.
void testhandle()
{
  section("test handles");
  subsection("create objects");
  CODE( Handletestbaseclass B1(1) );
  CODE( Handletestclass C1(2,3) );
  subsection("create handle from object");
  CODE( pgplot::Handle<Handletestclass> H1(C1) );
  CODE( H1->value()=8 );
  CODE( H1->print(cout) );
  subsection("create handle to base from handle");
  CODE( pgplot::Handle<Handletestbaseclass> H2(H1) );
  CODE( H1->basevalue()=9 );
  CODE( H1->print(cout) );
  CODE( H2->print(cout) );
  CODE( H1->value()=-20 );
  CODE( H2->basevalue()=-5 );
  CODE( H2->print(cout) );
  ILLEGAL( H2->value()=18 );
  subsection("create handles directly");
  CODE( pgplot::Handle<Handletestclass> H3(Handletestclass(11,13)) );
  CODE( pgplot::Handle<Handletestbaseclass> H4(Handletestclass(12,14)) );
  subsection("create handle to constant base");
  CODE( pgplot::Handle<Handletestbaseclass>::Tcoc H5(H3) );
  CODE( H5->print(cout) );
  ILLEGAL( H5->basevalue()=-8 );
  cout << endl;
  CODE( testhandlefunction(H3) );
  subsection("assignement using inheritance transparency");
  CODE( H4->print(cout) );
  CODE( H4=H3 );
  CODE( H4->print(cout) );
  CODE( H4->basevalue()=100 );
  CODE( H3->print(cout) );
  CODE( pgplot::Handle<Handletestbaseclass>::Tcoc H6(Handletestbaseclass(23)));
  CODE( H6->print(cout) );
  CODE( H6=H3 );
  CODE( H6->print(cout) );
  ILLEGAL( H6->basevalue()=64 );
  subsection("finished");
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    PGTESTXX_VERSION "\n"
    "usage: pgtestxx [-all] [-handle]" "\n"
    "   or: pgtestxx --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-all         run all tests" "\n"
    "-handle      test Handle class template" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: test handle
    {"handle",arg_no,"-"},
    // 3: run all tests
    {"all",arg_no,"-"},
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
    cerr << pgplot::usage_escape_sequences;
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

  cout << PGTESTXX_VERSION << endl;
  if (cmdline.optset(2) || cmdline.optset(3)) { testhandle(); }
}

/* ----- END OF pgtestxx.cc ----- */
