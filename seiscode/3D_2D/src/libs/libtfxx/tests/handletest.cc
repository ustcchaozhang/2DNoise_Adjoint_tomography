/*! \file handletest.cc
 * \brief test handle containers
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/07/2005
 * 
 * test handle containers
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
 *  - 04/07/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define HANDLETEST_VERSION \
  "HANDLETEST   V1.0   test handle containers"

#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/handle.h>
#include <tfxx/range.h>

using std::cout;
using std::cerr;
using std::endl;
 
//! Show code along with output of executed code.
#define CODE(line) cout.width(50); cout << endl << #line << ";  -->  "; line

//! Show code that does not compile.
#define ILLEGAL(line) cout.width(50); cout << #line << \
  ";  !!!  does not compile!" << endl;

typedef tfxx::Range<int> Tirange;

std::ostream& operator << (std::ostream& os, const Tirange& r)
{ os << r.first() << "-" << r.last(); }

std::ostream& operator << (std::ostream& os, 
                           const tfxx::ConstHandle<Tirange>& r)
{ os << r->first() << "-" << r->last(); }

class Any {
  public:
    virtual ~Any() { }
    virtual void print() const { cout << "ERROR: called base!" << endl; }
}; // class Any

class Int: public Any {
  public:
    Int(const int& v): Mv(v) { }
    void print() const { cout << Mv << endl; }
  private:
    int Mv;
}; // class Int

class String: public Any {
  public:
    String(const std::string& v): Mv(v) { }
    void print() const { cout << Mv << endl; }
  private:
    std::string Mv;
}; // class Int

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    HANDLETEST_VERSION "\n"
    "usage: handletest" "\n"
    "   or: handletest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    HANDLETEST_CVSID
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

  CODE( Tirange r1(4,8) );
  CODE( cout << r1 << endl );
  CODE( typedef tfxx::Handle<Tirange> Tirhandle );
  CODE( Tirhandle h1(r1) );
  CODE( cout << *h1 << endl );
  CODE( Tirhandle h2(h1) );
  CODE( cout << *h2 << endl );
  CODE( *h1=Tirange(12,34) );
  CODE( cout << *h2 << endl );
  CODE( h2->shift(-12) );
  CODE( cout << *h1 << endl );
  CODE( Tirhandle::Tcoc ch1(h1) );
  CODE( cout << *ch1 << endl );
  CODE( h2->shift(10) );
  CODE( cout << *ch1 << endl );
  ILLEGAL( ch1->shift(10) );
  CODE( cout << *h1 << ", " << h1 << endl );
  CODE( cout << *ch1 << ", " << ch1 << endl );

  CODE( String s1("Hi there") );
  CODE( s1.print() );
  CODE( Int i1(13) );
  CODE( i1.print() );
  CODE( typedef tfxx::Handle<Any> Tanyhandle );
  CODE( Tanyhandle ah1(s1) );
  CODE( ah1->print() );
  CODE( Tanyhandle ah2(new String("ein string")) );
  CODE( ah2->print() );
  CODE( Tanyhandle ah3(ah2) );
  CODE( ah3->print() );
  CODE( Tanyhandle ah4(new Int(4321)) );
  CODE( ah4->print() );
  CODE( ah3=ah4 );
  CODE( ah3->print() );
}

/* ----- END OF handletest.cc ----- */
