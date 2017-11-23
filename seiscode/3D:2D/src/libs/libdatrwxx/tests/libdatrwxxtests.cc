/*! \file libdatrwxxtests.cc
 * \brief test internal features of the library
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/09/2011
 * 
 * test internal features of the library
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
 *  - 06/09/2011   V1.0   Thomas Forbriger
 *  - 11/07/2016   V1.1   provide means to test error message output
 * 
 * ============================================================================
 * \example libdatrwxxtests.cc 
 * \brief This program tests some internal features of libdatrwxx and can
 * serve as an example for using features like datrw::Subformat
 */
#define LIBDATRWXXTESTS_VERSION \
  "LIBDATRWXXTESTS   V1.1   test internal features of the library"

#include <iostream>
#include <fstream>
#include <string>
#include <tfxx/commandline.h>
#include <datrwxx/debug.h>
#include <datrwxx/error.h>
#include <datrwxx/util.h>
#include <datrwxx/types.h>
#include <datrwxx/readany.h>
#include <datrwxx/formatmodifier.h>
#include <aff/dump.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool testformat, readint, readsingle, skipsamples, readtest;
  bool testerror;
  bool debug, verbose;
  std::string formatstring, readtype;
  int nerrortest; // select an error function to be tested
}; // struct Options

/*----------------------------------------------------------------------*/

void reportkey(const datrw::Subformat& f, 
               const std::string k,
               const std::string d)
{
  cout << "  " << k << " is ";
  if (!f.isset(k))
  {
    cout << "NOT ";
  }
  cout << "set";
  cout << " and has value " << f.value(k, d) << endl;
  double a,b;
  f(k, d) >> a >> b;
  cout << "    reading two doubles from value: " << a << ", " << b;
  cout << endl;
  cout << "    the given default value is " << d << endl;
} // void reportkey(const datrw::Subformat& f, const std::string k)

/*----------------------------------------------------------------------*/

void reportprovides(const std::string& type, const bool& flag)
{
  cout << "input stream ";
  if (flag)
  { cout << "provides"; }
  else
  { cout << "doesn't provide"; }
  cout << " " << type << " type data" << endl;
} // void reportprovides(const std::string& type, const bool& flag)

/*----------------------------------------------------------------------*/

void checkkey(const datrw::Subformat& f, const std::string k)
{
  cout << "  " << k << " is ";
  if (!f.isset(k))
  {
    cout << "NOT ";
  }
  cout << "set" << endl;
} // void checkkey(const datrw::Subformat& f, const std::string k)

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    LIBDATRWXXTESTS_VERSION "\n"
    "usage: libdatrwxxtests [-format s] [-rtest type] [-int] [-single]\n"
    "                       [-error[=n]]" "\n"
    "                       [-v] [-skip] [-DEBUG] file [file ...]" "\n"
    "   or: libdatrwxxtests --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v           verbose mode\n"
    "-DEBUG       activate debugging output\n"
    "-format s    test datrwxx::Subformat by passing format string s\n"
    "-rtest type  read files of type \"type\" and report content\n"
    "-int         use integer reading\n"
    "-single      use float reading\n"
    "-skip        skip traces\n"
    "-error[=n]   test error report mechanism (n=0: prints available tests)\n"
    "file         file to be read\n"
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
    {"format",arg_yes,"-"},
    // 3: perform a read test
    {"rtest",arg_yes,"-"},
    // 4: use integer samples
    {"int",arg_no,"-"},
    // 5: use single precision samples
    {"single",arg_no,"-"},
    // 6: skip samples
    {"skip",arg_no,"-"},
    // 7: debug output
    {"DEBUG",arg_no,"-"},
    // 8: skip samples
    {"error",arg_opt,"0"},
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
  opt.verbose=cmdline.optset(1);
  opt.testformat=cmdline.optset(2);
  opt.formatstring=cmdline.string_arg(2);
  opt.readtest=cmdline.optset(3);
  opt.readtype=cmdline.string_arg(3);
  opt.readint=cmdline.optset(4);
  opt.readsingle=cmdline.optset(5);
  opt.skipsamples=cmdline.optset(6);
  opt.debug=cmdline.optset(7);
  opt.testerror=cmdline.optset(8);
  opt.nerrortest=cmdline.int_arg(8);

  /*======================================================================*/

  if (opt.testformat)
  {
    cout << "Test format modifiers" << endl;
    cout << "=====================" << endl;
    std::string format=datrw::util::clipstring(opt.formatstring);
    datrw::Subformat subformat(opt.formatstring);
    cout << "data format ID: " << format << endl;
    reportkey(subformat, "key1", "default1");
    reportkey(subformat, "key2", "18.,23.6");
    checkkey(subformat, "key3");
    checkkey(subformat, "key4");
    if (subformat.allarechecked())
    {
      cout << "All keys have been checked by the program." << endl;
    }
    else
    {
      DATRW_assert_modifiers_are_recognized(subformat, "libdatrwxxtests");
    }
    cout << endl;
  } // if (opt.testformat)

  /*======================================================================*/

  if (opt.readtest)
  {
    sff::FREE free;
    sff::INFO info;
    sff::SRCE srce;
    sff::WID2 wid2;
    cout << "Read input files of type: " << opt.readtype << endl;
    cout << "=========================" << endl;
    while (cmdline.extra())
    {
      std::string filename=cmdline.next();
      cout << endl;
      cout << "read file " << filename << endl;

      std::ifstream ifs(filename.c_str());
      datrw::ianystream is(ifs, opt.readtype, opt.debug);
      if (opt.verbose)
      {
        reportprovides("double", is.providesd());
        reportprovides("float", is.providesf());
        reportprovides("int", is.providesi());
      }
      if (is.hasfree())
      {
        if (opt.verbose)
        {
          cout << "file FREE block:" << endl;
          is >> free;
          ::sff::verbose(std::cout, free);
        }
        else
        {
          cout << "file has FREE block" << endl;
        }
      }
      else
      {
        cout << "file has no FREE block" << endl;
      }
      if (is.hassrce())
      {
        if (opt.verbose)
        {
          cout << "file SRCE line:" << endl;
          is >> srce;
          ::sff::verbose(std::cout, srce);
        }
        else
        {
          cout << "file has SRCE line" << endl;
        }
      }
      else
      {
        cout << "file has no SRCE line" << endl;
      }

      while (!is.last())
      {
        datrw::Tdseries dseries;
        datrw::Tfseries fseries;
        datrw::Tiseries iseries;

        if (opt.skipsamples)
        {
          is.skipseries();
        }
        else
        {
          if (opt.readint)
          {
            if (is.providesi())
            {
              is >> iseries;
            }
            else
            {
              cout << "stream does not provide integer data" << endl;
              is.skipseries();
            }
          }
          else if (opt.readsingle)
          {
            if (is.providesf())
            {
              is >> fseries;
            }
            else
            {
              cout << "stream does not provide single data" << endl;
              is.skipseries();
            }
          }
          else
          {
            if (is.providesd())
            {
              is >> dseries;
            }
            else
            {
              cout << "stream does not provide double data" << endl;
              is.skipseries();
            }
          }
        }
        if (is.hasfree())
        {
          if (opt.verbose)
          {
            cout << "trace FREE block:" << endl;
            is >> free;
            ::sff::verbose(std::cout, free);
          }
          else
          {
            cout << "trace has FREE block" << endl;
          }
        }
        else
        {
          cout << "trace has no FREE block" << endl;
        }
        if (is.hasinfo())
        {
          if (opt.verbose)
          {
            cout << "trace INFO line:" << endl;
            is >> info;
            ::sff::verbose(std::cout, info);
          }
          else
          {
            cout << "trace has INFO line" << endl;
          }
        }
        else
        {
          cout << "trace has no INFO line" << endl;
        }
        is >> wid2;
        ::sff::verbose(std::cout, wid2);
        if (opt.readint)
        {
          if (is.providesi())
          {
            if (opt.verbose)
            {
              DUMP( iseries );
            }
            else
            {
              cout << "read " << iseries.size() << " integer samples" << endl;
            }
          }
          else
          {
            cout << "stream does not provide integer data" << endl;
          }
        }
        else if (opt.readsingle)
        {
          if (is.providesf())
          {
            if (opt.verbose)
            {
              DUMP( fseries );
            }
            else
            {
              cout << "read " << fseries.size() 
                << " single precision samples" << endl;
            }
          }
          else
          {
            cout << "stream does not provide single precision data" << endl;
          }
        }
        else
        {
          if (is.providesd())
          {
            if (opt.verbose)
            {
              DUMP( dseries );
            }
            else
            {
              cout << "read " << dseries.size() 
                << " double precision samples" << endl;
            }
          }
          else
          {
            cout << "stream does not provide double precision data" << endl;
          }
        }
        if (is.last())
        {
          cout << "This was the last trace in file!" << endl;
        }
      } // while (!is.last())
    } // while (cmdline.extra())
  } // if (opt.readtest)

  /*======================================================================*/

  if (opt.testerror)
  {
    cout << "Test error reports (formatting of messages)" << endl;
    cout << "===========================================" << endl;
    cout << endl;
    if (opt.nerrortest == 0)
    {
      cout << "available tests:" << endl;
      cout << "1: non-fatal reports" << endl;
      cout << "2: direct abort" << endl;
      cout << "3: fatal/non-fatal assert (non-fatal version)" << endl;
      cout << "4: fatal/non-fatal assert (fatal version)" << endl;
      cout << "5: standard assertion" << endl;
    }
    else if (opt.nerrortest == 1)
    {
      cout << "report assert" << endl;
      cout << "-------------" << endl;
      DATRW_report_assert(false, "This message is produced after testing"
                          " a condition to report\n"
                          "that the test fails.");
      cout << "\nwarning" << endl;
      cout <<   "-------" << endl;
      DATRW_warning("main", "This is a warning message from a specific"
                    " function in the library.\n"
                    "The macro takes a multiline message output too.");
    }
    else if (opt.nerrortest == 2)
    {
      cout << "direct abort" << endl;
      cout << "------------" << endl;
      DATRW_abort("Abort program intentionally.\n"
                  "A multi-line message my be passed too in this case.\n"
                  "Pass one of the non-specified values for the error\n"
                  "test to see the output for DATRW_illegal.");
    }
    else if ((opt.nerrortest == 3) || (opt.nerrortest == 4))
    {
      cout << "fatal/non-fatal assert" << endl;
      cout << "----------------------" << endl;
      DATRW_nonfatal_assert(opt.nerrortest==3,
                            (opt.nerrortest<=1)||(opt.nerrortest>=6),
                            "This is a test of a fatal/non-fatal assert.\n"
                            "The error is made non-fatal, if the option"
                            "opt.nerrortest equals 3.\n"
                            "Currently " << DATRW_value(opt.nerrortest));
    }
    else if (opt.nerrortest == 5)
    {
      cout << "standard assertion" << endl;
      cout << "------------------" << endl;
      DATRW_assert(false, 
                   "This type of error is emitted,"
                   " if an assertion is failed.\n"
                   "Several lines of explanation can be provided"
                   " to the user.\n"
                   "This can include the output of variables like "
                   << DATRW_value(opt.nerrortest));
    }
    else
    {
      cout << "no test selected by n=" << opt.nerrortest << endl;
      DATRW_illegal;
    }
    cout << "...program did not abort" << endl;
  } // if (opt.testerror)
}

/* ----- END OF libdatrwxxtests.cc ----- */
