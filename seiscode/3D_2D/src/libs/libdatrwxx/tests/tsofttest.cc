/*! \file tsofttest.cc
 * \brief test tsoft reading
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/09/2009
 * 
 * test tsoft reading
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
 *  - 17/09/2009   V1.0   Thomas Forbriger (thof)
 *  - 07/01/2013   V1.1   hunting a bug when reading single trace data
 *  - 19/02/2104 thof:    comment iunits and count since they are only set but
 *                        never used
 * 
 * ============================================================================
 */
#define TSOFTTEST_VERSION \
  "TSOFTTEST   V1.1   test tsoft reading"

#include <iostream>
#include <fstream>
#include <sffxx.h>
#include <tfxx/commandline.h>
#include <datrwxx/tsoftdata.h>
#include <datrwxx/tsoftreader.h>
#include <datrwxx/channeltranslation.h>
#include <datrwxx/tsoft.h>
#include <datrwxx/error.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, test1, test2, test3, debug, ttest, stest;
}; // struct Options

/*----------------------------------------------------------------------*/

std::string passtrimws(std::string line)
{
  datrw::tsoft::trimws(line);
  return(line);
}

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TSOFTTEST_VERSION "\n"
    "usage: tsofttest file [-t1] [-t2] [-t3] [-t4] [-t5] [-v] [-D]" "\n"
    "   or: tsofttest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "file   TSOFT file" "\n"
    "-v     be verbose" "\n"
    "-D     debug mode" "\n"
    "-t1    test individual components" "\n"
    "-t2    test trimws" "\n"
    "-t3    test class tsoftfile" "\n"
    "-t4    test translation table" "\n"
    "-t5    test itsoftstream" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: test basic classes
    {"t1",arg_no,"-"},
    // 3: test trimws 
    {"t2",arg_no,"-"},
    // 4: test class tsoftfile
    {"t3",arg_no,"-"},
    // 5: debug mode
    {"D",arg_no,"-"},
    // 6: test translation table
    {"t4",arg_no,"-"},
    // 7: test translation table
    {"t5",arg_no,"-"},
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
  opt.test1=cmdline.optset(2);
  opt.test2=cmdline.optset(3);
  opt.test3=cmdline.optset(4);
  opt.debug=cmdline.optset(5);
  opt.ttest=cmdline.optset(6);
  opt.stest=cmdline.optset(7);

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

  TFXX_assert(cmdline.extra(), "missing file name");
  std::string filename=cmdline.next();
  if (opt.verbose)
  { cout << "test TSOFT reading on file " << filename << endl; }

   
  /*======================================================================*/
  // test 1
  if (opt.test1) 
  {
    if (opt.verbose)
    { cout << "test 1: test individual components" << endl; }
    std::ifstream is(filename.c_str());
    std::string line;
    bool indata=false;
    bool inchannels=false;
    // bool inunits=false;
    // int count=0;
    while (is.good())
    {
      line=datrw::tsoft::getDOSline(is);
      if (is.good())
      {
        datrw::tsoft::Line theline(line);
        cout << "<" << theline.theline() << ">" << endl;
        if (theline.hastag())
        {
          indata=false;
          inchannels=false;
          // inunits=false;
          // count=0;
          cout << "tag: " << theline.thetag() << "; ";
          if (theline.hascontent())
          {
            cout << "content: " << theline.thecontent() << endl;
          }
          else
          {
            cout << "no content" << endl;
          }
          if (theline.thetag() == datrw::tsoft::tagdata) { indata=true; }
          if (theline.thetag() == datrw::tsoft::tagchannels) 
          { inchannels=true; }
          // if (theline.thetag() == datrw::tsoft::tagunits) { inunits=true; }
        }
        else if (indata)
        {
          cout << line << endl;
          datrw::tsoft::Dataline theline(line);
          cout << theline.time().timestring() << endl;
          cout << "  ";
          for (int i=0;i<theline.nsamples();++i)
          {
            cout << theline.sample(i) << "; ";
          }
          cout << endl;
        }
        else if (inchannels)
        {
          datrw::tsoft::Channelinfo ci;
          ci.setchannelinfo(line);
          cout << ci.thelocation() << " * "
            << ci.theinstrument() << " * "
            << ci.thedatatype() << endl;
        }
      }
    }
  }
  // end of test 1
   
  /*======================================================================*/
  // test 2
  if (opt.test2) 
  {
    if (opt.verbose)
    { cout << "test 2: test trimws" << endl; }
    cout << "++++" << passtrimws("  hdj wed ") << "++++" << endl;
    cout << "++++" << passtrimws("  hdj wed") << "++++" << endl;
    cout << "++++" << passtrimws("hdj wed   ") << "++++" << endl;
    cout << "++++" << passtrimws("   ") << "++++" << endl;
    cout << "++++" << passtrimws("") << "++++" << endl;
  }
  // end of test 2
   
  /*======================================================================*/
  // test 3
  if (opt.test3) 
  {
    if (opt.verbose)
    { cout << "test 3: test class tsoftfile" << endl; }
    std::ifstream is(filename.c_str());
    datrw::tsoft::TSOFTfile thefile(is, opt.debug);

    // report file free
    sff::FREE filefree;
    filefree.append(thefile.free());
    cout << filefree;

    const datrw::tsoft::Datacontainer& dc=thefile.dc();
    // report on channels
    for (int ich=0; ich<dc.nchannels(); ++ich)
    {
      cout << "channel #" << ich << ":" << endl;
      cout << "===========" << endl;
      const datrw::tsoft::Channeldata& cd=dc.channel(ich, true);
      const datrw::tsoft::Channelinfo& ci=cd.chinfo();
      sff::FREE channelfree;
      channelfree.append(datrw::tsoft::channelinfofree(ci));
      cout << channelfree;
      cout << "number of available sequences: " << cd.ntraces() << endl;
      for (int iseq=0; iseq<cd.ntraces(); ++iseq)
      {
        const datrw::tsoft::Datatrace& ds=cd.trace(iseq);
        cout << "  sequence #" << iseq << ":" << endl;
        cout << "  =============" << endl;
        cout << "  start:             " << ds.date().timestring() << endl;
        cout << "  dt:                " << ds.interval().timestring() << endl;
        datrw::Tdseries s=ds.series();
        cout << "  number of samples: " << s.size() << endl;
        int nsmp=s.size() < 10 ? s.size() : 10;
        int f=s.f();
        for (int isa=0; isa<nsmp; ++isa)
        {
          cout << "    " << s(f+isa) << endl;
        }
      }
    }
  }
  // end of test 3
   
  /*======================================================================*/
  // t test
  if (opt.ttest) 
  {
    if (opt.verbose)
    { cout << "ttest: translation table" << endl; }
    datrw::tsoft::reporttranslation(cout);
    if (datrw::tsoft::translationisunique())
    {
      cout << "The translation table is unique" << endl;
    }
    else
    {
      cout << "ATTENTION: The translation table is ambiguous" << endl;
    }
  }
  // end of t test
   
  /*======================================================================*/
  // s test
  if (opt.stest) 
  {
    if (opt.verbose)
    { cout << "stest: itsoftstream" << endl; }
    std::ifstream is(filename.c_str());
    datrw::itsoftstream its(is);
    cout << its.free();
    DATRW_assert(its.good(),
                   "stream is not good although no trace has been read");
    while (its.good())
    {
      datrw::Tdseries series=its.dseries();
      sff::WID2 wid2=its.wid2();
      sff::FREE free=its.free();
      cout << endl;
      cout << "WID2 line of next trace:" << endl;
      cout << wid2.line();
      cout << "FREE block of next trace:" << endl;
      cout << free;
      cout << "some samples of of next trace:" << endl;
      int nsa=series.size() < 10 ? series.size() : 10;
      for (int isa=0; isa<nsa; ++isa)
      {
        cout << series(isa+series.f()) << endl;
      }
    }
  }
  // end of s test
}

/* ----- END OF tsofttest.cc ----- */
