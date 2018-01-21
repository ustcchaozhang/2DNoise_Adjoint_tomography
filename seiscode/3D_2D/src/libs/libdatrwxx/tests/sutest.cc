/*! \file sutest.cc
 * \brief test Seismic Unix reading module
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/11/2010
 * 
 * test Seismic Unix reading module
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/11/2010   V1.0   Thomas Forbriger
 *  - 29/03/2011   V1.1   test of ScalCoo::power()
 *  - 01/04/2011   V1.2   write test
 *  - 21/01/2012   V1.3   prepared isustream and osustream to take modifiers
 *  - 02/12/2016   V1.4   report additional header field values
 * 
 * ============================================================================
 */
#define SUTEST_VERSION \
  "SUTEST   V1.4   test Seismic Unix reading module"

#include <iostream>
#include <fstream>
#include <tfxx/commandline.h>
#include <datrwxx/suheaderstruct.h>
#include <datrwxx/su.h>
#include <datrwxx/sucomanager.h>

using std::cout;
using std::cerr;
using std::endl;

#define CODE( C ) std::cout << "CODE: " << #C << "\n"; C
#define VALUE( V ) std::cout << "VALUE: " << #V << "=" << V << "\n";

struct Options {
  bool verbose, debug, streamfile, scanfile, doscaletest;
  bool docootest, dowritetest;
  std::string filename, sfilename, ofilename, imodifier, omodifier;
  short scale;
  double coordinate;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SUTEST_VERSION "\n"
    "usage: sutest [-v] [-D] [-file filename] [-stream filename]" "\n"
    "              [-scale s] [-coordinate v] [-write filename]" "\n"
    "              [-imodifier m] [-omodifier m]" "\n"
    "   or: sutest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    SUTEST_VERSION "\n"
    "\n"
    "-v                 be verbose\n"
    "-D                 switch on DEBUG mode\n"
    "-file filename     scan file, using elementary SU structs and\n"
    "                   report contents\n"
    "                   input modifiers are effective\n"
    "-stream filename   use isustream to read an SU data file and\n"
    "                   report contents\n"
    "                   input modifiers are effective\n"
    "-write filename    write a synthetic SU data file containing\n"
    "                   sine signals\n"
    "                   output modifiers are effective\n"
    "-scale s           test the ScalCoo struct, which is used to\n"
    "                   hold scalco and scalel header values\n"
    "                   input and output modifiers are effective\n"
    "-coordinate v      test modules ScalCoo, Coordinates, and\n"
    "                   SUheader\n"
    "                   input and output modifiers are effective\n"
    "-imodifier m       set input stream modifiers\n"
    "-omodifier m       set output stream modifiers\n"
    "\n"
    "If no option is selected, the program reports the size of the\n"
    "TraceHeaderStruct and terminates.\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: debug mode
    {"D",arg_no,"-"},
    // 3: file to scan
    {"file",arg_yes,"-"},
    // 4: file to scan with stream
    {"stream",arg_yes,"-"},
    // 5: test coordinate scaling for scale value
    {"scale",arg_yes,"-"},
    // 6: test coordinate scaling for coordinate value
    {"coordinate",arg_yes,"-"},
    // 7: write a test file
    {"write",arg_yes,"-"},
    // 8: use modifier string for input
    {"imodifier",arg_yes,""},
    // 9: use modifier string for output
    {"omodifier",arg_yes,""},
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
  opt.debug=cmdline.optset(2);
  opt.scanfile=cmdline.optset(3);
  opt.filename=cmdline.string_arg(3);
  opt.streamfile=cmdline.optset(4);
  opt.sfilename=cmdline.string_arg(4);
  opt.doscaletest=cmdline.optset(5);
  opt.scale=cmdline.int_arg(5);
  opt.docootest=cmdline.optset(6);
  opt.coordinate=cmdline.double_arg(6);
  opt.dowritetest=cmdline.optset(7);
  opt.ofilename=cmdline.string_arg(7);
  opt.imodifier=cmdline.string_arg(8);
  opt.omodifier=cmdline.string_arg(9);

  cout << "size of TraceHeaderStruct " << 
    sizeof(datrw::su::TraceHeaderStruct) << endl;

  /*----------------------------------------------------------------------*/

  if (opt.dowritetest)
  {
    cout << "Write test file\n"
      <<    "---------------" << endl;
    if (opt.verbose)
    {
      cout << "write to file " << opt.ofilename << endl;
    }
    std::ofstream ofs(opt.ofilename.c_str(),
                      datrw::osustream::openmode);
    datrw::osustream os(ofs, opt.omodifier, opt.debug);
    ::sff::WID2 wid2;
    wid2.dt=1.e-4;
    wid2.station="stat";
    wid2.channel="chan";
    wid2.auxid="auxid";
    wid2.instype="inst";
    ::sff::SRCE srce;
    ::sff::INFO info;
    ::sff::verbose(cout, srce);
    os << srce;
    const int mtrace=3;
    const int msamp=100;
    ::datrw::Tfseries series(0,msamp-1);
    for (int itrace=0; itrace<mtrace; ++itrace)
    {
      cout << endl << "trace #" << itrace << endl;
      wid2.date=srce.date+libtime::double2time(wid2.dt*msamp*itrace);
      os << wid2;
      info.cx=1.*(itrace+1);
      os << info;
      ::sff::verbose(cout, wid2);
      ::sff::verbose(cout, info);
      
      for (int isamp=0; isamp<msamp; ++isamp)
      {
        series(isamp)=std::sin(6*3.14159265358*
                               ((static_cast<double>(isamp)/msamp)
                                +(static_cast<double>(itrace)/mtrace)));
      }
      os << series;
    }
  }

  /*----------------------------------------------------------------------*/

  if (opt.scanfile)
  {
    std::ifstream ifs(opt.filename.c_str(),
                      datrw::isustream::openmode);
    datrw::su::options::SUHeaderControl hc
      =datrw::su::inputmodifiers(opt.imodifier, opt.debug);
    datrw::su::SUheader header(hc, opt.debug);
    bool hot=true;
    while (hot)
    {
      try {
        header.read(ifs);
        cout << "trid:   " << header.Mheader.trid << endl;
        cout << "ns:     " << header.Mheader.ns << endl;
        cout << "delrt:  " << header.Mheader.delrt << endl;
        cout << "scalco: " << header.Mheader.scalco << endl;
        cout << "scalel: " << header.Mheader.scalel << endl;
        datrw::Tfseries series(header.Mheader.ns);
        char *ipointer=reinterpret_cast<char *>(series.pointer());
        DATRW_Xassert(ifs.read(ipointer, series.size()*sizeof(float)),
                      "ERROR: reading SU samples",
                      ::datrw::su::SUReadException);
        cout << "read " << series.size() << " samples"
          << " which are " << series.size()*sizeof(float)
          << " characters" << endl;
        cout << header.wid2().line() << endl;
        cout << "srce date: " << header.dateofshot().timestring() << endl;
        cout << "delay: " << header.delay().timestring() << endl;
        cout << "time of data: " << header.dateoffirstsample().timestring() << endl;
        cout << header.info().line() << endl;
        cout << header.srce().line() << endl;
      }
      catch (...)
      {
        hot=false;
        cout << "CAUGHT EXCEPTION" << endl;
      }
    }
  }

  /*----------------------------------------------------------------------*/

  if (opt.streamfile)
  {
    std::ifstream ifs(opt.sfilename.c_str(),
                      datrw::isustream::openmode);
    datrw::isustream is(ifs, opt.imodifier, opt.debug);
    ::datrw::Tfseries series;
    ::sff::WID2 wid2;
    ::sff::SRCE srce;
    ::sff::INFO info;
    int itrace=0;
    is >> srce;
    ::sff::verbose(cout, srce);
    while (is.good())
    {
      ++itrace;
      cout << endl << "trace #" << itrace << endl;
      is >> series;
      is >> wid2;
      is >> info;
      ::sff::verbose(cout, wid2);
      ::sff::verbose(cout, info);
      int l=3;
      l=series.l()>l ? l:series.l();
      for (int i=series.f(); i<=l; ++i)
      {
        cout << "s("<<i<<")="<<series(i)<<" ";
      }
      cout << endl;
      l=series.l()-2;
      l=series.f()>l ? series.f():l;
      for (int i=l; i<=series.l(); ++i)
      {
        cout << "s("<<i<<")="<<series(i)<<" ";
      }
      cout << endl;
    }
  }

  /*----------------------------------------------------------------------*/

  if (opt.doscaletest)
  {
    datrw::su::options::SUHeaderControl hc
      =datrw::su::outputmodifiers(opt.omodifier, opt.debug);
    datrw::su::options::SUHeaderControl ihc
      =datrw::su::inputmodifiers(opt.imodifier, opt.debug);
    hc.spatialsampling.bestrict=ihc.spatialsampling.bestrict;
    cout << "scale value: " << opt.scale << endl;
    ::datrw::su::ScalCoo co(hc.spatialsampling, opt.debug);
    co.set(opt.scale, 34500);
    cout << "scale in ScalCoo: " << co.scale << endl;
    cout << "coordinate in ScalCoo: " << co.coo << endl;
    cout << "value from ScalCoo: " << co.value() << endl;
    cout << "power from ScalCoo: " << co.power() << endl;
    co.adjustscale();
    cout << "scale in ScalCoo after adjust: " << co.scale << endl;
    cout << "coordinate in ScalCoo: " << co.coo << endl;
    cout << "value from ScalCoo: " << co.value() << endl;
  }

  /*----------------------------------------------------------------------*/

  if (opt.docootest)
  {
    cout << "Test su coordinate manager\n"
      <<    "==========================\n\n";
    datrw::su::options::SUHeaderControl hc
      =datrw::su::outputmodifiers(opt.omodifier, opt.debug);
    datrw::su::options::SUHeaderControl ihc
      =datrw::su::inputmodifiers(opt.imodifier, opt.debug);
    hc.spatialsampling.bestrict=ihc.spatialsampling.bestrict;
    cout << "Test module ::datrw::su::ScalCoo\n"
      <<    "--------------------------------\n" << endl;
    cout << "check det function\n";
    CODE( ::datrw::su::ScalCoo co(hc.spatialsampling, opt.debug); )
    for (int i=-4; i<5; ++i)
    {
      if (i<0) 
      {
        CODE( co.set(-std::pow(10, -i), 1); )
      }
      else
      {
        CODE( co.set(std::pow(10, i), 1); )
      }
      cout << "i: " << i << "; co.scale " << co.scale
           << "; co.coo " << co.coo
           << "; co.power() " << co.power()
           << "; co.value() " << co.value()
           << endl;
    }
    cout << "\n";

    CODE( co.set(opt.coordinate); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    CODE( co.scaletopower(-3); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    CODE( co.scaletopower(1); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    CODE( co.scaletopower(3); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    VALUE( co.smallestpower(hc.spatialsampling.scalco) );
    CODE( co.scaletopower(co.smallestpower(hc.spatialsampling.scalco)); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    CODE( co.set(opt.coordinate); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    CODE( co.adjustscale(); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    VALUE( co.smallestpower(hc.spatialsampling.scalco) );
    CODE( co.scaletopower(co.smallestpower(hc.spatialsampling.scalco)); )
    VALUE( opt.coordinate );
    VALUE( co.scale );
    VALUE( co.coo );
    VALUE( co.power() );
    VALUE( co.value() );
    cout << endl;

    cout << "\n"
      <<    "Test module ::datrw::su::Coordinates\n"
      <<    "------------------------------------\n" << endl;
    ::datrw::su::Coordinates coo(hc.spatialsampling, opt.debug);
    coo.sx.set(opt.coordinate);
    coo.gy.set(opt.coordinate);
    cout << "values:"
      << " sx: " << coo.sx.value()
      << " sy: " << coo.sy.value()
      << " sdepth: " << coo.sdepth.value()
      << " gx: " << coo.gy.value()
      << " gy: " << coo.gy.value()
      << " gelev: " << coo.gelev.value()
      << endl;
    cout << "call equalizescaling()" << endl;
    coo.equalizescaling();
    cout << "values:"
      << " sx: " << coo.sx.value()
      << " sy: " << coo.sy.value()
      << " sdepth: " << coo.sdepth.value()
      << " gx: " << coo.gy.value()
      << " gy: " << coo.gy.value()
      << " gelev: " << coo.gelev.value()
      << endl;

    cout << "\n\n"
      <<    "Test module ::datrw::su::SUheader\n"
      <<    "---------------------------------\n" << endl;
    ::datrw::su::SUheader suh(hc, opt.debug);
    ::sff::SRCE srce;
    srce.cy=opt.coordinate;
    cout << "SRCE data passed to su header:" << endl;
    ::sff::verbose(cout, srce);
    suh.set(srce);
    ::sff::INFO info;
    info.cx=opt.coordinate;
    cout << "INFO data passed to su header:" << endl;
    ::sff::verbose(cout, info);
    suh.set(info);
    cout << "INFO data returned from su header:" << endl;
    ::sff::verbose(cout, suh.info());
  }
}

/* ----- END OF sutest.cc ----- */
