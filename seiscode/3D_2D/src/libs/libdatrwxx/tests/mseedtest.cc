/*! \file mseedtest.cc
 * \brief test mini-SEED reading
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/07/2004
 * 
 * test mini-SEED reading
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 15/07/2004   V1.0   Thomas Forbriger
 *  - 04/04/2006   V1.1   handle special EDL data features
 *  - 09/05/2006   V1.2   support Steim 2 too
 *  - 11/05/2006   V1.3   generic frame dump
 *  - 07/07/2006   V1.4   provide non fatal scan mode
 *  - 13/09/2011   V1.5   handles ASCII data streamed from Q330HR systems
 *  - 07/05/2014   V1.6   accept all type field indicators in the fixed
 *                        section of data header as defined by SEED V2.4
 * 
 * ============================================================================
 */
#define MSEEDTEST_VERSION \
  "MSEEDTEST   V1.6   test MiniSEED reading"

#include <fstream>
#include <iostream>
#include <tfxx/commandline.h>
#include<tfxx/bytesex.h>
#include <datrwxx/error.h>
#include <datrwxx/mseedread.h>
#include <datrwxx/seedstructdump.h>
#include <datrwxx/mseed.h>

using std::cout;
using std::cerr;
using std::endl;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    MSEEDTEST_VERSION "\n"
    "usage: mseedtest [-v] [-raw] [-reader] [-skip] [-stream] [-silent]" "\n"
    "                 [-nfinconsist] [-dumpsamples] [-modifiers m]" "\n"
    "                 [-logdump]\n"
    "                 file [file...]" "\n"
    "   or: mseedtest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           verbose mode" "\n"
    "-raw         Use raw reading functions from mseedstruct." "\n"
    "             This option uses an algorithm coded on mseedtest.cc\n"
    "             Tested conditions may be different from the conditions\n"
    "             tested in the actual reader functions in the library.\n"
    "             Being able to read a file with this option does not\n"
    "             necessarily mean, that the reader will decode the file\n"
    "             without complaining.\n"
    "-reader      use reading functions from mseedread" "\n"
    "-skip        skip data samples" "\n"
    "-stream      read data through stream" "\n"
    "-generic     use generic frame dump during raw test" "\n"
    "-nfinconsist make inconsistencies non fatal errors" "\n"
    "-dumpsamples dump sample values in reader mode" "\n"
    "-modifiers m format modifiers" "\n"
    "-silent      be silent in stream mode; just output by imseedstream\n"
    "             is produced; this is useful to dump ASCII log entries\n"
    "-logdump     dump ASCII log entries\n"
    "             This is identical to\n"
    "             -stream -modifiers dumpascii -silent -skip\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: raw mode
    {"raw",arg_no,"-"},
    // 3: reader mode
    {"reader",arg_no,"-"},
    // 4: skip mode
    {"skip",arg_no,"-"},
    // 5: stream mode
    {"stream",arg_no,"-"},
    // 6: stream mode
    {"generic",arg_no,"-"},
    // 7: stream mode
    {"nfinconsist",arg_no,"-"},
    // 8: dump samples in reader mode
    {"dumpsamples",arg_no,"-"},
    // 9: dump samples in reader mode
    {"modifiers",arg_yes,""},
    // 10: dump samples in reader mode
    {"silent",arg_no,"-"},
    // 11: dump samples in reader mode
    {"logdump",arg_no,"-"},
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

  // dummy operation: print option settings
  /*
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
  */ 

  bool verbosemode=cmdline.optset(1);
  bool rawmode=cmdline.optset(2);
  bool readermode=cmdline.optset(3);
  bool skipmode=cmdline.optset(4);
  bool streammode=cmdline.optset(5);
  bool genericframedump=cmdline.optset(6);

  datrw::mseed::Debug MiniSEEDdebug;
  MiniSEEDdebug.inconsistencies_are_not_fatal=cmdline.optset(7);

  bool dumpsamples=cmdline.optset(8);
  std::string modifiers=cmdline.string_arg(9);
  bool silent=cmdline.optset(10);

  if (cmdline.optset(11))
  {
    modifiers="dumpascii";
    silent=true;
    streammode=true;
    skipmode=true;
  }

  if (verbosemode) 
  { 
    cout << MSEEDTEST_VERSION << endl;
    cout << "in verbose mode" << endl; 
  }
  if (MiniSEEDdebug.inconsistencies_are_not_fatal)
  { cout << "inconsistencies are made non fatal" << endl; }

  while (cmdline.extra()) 
  {
    std::cout << std::endl;
    std::string filename=cmdline.next();
    std::cout << "file: " << filename << std::endl;
    std::cout << "using standard input block size of "
      << datrw::mseed::MiniSEEDblock::standard_block_size
      << " bytes" << endl;

    /*======================================================================*/

    if (rawmode)
    {
    cout << endl;
    cout << "raw mode" << endl;
    cout << "========" << endl;

    std::ifstream ifs(filename.c_str());
    int iblock=0;
    int irecord=0;
    datrw::mseed::MiniSEEDblock block;
    ifs >> block;
    while (ifs.good())
    {
      iblock++;
      cout << endl << "BLOCK #" << iblock << endl
                   << "===========" << endl;
      // SEED defines Motorola (big-endian) to be the standard byte order
      // for all headers
      // check our CPU type
      tfxx::ioswap::Ecpu_type mysex=tfxx::ioswap::cpu();
      DATRW_assert((mysex != tfxx::ioswap::cpu_unknown),
                       "ERROR (reading MiniSEED record): "
                       "cannot identify CPU type");
      bool doswap = (mysex == tfxx::ioswap::cpu_Intel);
      if (doswap) { cout << "swap bytes, when reading header" << endl; }
      else { cout << "do not swap bytes, when reading header" << endl; }
      datrw::mseed::SEED::ControlHeader controlheader(block.block());
      if ((controlheader.type == 'D')
          || (controlheader.type == 'R')
          || (controlheader.type == 'Q')
          || (controlheader.type == 'M'))
      {
        datrw::mseed::SEED::FixedDataRecordHeader
          dataheader(block.block(),doswap);
        if (dataheader.fblock > block.bytesize())
        {
          // try it the other way
          cout << "Could not find first Blockette!" << endl;
          cout << "Trying different byte order..." << endl;
          doswap = !doswap;
          dataheader=datrw::mseed::SEED::FixedDataRecordHeader(block.block(),
                                                                 doswap);
          DATRW_assert((dataheader.fblock < block.bytesize()),
                         "ERROR (reading MiniSEED record): "
                         "cannot find first blockette");
          cout << "Apparently we are reading raw data written by an "
            << "EarthDataLogger (EDL) :-)" << endl;
          if (doswap) { cout << "Hence"; }
          else { cout << "Hence do not"; }
          cout << " swap bytes, when reading header!" << endl; 
        }
        ++irecord;
        cout << endl;
        cout << "RECORD #" << irecord << endl;
        cout << "===========" << endl;
        datrw::mseed::SEED::dump(dataheader, cout);
        int nblockettes(dataheader.numblock);
        unsigned int blocketteadr(dataheader.fblock);
        // scan blockettes
        bool foundBlockette1000=false;
        bool foundBlockette1001=false;
        datrw::mseed::SEED::DataOnlySEEDBlockette blockette1000;
        datrw::mseed::SEED::DataExtensionBlockette blockette1001;
        for (int i=0; i<nblockettes; i++)
        {
          datrw::mseed::SEED::DataRecordBlocketteHeader
            bh(block.block(blocketteadr), doswap);
          cout << "Blockette #" << i<< endl;
          if (bh.type == 1000)
          {
            datrw::mseed::SEED::DataOnlySEEDBlockette
              bck(block.block(blocketteadr), doswap);
            datrw::mseed::SEED::dump(bck, cout);
            blockette1000=bck;
            foundBlockette1000=true;
          }
          else if (bh.type == 1001)
          {
            datrw::mseed::SEED::DataExtensionBlockette
              bck(block.block(blocketteadr), doswap);
            datrw::mseed::SEED::dump(bck, cout);
            blockette1001=bck;
            foundBlockette1001=true;
          }
          else
          {
            datrw::mseed::SEED::dump(bh, cout);
          }
          blocketteadr=bh.next;
        }
        // prepare frame reading
        if (foundBlockette1000)
        {
          // For SEED data (not header fields) the byte order my differ from
          // file to file. Use the byte order defined in the Data Only SEED
          // Blockette.
          bool dodataswap=datrw::mseed::needswap(blockette1000.bytesex);
          if (!dodataswap) { cout << "do not "; }
          cout << "swap bytes, when reading data" << endl; 
          if ((blockette1000.format == datrw::mseed::SEED::steim1) ||
              (blockette1000.format == datrw::mseed::SEED::steim2))
          {
            unsigned int isamples=0;
            int nframes=(blockette1000.reclenbytes()-dataheader.dbeg)/
              datrw::mseed::SEED::SteimFrame::blocksize;
            cout << "total number of possible frames in record: "
              << nframes << endl;
            if (foundBlockette1001)
            {
              /*
              cerr.flush();
              cout.flush();
              cout << "nframes: " << nframes << std::endl;
              cout << "blockette1001.fcount: " 
                << blockette1001.ifcount() << std::endl;
              cout << "blockette1000.reclenbytes:"
                << blockette1000.reclenbytes() << std::endl;
              cout << "dataheader.dbeg:"
                << dataheader.dbeg << std::endl;
              cout << "datrw::mseed::SEED::Steim1Frame::blocksize:"
                << datrw::mseed::SEED::Steim1Frame::blocksize << std::endl;
              */
              /*
              DATRW_assert((nframes == blockette1001.ifcount()),
                             "unexpected number of frames");
                             */
              cout << "number of frames specified in Blockette 1001: "
                << blockette1001.ifcount() << endl;
            }
            // read frames
            unsigned int pframe=dataheader.dbeg;
            for (unsigned int i=0; int(i)<nframes; ++i)
            {
              if (pframe >= block.bytesize())
              {
                ifs >> block;
                pframe=0;
                iblock++;
                cout << endl << "BLOCK #" << iblock << endl
                             << "===========" << endl;
              }
              if (isamples < dataheader.nsamp)
              { cout << "decode"; }
              else { cout << "ignore"; }
              cout << " frame #" << i+1 << "/" << nframes;
              cout << " at 0x";
              std::ostream::fmtflags flags=cout.flags();
              cout.setf(std::ios_base::hex, std::ios_base::basefield);
              // cout.setf(std::ios_base::showbase);
              cout.width(4);
              cout.fill('0');
              cout << pframe;
              cout.flags(flags);
              cout << std::endl;
              if (isamples < dataheader.nsamp)
              {
                if (blockette1000.format == datrw::mseed::SEED::steim1) 
                {
                  datrw::mseed::SEED::Steim1Frame frame(block.block(pframe),
                                                          dodataswap);
                  if (genericframedump)
                  {
                    datrw::mseed::SEED::SteimFrame& gframe=frame;
                    datrw::mseed::SEED::dump(gframe, cout);
                  }
                  else
                  {
                    datrw::mseed::SEED::dump(frame, cout);
                  }
                  frame.reset();
                  while (frame.valid())
                  {
                    ++isamples;
                    frame.next();
                  }
                }
                else
                {
                  datrw::mseed::SEED::Steim2Frame frame(block.block(pframe),
                                                          dodataswap);
                  if (genericframedump)
                  {
                    datrw::mseed::SEED::SteimFrame& gframe=frame;
                    datrw::mseed::SEED::dump(gframe, cout);
                  }
                  else
                  {
                    datrw::mseed::SEED::dump(frame, cout);
                  }
                  frame.reset();
                  while (frame.valid())
                  {
                    ++isamples;
                    frame.next();
                  }
                }
              }
              pframe += datrw::mseed::SEED::SteimFrame::blocksize;
            }
          }
          else if (blockette1000.format == datrw::mseed::SEED::ascii) 
          {
            cout << "This is an ascii record" << endl;
            unsigned int totalreclen=blockette1000.reclenbytes();
            cout << "Provides " << totalreclen
              << " bytes of data in total" << endl;
            unsigned int bytecount=dataheader.dbeg;
            unsigned int pframe=dataheader.dbeg;
            cout << ">>";
            while (bytecount < totalreclen)
            {
              if (pframe >= block.bytesize())
              {
                ifs >> block;
                pframe=0;
                iblock++;
                cout << endl << "BLOCK #" << iblock << endl
                             << "===========" << endl;
              }
              while (pframe < block.bytesize())
              {
                char c=block[pframe];
                if (isprint(c))
                {
                  cout << c;
                }
                else if (c == 0x0d)
                {
                  cout << endl << ">>";
                }
                ++pframe;
                ++bytecount;
              }
            }
            cout << endl;
          }
          else
          {
            cout << "Not steim1, steim2, or ascii format!" << std::endl;
          }
        }
        else
        {
          cout << "No Blockette 1000 available!" << std::endl;
        }
      }
      else
      {
        std::string seqnum(controlheader.seqno, 6);
        cout << "seqno: #" << seqnum << "#" 
          << controlheader.type 
          << "-"
          << controlheader.cont 
          << endl;
        datrw::mseed::SEED::dump(controlheader, cout);
      }
      ifs >> block;
    }
    } // end raw mode

    /*======================================================================*/

    if (readermode)
    {
      cout << endl;
      cout << "reader mode" << endl;
      cout << "===========" << endl;

      std::ifstream ifs(filename.c_str());
      datrw::mseed::MiniSEEDRecord record(MiniSEEDdebug);
      int irecord=0;
      while (ifs.good())
      {
        ++irecord;
        cout << endl;
        cout << "RECORD #" << irecord << endl;
        cout << "===========" << endl;
        if (skipmode) 
        { 
          if (verbosemode) { cout << "read record by skipping samples" << endl; }
          record.skipdata(ifs); 
        }
        else 
        { 
          if (verbosemode) { cout << "read record header and samples" << endl; }
          ifs >> record; 
        }
        if (record.valid())
        {
          if (!skipmode)
          {
            cout << "  last sample of previous record: " 
              << record.xm1() << endl << endl;
          }
          datrw::mseed::SEED::dump(record.recordheader(), cout);
          cout << endl;
          datrw::mseed::SEED::dump(record.blockette1000(), cout);
          cout << endl;
          if (record.hasblockette1001())
          { 
            datrw::mseed::SEED::dump(record.blockette1001(), cout); 
            cout << endl;
          }
          if (!skipmode)
          {
            cout << "data:" << endl;
            datrw::mseed::MiniSEEDRecord::Tseries data(record.data());
            cout << "       number of samples in data: " 
              << data.size() << endl;
            cout << "  last sample of previous record: " 
              << record.xm1() << endl;
            cout << "                    first sample: " 
              << data(data.f()) << endl;
            cout << "                     last sample: " 
              << data(data.l()) << endl;
            if (dumpsamples)
            {
              cout << "BEGIN OF DUMP: data values" << endl;
              for (int i=data.f(); i<=data.l(); ++i)
              {
                cout << data(i) << endl;
              }
              cout << "END OF DUMP: data values" << endl;
            }
          }
          {
            cout << endl;
            libtime::TRelativeTime dt
              =libtime::double2time(record.dt());
            libtime::TRelativeTime length=dt*(record.nsamples()-1);
            libtime::TAbsoluteTime date=record.date();
            libtime::TAbsoluteTime next=(date+dt+length);
            cout << "               sampling interval: " 
              << dt.timestring() << endl;
            cout << "                length of record: " 
              << length.timestring() << endl;
            cout << "            start of this record: " 
              << date.timestring() << endl;
            cout << "   expected start of next record: " 
              << next.timestring() << endl;
          }
        }
        else
        {
          cout << "record is invalid!" << endl;
        }
      }
    } // end reader mode

    /*======================================================================*/

    if (streammode)
    {
      cout << endl;
      cout << "stream mode" << endl;
      cout << "===========" << endl;
      cout << "format modifiers: " << modifiers << endl;
      if (silent)
      {
        cout << "silent mode: only imseedstream produces output" << endl;
      }

      std::ifstream ifs(filename.c_str());
      datrw::imseedstream is(ifs, modifiers, verbosemode);

      int itrace=0;
      while (ifs.good() && is.good() && (!is.last()))
      {
        datrw::Tiseries series;
        if (skipmode) { is.skipseries(); }
        else { is >> series; }
        ++itrace;
        if (!silent)
        {
          cout << endl;
          cout << "TRACE #" << itrace << endl;
          cout << "=========" << endl;
        }
        if (is.last() && (!silent))
        { cout << "(is the last trace in the file)" << endl; }

        sff::WID2 wid2line;
        is >> wid2line;
        if (!silent) { cout << wid2line.line() << endl; }

        if (!silent)
        {
          if (skipmode)
          {
            cout << "data samples were skipped" << endl;
          }
          else
          {
            cout << "series contains " << series.size() 
              << " samples" << endl;
            cout << "first (" << series.f() << ") sample: " 
              << series(series.f()) << endl;
            cout << "last (" << series.l() << ") sample: " 
              << series(series.l()) << endl;
          }

          if (is.hasinfo())
          {
            sff::INFO infoline;
            is >> infoline;
            cout << infoline.line() << endl;;
          }

          if (is.hasfree())
          {
            sff::FREE freeblock;
            is >> freeblock;
            cout << freeblock;
          }
        }
      }
    } // end stream mode
  }
}

/* ----- END OF mseedtest.cc ----- */
