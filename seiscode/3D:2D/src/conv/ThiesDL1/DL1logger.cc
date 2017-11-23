/*! \file DL1logger.cc
 * \brief data logging program
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/03/2014
 * 
 * data logging program
 * 
 * Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 27/11/2008   V0.9   Thomas Forbriger (thof)
 *  - 19/12/2008   V1.0   first version that is fully operational
 *  - 22/12/2008   V1.1   
 *                        - handle exceptions
 *                        - provide information on program execution
 *                        - correct start time for current data
 *                        - maintain an integer time raster for polls
 *                        - log software version
 *                        - tolerance mode: non-matching data lines
 *  - 18/12/2012   V1.2   
 *                        - begin work on implementation of date specific data
 *                          paths
 *                        - implement alternative output formats
 *  - 20/03/2014 thof:    
 *                        - accept lines with duplicate time stamps as part of
 *                          normal operation, ignore tolerate redundant option
 *  - 24/03/2014 thof:
 *                        - new version uses path patterns and provides
 *                          libdatrwxx output formats
 *  - 31/03/2014 thof:
 *                        - report DL1 logger software version
 * 
 * ============================================================================
 */
/*! \page DL1logger The DL1logger binary executable
 *
 * This program runs in an infinite loop and controls data acquisition through
 * the Thies DL1 data logger.
 * It maintains the logger's internal clock and maintains a collection of data
 * files.
 * Execute \code DL1logger -help\endcode to obtain a description of the
 * program's functionality.
 *
 * \sa DL1logger.cc
 *
 * - \ref DL1logger_sec_usage
 * - \ref DL1logger_sec_internals
 *   - \ref DL1logger_subsec_flow
 *   - \ref DL1logger_subsec_interfaces
 */
// more contents to the doxygen page are added at the end of the file
#define DL1LOGGER_VERSION \
  "DL1LOGGER   V2014-03-24   data logging program for Thies DL1"
#define DL1LOGGER_CVSID \
  "$Id$"

// #define _GNU_SOURCE
#include <string.h>
#include <signal.h>
#include <iostream>
#include <string>
#include <list>
#include <tfxx/commandline.h>
#include <tfxx/stringfunc.h>
#include <datrwxx/thiesdl1.h>
#include <libtime++.h>
#include "thiesdl1.h"
#include "functions.h"
#include "error.h"
#include "logger.h"
#include "memory.h"
#include "version.h"
#include "DL1logger_longusage_text.h"
#include "DL1logger_notes_text.h"
#include "DL1logger_operation_text.h"
#include "DL1logger_usage_text.h"

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, toleratewrongtime;
  std::string device;
  bool lastdateset, datasel, dumpsel;
  libtime::TAbsoluteTime lastdate;
  std::string memorypath, datapath, datatypes, activepath;
  int interval;
}; // Options

/*----------------------------------------------------------------------*/

// identifier for syslog
using dl1::Logger;

/*----------------------------------------------------------------------*/

// constants
const std::string cmemkeylastdate("LastDateFullyProcessed");
const libtime::TRelativeTime conedayandonehour(1,1);

/*----------------------------------------------------------------------*/

/*! \fn void logabort(int isig)
 * \brief signal handler.
 * This function is used as signal handler. It leaves a note in the log files
 * upon program termination.
 * \param isig signal number
 */
void logabort(int isig)
{
  Logger(dl1::log_emerg) << "aborting due to signal: " << strsignal(isig);
  exit(EXIT_FAILURE);
}

/*======================================================================*/

int main(int iargc, char* argv[])
{
  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: device file
    {"port",arg_yes,"/dev/ttyS0"},
    // 3: debug mode
    {"DEBUG",arg_no,"-"},
    // 4: debug mode
    {"lastdate",arg_yes,"-"},
    // 5: path to memory file 
    {"memory",arg_yes,"./DL1loggermemory"},
    // 6: data path pattern for completed days
    {"datapath",arg_yes,"./%T/%Y/DL1data%Y%M%D.%T"},
    // 7: additional data type for output
    {"datatypes",arg_yes,"bin"},
    // 8: interval to collect data
    {"interval",arg_yes,"5"},
    // 9: tolerate wrong time in data line
    {"toleratewrongtime",arg_no,"-"},
    // 10: print extended usage
    {"xhelp",arg_no,"-"},
    // 11: data path pattern for current day's data
    {"activepath",arg_yes,"./%T/DL1active.%T"},
    {NULL}
  };
  
  /*======================================================================*/
  
  // send initial message to system log
  Logger::setident("DL1logger");
  Logger() << DL1LOGGER_VERSION;

  // prepare signal handlers to ensure logging of program exit
  signal(SIGHUP, *logabort);
  signal(SIGINT, *logabort);
  signal(SIGQUIT, *logabort);
  signal(SIGABRT, *logabort);
  signal(SIGTERM, *logabort);

  // no arguments? print usage...
  // but do not abort, since this it is correct to call the program without
  // arguments
  if (iargc<2) 
  {
    cerr << DL1LOGGER_VERSION << endl;
    cerr << DL1logger_usage_text << endl;
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if ((cmdline.optset(0)) || (cmdline.optset(10)))
  {
    cerr << DL1LOGGER_VERSION << endl;
    cerr << DL1_LOGGER_SOFTWARE_VERSION << endl;
    cerr << DL1logger_usage_text << endl;
    cerr << DL1logger_longusage_text << endl;
    if (cmdline.optset(10))
    {
      cerr << DL1logger_operation_text << endl;
      cerr << DL1logger_notes_text << endl;
      cerr << "Source code version" << endl;
      cerr << "-------------------" << endl;
      {
        int i=0;
        while (dl1::CVSIDS[i]!=0)
        {
          cerr << dl1::CVSIDS[i] << endl;
          ++i;
        }
      }
    }
    exit(0);
  }

  /*----------------------------------------------------------------------*/

  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.device=cmdline.string_arg(2);
  opt.debug=cmdline.optset(3);
  opt.lastdate=libtime::utc();
  opt.lastdateset=cmdline.optset(4);
  if (opt.lastdateset) 
  {
    opt.lastdate=libtime::TAbsoluteTime(cmdline.string_arg(4));
  }
  opt.memorypath=cmdline.string_arg(5);
  opt.datapath=cmdline.string_arg(6);
  opt.datatypes=cmdline.string_arg(7);
  opt.interval=cmdline.int_arg(8);
  opt.toleratewrongtime=cmdline.optset(9);
  // option 10 is -xhelp
  opt.activepath=cmdline.string_arg(11);

  opt.lastdate=dl1::dateonly(opt.lastdate);

  /*======================================================================*/
  // here we go

  /*! \var std::string stage="entering try block";
   * \brief variable to provide current stage of execution.
   * The program stores a textual description of the current processing stage
   * in this variable. This way log output can indicate the current stage of
   * processing.
   */
  std::string stage="entering try block";
  
  // catch exceptions
  try {

  // report program version to log files
  Logger::beverbose(true);
  cout << DL1LOGGER_VERSION << endl; 
  Logger() << DL1_LOGGER_SOFTWARE_VERSION;
  Logger() << "new instance is launched at";
  Logger() << libtime::utc().timestring();
  Logger() << "software version:";
  {
    int i=0;
    while (dl1::CVSIDS[i]!=0)
    {
      Logger() << dl1::CVSIDS[i];
      ++i;
    }
  }

  /*----------------------------------------------------------------------*/
  // report command line settings
  Logger() << "device:                " << opt.device;
  if (opt.lastdateset)
  {
    Logger() << "last date set to:      " << opt.lastdate.timestring();
  }
  else
  {
    Logger() << "last date defaults to: " << opt.lastdate.timestring();
  }
  Logger() << "path to memory file:   " << opt.memorypath;
  Logger() << "data path pattern:     " << opt.datapath;
  Logger() << "active data path pat.: " << opt.activepath;
  Logger() << "poll interval:         " << opt.interval << " minutes";
  if (opt.toleratewrongtime)
  {
    Logger() << "Be tolerant against wrong time in data lines, do not abort";
  }

  /*----------------------------------------------------------------------*/
  // pass options to classes

  dl1::Record::betolerantagainstwrongtime(opt.toleratewrongtime);

  /*----------------------------------------------------------------------*/
  // initialize memory
  
  dl1::Memory mymemory(opt.memorypath);

  // report contents
  {
    bool novaluesinmemory=true;
    Logger() << "values obtained from memory file:";

    if (mymemory.available(cmemkeylastdate))
    {
      Logger() << "date last day fully processed: " 
        << mymemory(cmemkeylastdate);
      novaluesinmemory=false;
    }

    if (novaluesinmemory)
    {
      Logger() << "no values were present in the memory file";
    }
  }

  // set my values
  mymemory.setdefault(cmemkeylastdate, opt.lastdate.hierarchicalstring());

  /*----------------------------------------------------------------------*/

  Logger() << "open port " << opt.device;
  dl1::DL1 port(opt.device, opt.debug);

  // log status
  Logger() << status(port);

  /*----------------------------------------------------------------------*/
  libtime::TRelativeTime pollinterval(0,0,opt.interval);
  Logger() << "poll interval: " << pollinterval.timestring();

  // =====================
  // the never ending loop
  // =====================
  Logger() << "entering the infinite loop";
  Logger::beverbose(opt.verbose);
  while(true)
  {
    // first of all: activate DL1 (wake-up Thies DL1 data logger)
    port.activate();

    // get date of last date dumped
    libtime::TAbsoluteTime lastdate
      =dl1::dateonly(libtime::TAbsoluteTime(mymemory(cmemkeylastdate)));
    // get date of day that should be dumped next
    libtime::TAbsoluteTime nextdaytodump=lastdate+dl1::oneday;
    // current date and time
    libtime::TAbsoluteTime now=libtime::utc();

    bool clockinitialized=false;

    /* is there a full day to dump?
     * ----------------------------
     * there might be more than one day waiting to be dumped;
     * each poll will increase nextdaytodump by one day; keep on processing
     * while nextdaytodump is further in the past than one day and one hour
     */
    while ((now-nextdaytodump)>conedayandonehour)
    {
      stage="entering block to process days for which data is complete";
      Logger() << DL1_LOGGER_SOFTWARE_VERSION;
      Logger() << stage;

      // report logger status
      Logger() << dl1::status(port);

      // what was done last?
      Logger() << "last date processed: " << lastdate.timestring();

      // initialize clock if not yet done today
      if (!clockinitialized)
      {
        stage="set data logger's clock";
        Logger() << stage;
        initializeclock(port); 
        clockinitialized=true;
      }

      // process the next day
      stage="process the next day for which data is complete: "
        + nextdaytodump.timestring();
      Logger() << stage;
      processday(port, opt.datapath, opt.datatypes, lastdate+dl1::oneday);
      lastdate += dl1::oneday;
      mymemory.set(cmemkeylastdate, lastdate.hierarchicalstring());
      nextdaytodump=lastdate+dl1::oneday;
    }

    /* process current data (i.e. the not yet completed day of recording)
     * 
     * files containing recent data of not yet completed days have file
     * basenames 'active'.
     */

    libtime::TAbsoluteTime currentday(dl1::dateonly(nextdaytodump));
    Logger(dl1::log_info) << "get recent data since " 
      << currentday.timestring();

    stage="get current status";
    dl1::Tlistofstring info=dl1::status(port);

    stage="get current data";
    dl1::Record record=dl1::readrecordsincedate(port, currentday);

    stage="write current data";
    std::string dumpfilename=dl1::mkpathname(opt.activepath, currentday,
                                             datrw::thiesdl1::streamID,
                                             true);
    dl1::dumptofile(dumpfilename, record, info);
    dl1::Tlistofstring types;
    tfxx::string::gen_split(types,
                            tfxx::string::patsubst(opt.datatypes, ",", " "), 
                            std::string(" "), true);
    for (dl1::Tlistofstring::const_iterator I=types.begin();
         I != types.end(); ++I)
    {
      std::string filename=dl1::mkpathname(opt.activepath, currentday, 
                                           *I, true);
      dl1::writedata(filename, *I, record, info);
    }
    
    // initialize clock if weird time in data set
    if (record.foundunexpecteddatatime())
    {
      stage="weird time in data: set data logger's clock";
      Logger(dl1::log_warning) << "weird time in data: initialize clock";
      initializeclock(port); 
    }
      
    /* wait (sleep until next poll should be done)
     * -------------------------------------------
     *
     * if previous steps took too much time, we might be more than one
     * pollinterval ahead; advance nexttimetopoll until it is in the future;
     * then calculate appropriate number of seconds to fall asleep
     */
    stage="go sleeping";
    now=libtime::utc();
    int nintervals=(now-dl1::dateonly(now))/pollinterval;
    libtime::TAbsoluteTime nexttimetopoll
      =nintervals*pollinterval+dl1::dateonly(now);
    now=libtime::utc();
    while (nexttimetopoll < now)
    {
      nexttimetopoll += pollinterval;
      now=libtime::utc();
    }
    int nsleepseconds=int(libtime::time2double(nexttimetopoll-now));
    if (nsleepseconds<0) { nsleepseconds=0; }
    Logger(dl1::log_info) << "sleep " << nsleepseconds
      << " seconds until " << nexttimetopoll.timestring();
    sleep(nsleepseconds);
  }
  // end of try-block
  } 
  catch (dl1::Exception& E)
  {
    dl1::Logger::beverbose(true);
    dl1::Logger::beverboseerr(true);
    Logger(dl1::log_emerg) << "Caught an exception from the DL1 modules:";
    E.report();
    Logger(dl1::log_emerg) << "The program will be terminated.";
    Logger(dl1::log_emerg) << "last execution stage: " << stage;
    Logger(dl1::log_emerg) << "Better restart the program re-opening the port.";
  }
  catch (...)
  {
    dl1::Logger::beverbose(true);
    dl1::Logger::beverboseerr(true);
    Logger(dl1::log_emerg) << "An unknown exception was caught!";
    Logger(dl1::log_emerg) << "last execution stage: " << stage;
    Logger(dl1::log_emerg) << "The program will be terminated.";
  }
}

/*======================================================================*/

/*! \page DL1logger
 *
 * \section DL1logger_sec_usage Program usage
 *
 * \verbinclude DL1logger_usage_text.txt
 * \verbinclude DL1logger_longusage_text.txt
 * \verbinclude DL1logger_operation_text.txt
 * \verbinclude DL1logger_notes_text.txt
 *
 * \section DL1logger_sec_internals Internals 
 * \subsection DL1logger_subsec_flow Program's workflow
 *
 * The program internally maintains the string variable \ref stage
 * in order to provide a textual description of the current processing stage.
 *
 * The programs workflow is as follows:
 * -# define character variables with usage information
 * -# define options and arguments
 * -# initialize system logger utility dl1::Logger
 * -# initialize signal handlers with function logabort()
 * -# evaluate command line
 * -# enter try-block
 *    -# report program version to log file
 *    -# pass program options and arguments to classes
 *    -# initialize an instance of dl1::Memory
 *    -# open serial port by initializing an instance of dl1::DL1
 *    -# set poll interval
 *    -# enter never-ending while loop
 *       -# wake-up Thies DL1 data logger
 *       -# prepare time values for last poll, next poll waiting and current
 *          time
 *       -# process polls for completed days while next-day-to-dump is further
 *          in the past than one day and one hour (enter while loop)
 *          -# report stage and Thies DL1 status
 *          -# set Thies DL1 internal clock if not yet done today
 *          -# poll data by calling function dl1::processday()
 *          -# update time of next day to dump and update memory
 *       -# continue by processing current day
 *       -# report current date to log file
 *       -# initialize file path of current data
 *       -# capture Thies DL1 status for data header by storing return value
 *          of dl1::status()
 *       -# poll current days data by a call to dl1::readrecordsincedate()
 *       -# create path to file to dump current data from pattern
 *       -# write data through dl1::dumptofile()
 *       -# cycle through list of output file formats
 *          -# create path to file for output of current data from pattern
 *          -# write current data through dl1::writedata()
 *       -# check for unreasonable date and time in data and re-initialize
 *          internal clock of Thies DL1 if indicated
 *       -# sleep until next time of poll as configured by pollinterval
 *    -# repeat while loop
 * -# end of try-block; try-block is left by a fatal abort or by an execption
 *    being thrown; catch exceptions
 * -# catch dl1::Exception and report to log file
 * -# catch other exceptions and report to log file
 *
 * \subsection DL1logger_subsec_interfaces Used interfaces
 *
 * Data access to the Thies DL1 data logger is provided through 
 * \ref group_func_access
 * DL1logger make use of:
 * - dl1::processday()
 * - dl1::readrecordsincedate()
 *
 * Data is output through \ref group_func_output
 * DL1logger make use of:
 * - dl1::dumptofile()
 * - dl1::writedata()
 *
 */
/* ----- END OF DL1logger.cc ----- */
