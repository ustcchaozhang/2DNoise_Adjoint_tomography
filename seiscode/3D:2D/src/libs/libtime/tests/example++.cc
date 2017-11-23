/* this is <example++.cc>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 2000 by Thomas Forbriger (IMGF Frankfurt)
 *
 * example for libtime C++ code
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *    22/12/2000   V1.0   Thomas Forbriger
 *
 * ============================================================================
 */
 
#include"libtime++.h"
#include<iostream>

using std::cout;
using std::endl;

using namespace libtime;

#define PRINTVALUE( V ) \
  cout << #V << ":\n  " << string(V) << endl;

/*----------------------------------------------------------------------*/

void example1()
{
  cout << "\n"
       << "Example 1: How many seconds are there within a day?\n"
       << "---------­\n"
       << "\n"
       << "  First we create a relative time span of one day length:\n";

  libtime::TRelativeTime oneday(1);
  cout << "    " << string(oneday) << endl;

  cout << "\n"
       << "  Then we create a relative time span of one second length:\n";

  libtime::TRelativeTime onesecond(0,0,0,1);
  cout << "    " << string(onesecond) << endl;

  cout << "\n"
       << "  Then we divide the one by the other:\n"
       << "    " << oneday/onesecond << endl;

  cout << "  So there are " << oneday/onesecond 
                            << " seconds within one day.\n"
       << "  Too easy?\n";
}

/*----------------------------------------------------------------------*/

void example2()
{
  libtime::TRelativeTime timespan;
  int nsamples(1300);

  cout << "\n"
       << "Example 2: Which is the time of the last sample?\n"
       << "----------\n"
       << "\n";

  cout << "  We have a time series that starts with a first sample at:\n";
  std::string startstring("2000/12/20 22:34:53.241");
  cout << "  " << startstring << endl;

  cout << "\n"
       << "  We create an absolute time for the first sample:\n";
  libtime::TAbsoluteTime starttime(startstring);
  cout << "  " << string(starttime) << endl;

  cout << "\n"
       << "  The sampling interval of the time series is:\n";
  libtime::TRelativeTime interval(0,0,0,10);
  cout << "  " << string(interval) << endl;

  cout << "\n"
       << "  The total series has " << nsamples << " samples"
               << " with " << nsamples-1 << " intervals.\n"
       << "  That is a time span of:\n";
  timespan=interval*(nsamples-1);
  cout << "  " << string(timespan) << endl;

  cout << "\n"
       << "  Hence the last sample was taken at:\n"
       << "  " << string(starttime+timespan) << endl;

  cout << "  You'd like a more advanced one?\n";

}

/*----------------------------------------------------------------------*/

void example3sub1(std::string s1, std::string s2,
                  libtime::TAbsoluteTime t1, libtime::TAbsoluteTime t2,
                  libtime::TRelativeTime i)
{
  cout << "\n"
       << "  So " << s1 << " is ";
  t1 < t2 ? cout << "earlier" : cout << "later";
  cout << endl;
  cout << "  than " << s2 << "." << endl;;
  cout << "  The time span between them is " << std::string(t2-t1) << endl;
  cout << "  It is ";
  t2-t1 < i/2 ? cout << "smaller" : cout << "larger";
  cout << " than half a sampling interval.\n";
}

void example3()
{
  libtime::TAbsoluteTime first("2000/12/8 9:35:1.2671"),
                      reqstart("2000/12/8 12:37:14"),
                        reqend("2000/12/8 12:43:52"),
                        exstart, exend;
  libtime::TRelativeTime interval(0,0,0,0,5), reqspan, exspan;

  cout << "\n"
       << "Example 3: Serving a data request\n"
       << "----------\n"
       << "\n";

  cout << "  Imagine that you might have a database file that starts\n"
       << "  with a first sample at "
       << std::string(first) << ".\n"
       << "  A client requests a time series that should start at\n"
       << "  " << std::string(reqstart) << " and end at "
       << "  " << std::string(reqend) << ".\n"
       << "  The sampling interval is " << std::string(interval) << endl;

  long int offset1=(reqstart-first)/interval;
  long int offset2=(reqend-first)/interval;
  exstart=first+interval*(offset1);
  exend=first+interval*(offset2);
  reqspan=reqend-reqstart;
  exspan=exend-exstart;
  
  cout << "  The requested time span is "
       << std::string(reqspan) << endl;

  cout << "\n"
       << "  The client will receive a data segment from offset "
       << offset1 << " to " << offset2 << ".\n";

  cout << "  The segment's first sample is at " << std::string(exstart) << "\n"
       << "  and it's last one is at " << std::string(exend) << endl;

  example3sub1("the first requested sample",
               "the first delivered sample",
               reqstart, exstart, interval);

  example3sub1("the last requested sample",
               "the last delivered sample",
               reqend, exend, interval);

  example3sub1("the first requested sample",
               "the sample before the first delivered one",
               reqstart, exstart-interval, interval);

  example3sub1("the first requested sample",
               "the sample after the first delivered one",
               reqstart, exstart+interval, interval);

}

/*----------------------------------------------------------------------*/

void example4()
{
  libtime::TAbsoluteTime first, second;

  cout << "\n"
       << "Example 4: How about leap-years?\n"
       << "----------\n"
       << "\n";

  first="1979/02/15"; second="1979/03/01";
  cout << "\n"
       << "  The time span between " << string(first) << endl
       << "                    and " << string(second) << endl
       << "                     is " << string(second-first) << endl;

  first="1980/02/15"; second="1980/03/01";
  cout << "\n"
       << "  The time span between " << string(first) << endl
       << "                    and " << string(second) << endl
       << "                     is " << string(second-first) << endl;

  first="2000/02/15"; second="2000/03/01";
  cout << "\n"
       << "  The time span between " << string(first) << endl
       << "                    and " << string(second) << endl
       << "                     is " << string(second-first) << endl;

  first="2100/02/15"; second="2100/03/01";
  cout << "\n"
       << "  The time span between " << string(first) << endl
       << "                    and " << string(second) << endl
       << "                     is " << string(second-first) << endl;
}

/*----------------------------------------------------------------------*/

void example5()
{
  libtime::TAbsoluteTime exampletime;

  cout << "\n"
       << "Example 5: What is a spurious year value?\n"
       << "----------\n"
       << "\n";

  cout << "  If you set an absolute time to a year for which I do not\n"
       << "  expect any digital seismological data to be available, I\n"
       << "  will regard this as a spurious year value. However any\n"
       << "  calculations with such values will lead to the expected\n"
       << "  results apart from the annoying warning message.\n";

  cout << "\n"
       << "  I do not expect digital data for years before 1970:\n";

  exampletime="1972/1/1";
  exampletime="1971/1/1";
  exampletime="1970/1/1";
  exampletime="1969/1/1";
  exampletime="1968/1/1";

}

/*----------------------------------------------------------------------*/

void example6sub(std::string exampledate)
{
  cout << "  " << exampledate << " means " 
       << std::string(libtime::TAbsoluteTime(exampledate)) << endl;
}

void example6()
{
  libtime::TAbsoluteTime exampletime;

  cout << "\n"
       << "Example 6: How about year value abbreviation?\n"
       << "----------\n"
       << "\n";

  cout << "  You may abbreviate year values by two digits. The year\n"
       << "  will be automatically expanded to a full qualified value.\n"
       << "  The initial date is "
       << std::string(libtime::TAbsoluteTime("1970/1/1")) << ":" << endl;

  example6sub("72/1/1");
  example6sub("71/1/1");
  example6sub("70/1/1");
  example6sub("69/1/1");
  example6sub("68/1/1");

  cout << "\n"
       << "  This means that you can not set the year to values below 100:\n";
  example6sub("101/1/1");
  example6sub("100/1/1");
  example6sub("99/1/1");
  example6sub("0/1/1");

}

/*----------------------------------------------------------------------*/

void example7()
{
  cout << "\n"
       << "Example 7: Convenient ways to specify times\n"
       << "----------\n"
       << "\n";

  PRINTVALUE( Hours(6) );
  PRINTVALUE( 118*Days()+4*Hours()+10*Minutes() );
  PRINTVALUE( Minutes(34.123456) );
  PRINTVALUE( Minutes(34,123,456) );
  PRINTVALUE( Seconds(34.123456) );
  PRINTVALUE( Seconds(34,123,456) );
  PRINTVALUE( Seconds(90072) );
  PRINTVALUE( 2.5*Days() );
  PRINTVALUE( Days(2.5) );
  PRINTVALUE( Seconds(2.5*time2double(Days())) );
}

/*----------------------------------------------------------------------*/

int main()
{
  cout << "Example++\n\n"
       << "This is an example program for the libtime++.a library.\n";

  cout << "\n"
       << "Time comes in two different flavours:\n"
       << "1. absolute time (like today at 10 o'clock)\n"
       << "2. relative times (like the time span between now and christmas)\n"
       << "The following examples will show how to use and combine these\n"
       << "two flavours.\n";

  example1();
  example2();
  example3();
  example4();
  example5();
  example6();
  example7();

  cout << "\n"
       << "That's it... any questions?\n";
}
 
/* ----- END OF example++.cc ----- */
