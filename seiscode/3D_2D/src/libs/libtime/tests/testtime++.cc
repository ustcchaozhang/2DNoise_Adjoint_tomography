/* this is <testtime++.cc>
 * ----------------------------------------------------------------------------
 *
 * 09/08/2000 by Thomas Forbriger (IfG Stuttgart)
 *
 * comprehensive test code for libtime C++ version
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
 *
 * REVISIONS and CHANGES
 *    09/08/2000   V1.0   Thomas Forbriger
 *    22/12/2000   V1.1   changed namespace time to libtime
 *    13/01/2004   V1.2   test constructor for TRelativeTime from double
 *    15/12/2012   V1.3   test formatted string
 *
 * ============================================================================
 */

#include"libtime++.h"
#include<iostream>

using std::cout;
using std::endl;
using std::string;

using namespace libtime;

#define PRINTVALUE( V ) \
  cout << #V << ":\n  " << string(V) << endl;

/*
 * first of all declare some useful functions
 */
template<class X>
void init_from_string(const std::string &Initializer)
{
  X Instance(Initializer);
  std::cout << Instance.timestring() 
            << " initialized from string \"" 
            << Initializer << "\"" << endl;
}

void Ainit_from_value(const long int &year, 
           const long int &month, const long int &day, 
           const long int &hour=0, const long int &minute=0,
           const long int &second=0, const long int &milsec=0,
           const long int &micsec=0)
{
  libtime::TAbsoluteTime Instance(year,month,day,hour,minute,
                               second,milsec,micsec);
  std::cout << Instance.timestring() 
            << " initialized from values: " 
            << year << " " << month << " " << day << " " 
            << hour << " " << minute << " " << second << " " 
            << milsec << " " << micsec << endl;
}

void Rinit_from_value(const long int &days, 
           const long int &hour=0, const long int &minute=0,
           const long int &second=0, const long int &milsec=0,
           const long int &micsec=0)
{
  libtime::TRelativeTime Instance(days,hour,minute,
                               second,milsec,micsec);
  std::cout << Instance.timestring() 
            << " initialized from values: " 
            << days << " " 
            << hour << " " << minute << " " << second << " " 
            << milsec << " " << micsec << endl;
}

void init_from_seconds(const double& seconds)
{
  libtime::TRelativeTime value(libtime::double2time(seconds));
  std::cout << value.timestring() 
    << " initialized from "
    << seconds 
    << " seconds"
    << endl;
}

/*
 * main testing code
 */

int main() 
{
  std::cout << "Hello world!\n";

  std::cout << "\nTesting constructors"
            << "\n====================" << endl;

  std::cout << "\nTesting TAbsoluteTime"
            << "\n---------------------" << endl;
  init_from_string<libtime::TAbsoluteTime>("0/1/1");
  init_from_string<libtime::TAbsoluteTime>("70/1/1");
  init_from_string<libtime::TAbsoluteTime>("60/1/1");
  init_from_string<libtime::TAbsoluteTime>("160/1/1");
  init_from_string<libtime::TAbsoluteTime>("1978/1/1");
  init_from_string<libtime::TAbsoluteTime>("1978/13/1");
  init_from_string<libtime::TAbsoluteTime>("1978/1/2/3/4/5/6/7/8/9");

  std::cout << endl;
  Ainit_from_value(2000,1,1);
  Ainit_from_value(000,1,50);
  Ainit_from_value(000,1,50,12,23,34,45,56);

  std::cout << "\nTesting TRelativeTime"
            << "\n---------------------" << endl;
  init_from_string<libtime::TRelativeTime>("0");
  init_from_string<libtime::TRelativeTime>("8");
  init_from_string<libtime::TRelativeTime>("123.23.34.45.56789");
  init_from_string<libtime::TRelativeTime>("123.2345.34567.45678.56789");
  init_from_seconds(1.e-6);
  init_from_seconds(1.e-3);
  init_from_seconds(1.);
  init_from_seconds(60.);
  init_from_seconds(3600.);
  init_from_seconds(86400.);
  init_from_seconds(123.345763);
  init_from_seconds(871323.345763);

  std::cout << endl;
  Rinit_from_value(0);
  Rinit_from_value(2000,1,1);
  Rinit_from_value(0,24,60,60,1000,1000);
  Rinit_from_value(00,0,0,0,0,99999999L);

// junk:
  libtime::TRelativeTime Q(600,20,10,30,40,10);
  libtime::TRelativeTime OneSecond(0,0,0,1);
  libtime::TAbsoluteTime Now(2000,9,12,16,12);
  libtime::TAbsoluteTime Then(2000,10,2,10,0);
  cout << string(Q) << endl;
  cout << string(OneSecond) << endl;
  cout << string(Q%OneSecond) << endl;
  long int numbo=Q/OneSecond;
  cout << Q/OneSecond << endl;
  cout << string(OneSecond*numbo) << endl;
  numbo=(Now-Then)/OneSecond;
  cout << numbo << " seconds from "
       << string(Now) << endl << " to " << string(Then) << endl;
  Q=numbo*OneSecond;
  cout << "these are " << string(Q) << endl << " and lead to " 
       << string(Now+Q) << endl << " from " << string(Now) << endl;

  std::cout << "current time: " << libtime::now().timestring() << std::endl;
  std::cout << "current time (UTC): " << libtime::utc().timestring() << std::endl;

  {
    std::cout << "hierarchical strings:" << std::endl;
    libtime::TAbsoluteTime Atime(2000,9,12,16,12,34,56,78);
    libtime::TRelativeTime Rtime(514,16,12,34,56,78);
    std::cout << Atime.timestring() << std::endl;
    std::cout << Atime.hierarchicalstring() << std::endl;
    std::cout << Rtime.timestring() << std::endl;
    std::cout << Rtime.hierarchicalstring() << std::endl;
  }

  std::cout << "\nSystematically test derived units"
            << "\n---------------------------------" << endl;
  PRINTVALUE( Days(25.5) );
  PRINTVALUE( Hours(25.5) );
  PRINTVALUE( Minutes(25.5) );
  PRINTVALUE( Seconds(25.5) );
  PRINTVALUE( Milliseconds(25.5) );
  PRINTVALUE( Microseconds(25.5) );

  PRINTVALUE( Days(6) );
  PRINTVALUE( Hours(6) );
  PRINTVALUE( Minutes(6) );
  PRINTVALUE( Seconds(6) );
  PRINTVALUE( Milliseconds(6) );
  PRINTVALUE( Microseconds(6) );

  PRINTVALUE( Days(180) );
  PRINTVALUE( Hours(180) );
  PRINTVALUE( Minutes(180) );
  PRINTVALUE( Seconds(180) );
  PRINTVALUE( Milliseconds(180) );
  PRINTVALUE( Microseconds(180) );

  PRINTVALUE( Days(22,22,22,22,22,22) );
  PRINTVALUE( Hours(22,22,22,22,22) );
  PRINTVALUE( Minutes(22,22,22,22) );
  PRINTVALUE( Seconds(22,22,22) );
  PRINTVALUE( Milliseconds(22,22) );
  PRINTVALUE( Microseconds(22) );

  PRINTVALUE( Hours(6) );
  PRINTVALUE( 118*Days()+4*Hours()+10*Minutes()+Milliseconds(25.51) );
  PRINTVALUE( Minutes(34.123456) );
  PRINTVALUE( Minutes(34,123,456) );
  PRINTVALUE( Seconds(34.123456) );
  PRINTVALUE( Seconds(34,123,456) );
  PRINTVALUE( Seconds(90072) );
  PRINTVALUE( 2.5*Days() );
  PRINTVALUE( Days(2.5) );
  PRINTVALUE( Seconds(2.5*time2double(Days())) );

  libtime::TAbsoluteTime nowtime=libtime::now();
  PRINTVALUE( nowtime.timestring("%Y") );
  PRINTVALUE( nowtime.timestring("/basedir/subdir/%Y/%m/%Y%m%d.dat") );
  PRINTVALUE( libtime::now().timestring("%H:%M:%S") );
  PRINTVALUE( libtime::now().timestring("time is %c") );
  PRINTVALUE( libtime::utc().timestring("time is %c") );
}

/* ----- END OF testtime++.cc ----- */
