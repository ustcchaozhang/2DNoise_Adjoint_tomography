/* this is <libtime++.h>
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) by Thomas Forbriger (IfG Stuttgart)
 *
 * interface of class libtime++
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
 *    09/08/2000   V1.0   Thomas Forbriger
 *    10/08/2000   V1.1   started with operators
 *    06/09/2000   V1.2   finished operators and rearranged base class
 *                        constructors
 *    12/09/2000   V1.3   added absolute - absolute --> relative
 *                        implemented non-member multiplication
 *    22/12/2000   V1.4   changed namespace time to libtime (resolved conflict
 *                        with system time library)
 *    22/12/2003   V1.5   added function now()
 *    22/12/2003   V1.6   whooo - was a bit sloppy - corrected some errors
 *    27/12/2003   V1.7   added member function of TAbsoluteTime to
 *                        set doy explicitely
 *    13/01/2004   V1.8   added constructor from double for TRelativeTime
 *    02/02/2004   V1.9   constructor from double for TRelativeTime
 *                        was highly ambiguous
 *                        move conversion to doubel2time() function
 *    15/07/2005   V1.10  print usage information
 *    28/04/2006   V1.11  - we need a forward declaration for TRelativeTime
 *                        - provide virtual destructor for Exception class
 *    17/12/2007   V1.12  replace long int by typedef timeint
 *    25/11/2008   V1.13  added function utc()
 *    12/12/2008   V1.14  added hierarchical time strings
 *    20/02/2012   V1.15  
 *                        - implement new classes for shorter time intervals
 *                        - use const char*
 *    16/12/2012   V1.16  implemented strftime(3) formatted string for
 *                        TAbsoluteTime
 *
 * ============================================================================
 */
 
#ifndef TF_LIBTIME_H_
#define TF_LIBTIME_H_ "version V1.16"
 
#include <string>
#include "libtime.h"

using std::string;

namespace libtime {

/*! \mainpage
 *
 * \author Thomas Forbriger
 * \since 1997, 2012
 *
 * This library supports numerical operations for dates and times with integer
 * precision down to microseconds.
 * Time comes in two different flavours:
 * -# absolute time (like today at 10 o'clock) as supported by
 *    libtime::TAbsoluteTime
 * -# relative times (like the time span between now and christmas) as
 *    supported by libtime::TRelativeTime
 * For time differences (libtime::TRelativeTime) only positive values are
 * accepted. 
 * Accidentally the library appears to handle negative value too.
 * However, since the underlying Fortran code is designed to handle positive
 * values only, functions should throw an exception upon negative values being
 * passed.
 *
 * The modules in the library support operations like:
 * - calculate the number of samples in a time window
 * - calculate the expected time and date for the first sample in the next
 *   data block
 * - calculate the time gap between two data blocks
 * - calculate the the index of the sample nearest to a given time
 * - calculate the residual time between a given time and the nearest sample
 *
 * The library provides interfaces in Fortran, C, C++. 
 * Unfortunately handling of leapseconds is not yet supported.
 *
 * \sa example++.cc, tests/libtime++.cc
 */

/*! integer type used in libtime functions */
using time_kernel::timeint;

/*!
 * TBaseClassTime
 * ==============
 *
 * This is a class definition holding common parts od relative times and
 * absolute times. It's constructor is protected as it should not be
 * instantiated. It's an abstract class.
 */
class TBaseClassTime {
/*S*/
// member functions common to both flavours of time
  public:
    std::string timestring() const;
    timeint hour() const;
    timeint minute() const;
    timeint second() const;
    timeint milsec() const;
    timeint micsec() const;
    double float_second() const;

    operator time_kernel::time_Ts() const;
    operator std::string() const;
/*E*/
// make constructors and member protected
  protected:
    TBaseClassTime(const std::string &Itimestring);
    TBaseClassTime(const char *Itimestring);
    TBaseClassTime(const time_kernel::time_Ts &Itime_Ts);
    TBaseClassTime(const timeint &year, const timeint &doy,
                   const timeint &hour=0,
                   const timeint &minute=0, const timeint &second=0,
                   const timeint &milsec=0, const timeint &micsec=0);
    void string_read(const std::string &timestring);
    void char_read(const char *timestring);
    // well, there is no need to make norm accessible to public, as we will
    // always norm the structure ourselfs
    void norm();
    // here we hold the current values
    time_kernel::time_Ts Mtime_Ts;
};

/*S*/

//! forward declaration of TRelativeTime
class TRelativeTime;
  
//! class to contain absolute times
class TAbsoluteTime: public TBaseClassTime {
  friend class TRelativeTime;
  public:
    TAbsoluteTime(const std::string &Itimestring);
    TAbsoluteTime(const char *Itimestring);
    TAbsoluteTime(const time_kernel::time_Ts &Itime_Ts);
    TAbsoluteTime(const timeint &year=2000, const timeint &month=1,
                  const timeint &day=1, const timeint &hour=0,
                  const timeint &minute=0, const timeint &second=0,
                  const timeint &milsec=0, const timeint &micsec=0);

    void setdoy(const timeint &doy);
    void setdoy(const timeint &day, const timeint &month);
    void setdate(const timeint &day, const timeint &month);
    void getdate(timeint &day, timeint &month) const;
    bool isleapyear() const;

    timeint year() const;
    timeint doy() const;
    timeint month() const;
    timeint day() const;
    std::string hierarchicalstring() const;

    bool operator==(const TAbsoluteTime &A) const;
    bool operator!=(const TAbsoluteTime &A) const;
    bool operator<=(const TAbsoluteTime &A) const;
    bool operator>=(const TAbsoluteTime &A) const;
    bool operator< (const TAbsoluteTime &A) const;
    bool operator> (const TAbsoluteTime &A) const;

    TAbsoluteTime &operator+=(const TRelativeTime &A);
    TAbsoluteTime &operator-=(const TRelativeTime &A);
    TAbsoluteTime  operator+ (const TRelativeTime &A) const;
    TAbsoluteTime  operator- (const TRelativeTime &A) const;
    TRelativeTime  operator- (const TAbsoluteTime &A) const;

    TAbsoluteTime &operator= (const time_kernel::time_Ts &A);
    TAbsoluteTime &operator= (const std::string &timestring);
    TAbsoluteTime &operator= (const char *timestring);

    using TBaseClassTime::timestring;
    std::string timestring(const std::string& format) const;
};

/*! \brief class to contain relative times
 *
 * For time differences only positive values are accepted. 
 * Accidentally the library appears to handle negative value too.
 * However, since the underlying Fortran code is designed to handle positive
 * values only, functions should throw an exception upon negative values being
 * passed.
 */
class TRelativeTime: public TBaseClassTime {
  friend class TAbsoluteTime;
  public:
    TRelativeTime(const std::string &Itimestring);
    TRelativeTime(const char *Itimestring);
    TRelativeTime(const time_kernel::time_Ts &Itime_Ts);
    TRelativeTime(const int &days=0, const int &hour=0,
                  const int &minute=0, const int &second=0,
                  const int &milsec=0, const int &micsec=0);

    timeint days() const;
    std::string hierarchicalstring() const;
    void nfit(const TRelativeTime &delta, timeint &n, TRelativeTime &full)
      const;
    void div(const timeint &n, TRelativeTime &frac, timeint &rest)
      const;

    bool operator==(const TRelativeTime &A) const;
    bool operator!=(const TRelativeTime &A) const;
    bool operator<=(const TRelativeTime &A) const;
    bool operator>=(const TRelativeTime &A) const;
    bool operator< (const TRelativeTime &A) const;
    bool operator> (const TRelativeTime &A) const;

    TRelativeTime &operator+=(const TRelativeTime &A);
    TRelativeTime &operator-=(const TRelativeTime &A);
    TRelativeTime  operator+ (const TRelativeTime &A) const;
    TRelativeTime  operator- (const TRelativeTime &A) const;

    TAbsoluteTime  operator+ (TAbsoluteTime A) const;
    TAbsoluteTime  operator- (TAbsoluteTime A) const;

    TRelativeTime &operator*=(const timeint &n);
    TRelativeTime &operator/=(const timeint &n);
    TRelativeTime &operator%=(const timeint &n);

    TRelativeTime  operator* (const timeint &n) const;
    TRelativeTime  operator/ (const timeint &n) const;
    TRelativeTime  operator% (const timeint &n) const;

    timeint       operator/ (const TRelativeTime &A) const;
    TRelativeTime  operator% (const TRelativeTime &A) const;

    TRelativeTime &operator= (const time_kernel::time_Ts &A);
    TRelativeTime &operator= (const std::string &timestring);
    TRelativeTime &operator= (const char *timestring);
};

TRelativeTime operator* (const time_kernel::timeint &n, const TRelativeTime &A);

/*E*/

/*
 * Here we go for inline functions
 * ===============================
 */

/*
 * TBaseClassTime
 * --------------
 */
inline TBaseClassTime::TBaseClassTime(const std::string &Itimestring)
  { string_read(Itimestring); }
inline TBaseClassTime::TBaseClassTime(const char *Itimestring)
  { char_read(Itimestring); }

inline std::string TBaseClassTime::timestring() const
  { return(std::string(time_kernel::time_sprint(Mtime_Ts))); }

inline void TBaseClassTime::norm()
  { time_kernel::time_norm(&Mtime_Ts); }

inline TBaseClassTime::TBaseClassTime(const time_kernel::time_Ts &Itime_Ts):
  Mtime_Ts(Itime_Ts) { }

inline TBaseClassTime::TBaseClassTime(const timeint &year,
          const timeint &doy, const timeint &hour,
          const timeint &minute, const timeint &second,
          const timeint &milsec, const timeint &micsec)
{
  Mtime_Ts.year   =year;
  Mtime_Ts.doy    =doy;
  Mtime_Ts.hour   =hour;
  Mtime_Ts.minute =minute;
  Mtime_Ts.second =second;
  Mtime_Ts.milsec =milsec;
  Mtime_Ts.micsec =micsec;
}

inline timeint TBaseClassTime::hour()   const { return(Mtime_Ts.hour); }
inline timeint TBaseClassTime::minute() const { return(Mtime_Ts.minute); }
inline timeint TBaseClassTime::second() const { return(Mtime_Ts.second); }
inline timeint TBaseClassTime::milsec() const { return(Mtime_Ts.milsec); }
inline timeint TBaseClassTime::micsec() const { return(Mtime_Ts.micsec); }
inline double TBaseClassTime::float_second() const
  { return(Mtime_Ts.second+1.e-3*Mtime_Ts.milsec+1.e-6*Mtime_Ts.micsec); }

inline TBaseClassTime::operator time_kernel::time_Ts() const 
  { return(Mtime_Ts); }
inline TBaseClassTime::operator std::string() const { return(timestring()); }

/*
 * TAbsoluteTime
 * -------------
 */
inline TAbsoluteTime::TAbsoluteTime(const std::string &Itimestring):
  TBaseClassTime(Itimestring) { time_kernel::time_finish(&Mtime_Ts); }
inline TAbsoluteTime::TAbsoluteTime(const char *Itimestring):
  TBaseClassTime(Itimestring) { time_kernel::time_finish(&Mtime_Ts); }

inline TAbsoluteTime::TAbsoluteTime(const time_kernel::time_Ts &Itime_Ts):
  TBaseClassTime(Itime_Ts) 
  { time_kernel::time_finish(&Mtime_Ts); }

inline TAbsoluteTime::TAbsoluteTime(const timeint &year, 
           const timeint &month, const timeint &day, const timeint &hour, 
           const timeint &minute, const timeint &second,
           const timeint &milsec, const timeint &micsec):
  TBaseClassTime(year, 1, hour, minute, second, milsec, micsec)
  { time_kernel::time_fullyear(&Mtime_Ts.year); setdate(day, month); norm(); }

inline void TAbsoluteTime::setdoy(const timeint &doy)
  { Mtime_Ts.doy=doy; this->norm(); }

inline void TAbsoluteTime::setdoy(const timeint &day, const timeint &month)
  { time_kernel::time_setdoy(day, month, &Mtime_Ts); }

inline void TAbsoluteTime::setdate(const timeint &day, const timeint &month)
  { time_kernel::time_setdoy(day, month, &Mtime_Ts); }

inline void TAbsoluteTime::getdate(timeint &day, timeint &month) const
  { time_kernel::time_getdate(&day, &month, Mtime_Ts); }

inline bool TAbsoluteTime::isleapyear() const
  { return(time_kernel::time_isleapyear(Mtime_Ts.year) == TIME_ISLEAP); }

inline timeint TAbsoluteTime::year()  const { return(Mtime_Ts.year); }
inline timeint TAbsoluteTime::doy()   const { return(Mtime_Ts.doy); }
inline timeint TAbsoluteTime::month() const
  { timeint day, month; getdate(day, month); return(month); } 
inline timeint TAbsoluteTime::day() const 
  { timeint day, month; getdate(day, month); return(day); } 

inline bool TAbsoluteTime::operator==(const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)==0); }
inline bool TAbsoluteTime::operator!=(const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)!=0); }
inline bool TAbsoluteTime::operator<=(const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)<=0); }
inline bool TAbsoluteTime::operator>=(const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)>=0); }
inline bool TAbsoluteTime::operator< (const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)< 0); }
inline bool TAbsoluteTime::operator> (const TAbsoluteTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)> 0); }

inline TAbsoluteTime &TAbsoluteTime::operator+=(const TRelativeTime &A)
{ 
  TAbsoluteTime B(Mtime_Ts); 
  time_kernel::time_add(B.Mtime_Ts, A.Mtime_Ts, &Mtime_Ts);
  return(*this);
}
inline TAbsoluteTime &TAbsoluteTime::operator-=(const TRelativeTime &A)
{ 
  TAbsoluteTime B(Mtime_Ts); 
  time_kernel::time_sub(B.Mtime_Ts, A.Mtime_Ts, &Mtime_Ts);
  return(*this);
}

inline TAbsoluteTime TAbsoluteTime::operator+ (const TRelativeTime &A) const
  { TAbsoluteTime B(Mtime_Ts); return(B+=A); }
inline TAbsoluteTime TAbsoluteTime::operator- (const TRelativeTime &A) const
  { TAbsoluteTime B(Mtime_Ts); return(B-=A); }
inline TRelativeTime TAbsoluteTime::operator- (const TAbsoluteTime &A) const
{
    TRelativeTime B;
    time_kernel::time_sub(Mtime_Ts, A.Mtime_Ts, &B.Mtime_Ts);
    return(B); 
}

inline TAbsoluteTime &TAbsoluteTime::operator= (const time_kernel::time_Ts &A)
  { Mtime_Ts=A; time_kernel::time_finish(&Mtime_Ts); return(*this); }
inline TAbsoluteTime &TAbsoluteTime::operator= (const std::string &timestring)
  { string_read(timestring); return(*this); }
inline TAbsoluteTime &TAbsoluteTime::operator= (const char *timestring)
  { char_read(timestring); return(*this); }
  
/*
 * TRelativeTime
 * -------------
 */
inline TRelativeTime::TRelativeTime(const string &Itimestring):
  TBaseClassTime("0/0/" + Itimestring) { }
inline TRelativeTime::TRelativeTime(const char *Itimestring):
  TBaseClassTime("0/0/" + std::string(Itimestring)) { }

inline TRelativeTime::TRelativeTime(const time_kernel::time_Ts &Itime_Ts):
  TBaseClassTime(Itime_Ts) 
  { Mtime_Ts.year=0; norm(); }

inline TRelativeTime::TRelativeTime(const int &days, 
           const int &hour, 
           const int &minute, const int &second,
           const int &milsec, const int &micsec):
  TBaseClassTime(0, days, hour, minute, second, milsec, micsec)
  { norm(); }

inline timeint TRelativeTime::days()  const { return(Mtime_Ts.doy); }

inline void TRelativeTime::nfit(const TRelativeTime &delta, timeint &n, 
                 TRelativeTime &full) const
  { time_kernel::time_nfit(Mtime_Ts, delta.Mtime_Ts, &n, &full.Mtime_Ts); }

inline void TRelativeTime::div(const timeint &n, TRelativeTime &frac, 
                timeint &rest) const
  { time_kernel::time_div(Mtime_Ts, &frac.Mtime_Ts, n, &rest); }


inline bool TRelativeTime::operator==(const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)==0); }
inline bool TRelativeTime::operator!=(const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)!=0); }
inline bool TRelativeTime::operator<=(const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)<=0); }
inline bool TRelativeTime::operator>=(const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)>=0); }
inline bool TRelativeTime::operator< (const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)< 0); }
inline bool TRelativeTime::operator> (const TRelativeTime &A) const
  { return(time_kernel::time_compare(Mtime_Ts,A.Mtime_Ts)> 0); }

inline TRelativeTime &TRelativeTime::operator+=(const TRelativeTime &A)
{ 
  TRelativeTime B(Mtime_Ts); 
  time_kernel::time_add(B.Mtime_Ts, A.Mtime_Ts, &Mtime_Ts);
  return(*this);
}
inline TRelativeTime &TRelativeTime::operator-=(const TRelativeTime &A)
{ 
  TRelativeTime B(Mtime_Ts); 
  time_kernel::time_sub(B.Mtime_Ts, A.Mtime_Ts, &Mtime_Ts);
  return(*this);
}

inline TRelativeTime TRelativeTime::operator+ (const TRelativeTime &A) const
  { TRelativeTime B(Mtime_Ts); return(B+=A); }
inline TRelativeTime TRelativeTime::operator- (const TRelativeTime &A) const
  { TRelativeTime B(Mtime_Ts); return(B-=A); }

inline TAbsoluteTime TRelativeTime::operator+ (TAbsoluteTime A) const
  { TRelativeTime B(Mtime_Ts); return(A+=B); }
inline TAbsoluteTime TRelativeTime::operator- (TAbsoluteTime A) const
  { TRelativeTime B(Mtime_Ts); return(A-=B); }
  
inline TRelativeTime &TRelativeTime::operator*=(const timeint &n)
{ 
  time_kernel::time_Ts A(Mtime_Ts);
  time_kernel::time_mul(A, &Mtime_Ts, n); 
  return(*this);
}
inline TRelativeTime &TRelativeTime::operator/=(const timeint &n)
{ 
  time_kernel::time_Ts B(Mtime_Ts);
  timeint rest;
  time_kernel::time_div(B, &Mtime_Ts, n, &rest); 
  return(*this);
}
inline TRelativeTime &TRelativeTime::operator%=(const timeint &n)
{ 
  time_kernel::time_Ts B(Mtime_Ts);
  timeint rest;
  time_kernel::time_div(B, &Mtime_Ts, n, &rest); 
  time_kernel::time_clear(&Mtime_Ts);
  Mtime_Ts.micsec=rest;
  time_kernel::time_norm(&Mtime_Ts);
  return(*this);
}

inline TRelativeTime TRelativeTime::operator* (const timeint &n) const
  { TRelativeTime B(Mtime_Ts); return(B*=n); }
inline TRelativeTime TRelativeTime::operator/ (const timeint &n) const
  { TRelativeTime B(Mtime_Ts); return(B/=n); }
inline TRelativeTime TRelativeTime::operator% (const timeint &n) const
  { TRelativeTime B(Mtime_Ts); return(B%=n); }

inline TRelativeTime operator* (const timeint &n, const TRelativeTime &A)
  { TRelativeTime B(A); return(B*=n); }

inline timeint TRelativeTime::operator/ (const TRelativeTime &A) const
{
  timeint n;
  time_kernel::time_Ts full;
  time_kernel::time_nfit(Mtime_Ts, A.Mtime_Ts, &n, &full);
  return(n);
}
inline TRelativeTime TRelativeTime::operator% (const TRelativeTime &A) const
{
  timeint n;
  time_kernel::time_Ts full, rest;
  time_kernel::time_nfit(Mtime_Ts, A.Mtime_Ts, &n, &full);
  time_kernel::time_sub(Mtime_Ts, full, &rest);
  return(TRelativeTime(rest));
}

inline TRelativeTime &TRelativeTime::operator= (const time_kernel::time_Ts &A)
  { Mtime_Ts=A; Mtime_Ts.year=0; norm(); return(*this); }
inline TRelativeTime &TRelativeTime::operator= (const std::string &timestring)
  { string_read("0/0/" + timestring); return(*this); }
inline TRelativeTime &TRelativeTime::operator= (const char *timestring)
  { string_read("0/0/" + std::string(timestring)); return(*this); }

/*======================================================================*/
/*S*/

/*
 * some functions
 * --------------
 */
//! return system time
TAbsoluteTime now();
//! return system time in UTC
TAbsoluteTime utc();
//! convert relative time to seconds
double time2double(const TRelativeTime&);
//! convert seconds to relative time
TRelativeTime double2time(const double& seconds);
//! dump internal representation of time (for debugging purposes)
void dump(std::ostream& os, const TBaseClassTime& t);
/*E*/ 
  
/*======================================================================*/
/*S*/

/*! \brief provide a convenient way to specify time intervals in the order of
 * one day
 */
class Days: public TRelativeTime {
  public:
    Days(const timeint &day=1, const timeint &hour=0,
         const timeint &minute=0, const timeint &second=0,
         const timeint &milsec=0, const timeint &micsec=0)
      : TRelativeTime(day, hour, minute, second, milsec, micsec) { }
    Days(const double &days)
      : TRelativeTime(double2time(60.*60.*24.*days)) { }
}; // class Days

/*! \brief provide a convenient way to specify time intervals in the order of
 * one hour
 */
class Hours: public TRelativeTime {
  public:
    Hours(const timeint &hour=1,
          const timeint &minute=0, const timeint &second=0,
          const timeint &milsec=0, const timeint &micsec=0)
      : TRelativeTime(0, hour, minute, second, milsec, micsec) { }
    Hours(const double &hours)
      : TRelativeTime(double2time(60.*60.*hours)) { }
}; // class Hours

/*! \brief provide a convenient way to specify time intervals in the order of
 * one minute
 */
class Minutes: public TRelativeTime {
  public:
    Minutes(const timeint &minute=1, const timeint &second=0,
            const timeint &milsec=0, const timeint &micsec=0)
      : TRelativeTime(0, 0, minute, second, milsec, micsec) { }
    Minutes(const double &minutes)
      : TRelativeTime(double2time(60.*minutes)) { }
}; // class Minutes

/*! \brief provide a convenient way to specify time intervals in the order of
 * one second
 */
class Seconds: public TRelativeTime {
  public:
    Seconds(const timeint &second=1,
            const timeint &milsec=0, const timeint &micsec=0)
      : TRelativeTime(0, 0, 0, second, milsec, micsec) { }
    Seconds(const double &seconds)
      : TRelativeTime(double2time(seconds)) { }
}; // class Seconds

/*! \brief provide a convenient way to specify time intervals in the order of
 * one millisecond
 */
class Milliseconds: public TRelativeTime {
  public:
    Milliseconds(const timeint &milsec=1, const timeint &micsec=0)
      : TRelativeTime(0, 0, 0, 0, milsec, micsec) { }
    Milliseconds(const double &milliseconds)
      : TRelativeTime(double2time(1.e-3*milliseconds)) { }
}; // class Milliseconds

/*! \brief provide a convenient way to specify time intervals in the order of
 * one microsecond
 */
class Microseconds: public TRelativeTime {
  public:
    Microseconds(const timeint &micsec=1)
      : TRelativeTime(0, 0, 0, 0, 0, micsec) { }
    Microseconds(const double &microseconds)
      : TRelativeTime(double2time(1.e-6*microseconds)) { }
}; // class Microseconds
  
/*E*/ 

/*======================================================================*/
/*S*/

  //! time range
  class TRange {
    public:
      TRange(): Mbegin(now()), Mend(now()) { }
      TRange(const TAbsoluteTime& begin,
             const TAbsoluteTime& end);
      TAbsoluteTime begin() const { return(Mbegin); }
      TAbsoluteTime end() const { return(Mend); }
      TRelativeTime size() const { return(Mend-Mbegin); }
      bool includes(const TAbsoluteTime&) const;
      bool includes(const TRange&) const;
      bool overlaps(const TRange&) const;
      TRange largestcommon(const TRange&) const;
      TRange smallestcommon(const TRange&) const;
      TRange& delay(const TRelativeTime&);
      TRange& advance(const TRelativeTime&);
      TRange delayedby(const TRelativeTime&) const;
      TRange advancedby(const TRelativeTime&) const;
      void expand(const TRange&);
      void shrink(const TRange&);
    private:
      TAbsoluteTime Mbegin;
      TAbsoluteTime Mend;
  }; // class TRange

/*E*/ 

/*======================================================================*/
// error handling and exception class
// ----------------------------------

/*S*/
  struct Warning {
    public:
      static bool suppress_normal;
      static bool suppress_year;
      static bool suppress_any;
  }; // class Warning

  class Exception 
  {
    public:
      //! Creates exception with no explaining comments
      Exception();
      //! Creates an exception with an explanation message
      Exception(const char* message);
      //! Creates an exception with message and failed assertion
      Exception(const char* message, 
                const char* condition);
      //! Create with message, failed assertion, and code position
      Exception(const char* message, 
                const char* file,
                const int& line,
                const char* condition);
      //! Create with message and code position
      Exception(const char* message, 
                const char* file,
                const int& line);
      //! provide explicit virtual destructor
      virtual ~Exception() { }
      //! Screen report
      virtual void report() const;
      //! Issue a screen report on construction of exception
      static void report_on_construct();
      //! Issue NO screen report on construction of exception
      static void dont_report_on_construct();
    protected:
      //! Screen report
      void base_report() const;
    private:
      //! Shall we print to cerr at construction time?
      static bool Mreport_on_construct;
      //! pointer to message string
      const char* Mmessage;
      //! pointer to file name string
      const char* Mfile;
      //! pointer to line number in source file
      const int& Mline;
      //! pointer to assertion condition text string
      const char* Mcondition;
  }; // class Exception

/*======================================================================*/
//
// preprocessor macros
// ===================

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 * \param E exception class to throw
 */
#define libtime_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 */
#define libtime_assert(C,M) libtime_Xassert( C , M , libtime::Exception )

/*! \brief Abort and give a message
 *
 * \ingroup group_error
 * \param M message of type char*
 * \param E exception class to throw
 */
#define libtime_abort(M) \
  throw( libtime::Exception ( M , __FILE__, __LINE__ )) 
/*E*/ 
  
/*======================================================================*/
/*S*/

/*
 * some string constants
 * --------------
 */
//! print information on time format
extern const char usage_time_format_string[];
/*E*/ 

} // namespace libtime
#endif // TF_LIBTIME_H_
 
/* ----- END OF libtime++.h ----- */
