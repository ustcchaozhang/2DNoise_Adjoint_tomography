/*! \file gsexx.h
 * \brief GSE++ library: read and write GSE waveform data (prototypes).
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 16/03/2002
 * 
 * GSE++ library: read and write GSE waveform data (prototypes)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 16/03/2002   V1.0   Thomas Forbriger
 *  - 26/12/2003   V1.1   final checksum is defined to be absolute value
 *  - 28/04/2006   V1.2   classes that use virtual functions require the
 *                        definition of an explicit virtual destructor
 *  - 24/07/2006   V1.3   provide milliseconds precision
 *                        \b !! interface has changed:
 *                              field Fseconds is removed from TWID2
 *  - 11/11/2009   V1.4  
 *                        - abs is not overloaded for int in header cmath
 *                        - \b !! interface has changed:
 *                              Terror now uses string class
 *  - 06/12/2011   V1.5   introduced some code for debugging second
 *                        differences
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_GSEXX_H_VERSION

#define TF_GSEXX_H_VERSION \
  "TF_GSEXX_H   V1.5"
#define TF_GSEXX_H_CVSID \
  "$Id$"

// #include <libtime++.h>
#include<string>
#include<iostream>

//! All stuff defined by the GSE2 standard.
namespace GSE2 {

/*----------------------------------------------------------------------*/
/*
 * exception base class
 * --------------------
 */

//! Base class for all exceptions in this module
class Terror {
  public: 
    //! pass error message
    Terror(const std::string& message): Mmessage(message) { 
      if (!silent) { std::cerr << Mmessage << std::endl; }
    }
    //! return error message
    const std::string& message() const { return(Mmessage); }
    //! be silent?
    static bool silent;
  private:
    //! error message
    std::string Mmessage;
}; // Terror

/*======================================================================*/

//! All waveform related stuff.
namespace waveform {

//! All GSE2 waveform data is based on 4 byte integers.
typedef int intT;

//! Possible subformats of waveform data.
enum Esubformat 
{
  SF_INT,        //!< INT
  SF_CM6,        //!< CM6
  SF_CM8,        //!< CM8
  SF_AUT,        //!< AUT
  SF_AU6,        //!< AU6
  SF_AU8         //!< AU8
};

/*======================================================================*/
/*
 * all stuff for WID2 lines
 * ------------------------
 */

//! A class to hold and manage the WID2-line.
//! This is a struct - because it is a simple collection of fields
//! together with a few formatting functions
struct TWID2 {
  public:
    //! GSE line idetifier
    static const char* const GSEID;
    //! default constructor sets default values
    TWID2() { this->defaults(); }
    //! return subformat ID string
    std::string subformat() const;
    //! write the WID2 line
    std::string line() const;
    //! read a WID2 line from a stream
    void read(std::istream& is);
    //! set the values to defaults
    void defaults();
    //! set subformat from key string
    void setsubformat(const std::string& key);
    //! provide seconds in GSE specific floating point format
    double seconds() const;
    //! set seconds in GSE specific floating point format
    void seconds(const double& s);
  public:
    int                    Fyear;        //!< year of date
    int                    Fmonth;       //!< month of date
    int                    Fday;         //!< day of date
    int                    Fhour;        //!< hour of time
    int                    Fminute;      //!< minute of time
    // double                 Fseconds;     //!< seconds of time
    std::string            Fstation;     //!< Station code
    std::string            Fchannel;     //!< FDSN channel code
    std::string            Fauxid;       //!< Auxiliary identification code
    Esubformat             Fsubformat;   //!< GSE2 waveform subformat
    int                    Fsamps;       //!< number of samples
    double                 Fsamprate;    //!< sampling rate (Hz)
    double                 Fcalib;       //!< calibration factor
    double                 Fcalper;      //!< calibration reference period
    std::string            Finstype;     //!< instrument type
    double                 Fhang;        //!< horizontal orientation
    double                 Fvang;        //!< veritcal orientation
    //! This field is required to obtain millisecond precision
    int                    Fmilsec;
}; // struct TWID2

/*======================================================================*/
/*
 * all stuff for the GSE2.1 STA2 line
 * ----------------------------------
 */

//! A class to hold and manage the STA2-line.
class TSTA2 {
  public:
    //! GSE line idetifier
    static const char* const GSEID;
}; // class TSTA2

/*======================================================================*/
/*
 * all stuff for checksums
 * -----------------------
 */

//! A class for the cumulative calculation of checksums.
class TCHK2 {
    friend std::istream& operator>>(std::istream&, TCHK2& chk2);
  public:
    //! Default constructor.
    TCHK2(): Msum(0) { }
    //! Add a value to the checksum.
    void add(const intT& value);
    //! Return the checksum value.
    intT value() const { return(Msum > 0 ? Msum : -Msum); }
    //! write CHK2 line to string
    std::string write() const;
    //! read CHK2 line from istream
    void read(std::istream& is);
    //! GSE line idetifier
    static const char* const GSEID;
  private:
    //! Set value when read from file.
    void set(const intT& value) { Msum=value; }
    intT Msum;   //!< checksum value
}; // class TCHK2

/*======================================================================*/
/*
 * modules to apply and remove differnces must be declared here
 * ------------------------------------------------------------
 * the full stuff must be provided here, since we want to use the classes
 * as member types in the TDAT2 compression classes.
 */

//! Stuff to apply and remove first, second and higher differences.
namespace differences {

//! Elementary operator to calculate differences.
class Tapply_diff { 
  public:
    //! initialize (set previous value to zero)
    Tapply_diff(): Mxold(0) { }
    //! return difference to previous value and remember this
    int operator()(const intT& x) 
    { 
      intT xold=Mxold;
      Mxold=x;
      return(x-xold); 
    }
    //! return diff value on request (for debugging)
    intT previous_value() const { return(Mxold); }
  private:
    //! previous value in stream
    intT Mxold;
}; // Tapply_diff


//! Elementary operator to calculate the sum, which removes differences.
class Tremove_diff { 
  public:
    //! initialize previous value to zero
    Tremove_diff(): Mxold(0) { }
    //! integrate by adding this value to the previous returned
    intT operator()(const intT& x) 
    { return(Mxold=Mxold+x); }
    //! return diff value on request (for debugging)
    intT previous_value() const { return(Mxold); }
  private:
    //! previous returned value
    intT Mxold;
}; // Tremove_diff

//! Template to apply or remove differences of any order n.
template<int n, class OP>
class Tdiff_operator {
  public:
    //! apply elementary operator to result of operator of next lower order 
    intT operator()(const intT& val) { return(Mop(Mdiff(val))); }
    //! report status (for debugging)
    void report_status(std::ostream& os) const
    {
      Mdiff.report_status(os);
      os << "operator of order " << n 
        << " holds value: " << Mop.previous_value() << std:: endl;
    }
  private:
    //! elementary operator
    OP Mop;
    //! next lower order operator
    Tdiff_operator<n-1, OP> Mdiff;
}; // Tdiff_operator<n, OP>

//! \brief Template specialization for order zero.
template<class OP>
class Tdiff_operator<0, OP> {
  public:
    //! least order operator must copy value (for any elementary operation)
    intT operator()(const intT& val) { return(val); }
    //! report status (for debugging)
    void report_status(std::ostream& os) const
    {
      os << "operator of order 0 just returns values" << std::endl;
    }
}; // Tdiff_operator<0, OP>

} // namespace differences

//! Operator to apply first differences
typedef differences::Tdiff_operator<1, differences::Tapply_diff> 
  apply1stdiffT;
//! Operator to apply second differences
typedef differences::Tdiff_operator<2, differences::Tapply_diff> 
  apply2nddiffT;
//! Operator to remove first differences
typedef differences::Tdiff_operator<1, differences::Tremove_diff> 
  remove1stdiffT;
//! Operator to remove second differences
typedef differences::Tdiff_operator<2, differences::Tremove_diff> 
  remove2nddiffT;

/*======================================================================*/
/*
 * All stuff for DAT2 reading and writing
 * --------------------------------------
 */

//! Handle checksum and sample count
class TDAT2sum {
  public:
    //! return the checksum
    const TCHK2& checksum() const { return(Mchecksum); }
    //! return the number of samples read
    intT nread() const { return(Mnsamp); }
    //! return total number of samples to read
    intT msamp() const { return(Mmsamp); }
    //! return true if not all samples are processed
    bool hot() const { return((Mnsamp<Mmsamp)); }
    //! GSE line idetifier
    static const char* const GSEID;
  protected:
    //! only derived class should create an instance
    TDAT2sum(const intT& msamp): Mnsamp(0), Mmsamp(msamp) { }
    //! count a sample
    void add(const intT& value);
  private:
    //! The checksum is handled by the base class.
    TCHK2 Mchecksum;
    //! number of samples read
    intT Mnsamp;
    //! total number of samples to read
    intT Mmsamp;
}; // TDAT2sum

// inline add function
inline void TDAT2sum::add(const intT& value)
{
  Mchecksum.add(value);
  ++Mnsamp;
}

/*----------------------------------------------------------------------*/

//! Abstract base class for reading %GSE2 %waveform data.
class TDAT2read: public TDAT2sum
{
  public:
    //! provide explicit virtual destructor
    virtual ~TDAT2read() { }
    //! get another value from the stream
    intT operator()(std::istream& is);
  protected:
    //! constructor to be called from implementation class
    TDAT2read(const intT& msamp):
      TDAT2sum(msamp) { }
  private:
    //! get from stream: user must define
    virtual intT convert(std::istream& is) = 0;
  protected:
}; // TDAT2read

/*----------------------------------------------------------------------*/

//! Abstract base class for writing %GSE2 %waveform data.
class TDAT2write: public TDAT2sum
{
  public:
    //! provide explicit virtual destructor
    virtual ~TDAT2write() { }
    //! write another value to the stream
    std::string operator()(const intT& value);
  protected:
    //! constructor to be called from implementation class
    TDAT2write(const intT& msamp, const intT& linelength):
      TDAT2sum(msamp), Mlinelength(linelength), Mcpos(0) { }
  private:
    //! put to stream: user must define
    virtual std::string convert(const intT& value) = 0;
  protected:
    //! linelength in output file
    intT Mlinelength;
    //! character position in line
    intT Mcpos;
}; // TDAT2write

/*----------------------------------------------------------------------*/

//! Derived class for reading %CM6 subformat data.
class TDAT2readCM6: public TDAT2read
{
  public:
    //! provide explicit virtual destructor
    virtual ~TDAT2readCM6() { }
    //! constructor of implementation class
    TDAT2readCM6(const intT& msamp):
      TDAT2read(msamp) { }
  private:
    //! get from stream: user must define
    intT convert(std::istream& is);
  private:
    //! only compression formats apply differences.
    remove2nddiffT Mremovediff;
}; // TDAT2readCM6

/*----------------------------------------------------------------------*/

//! Derived class for writing %CM6 subformat data.
class TDAT2writeCM6:public TDAT2write
{
  public:
    //! provide explicit virtual destructor
    virtual ~TDAT2writeCM6() { }
    //! constructor of implementation class
    TDAT2writeCM6(const intT& msamp, const intT& linelength=80):
      TDAT2write(msamp, linelength) { }
  private:
    //! put to stream: user must define
    std::string convert(const intT& value);
  private:
    //! only compression formats apply differences.
    apply2nddiffT Mapplydiff;
}; // TDAT2writeCM6

/*======================================================================*/

} // namespace waveform

/*======================================================================*/
/*
 * GSE2 helper functions
 * ---------------------
 */

//! Check GSE identifier at beginning of line.
template<class C>
bool GSEIDmatch(const std::string& line)
{ return(line.substr(0,4)==std::string(C::GSEID)); }

} // namespace GSE2

#endif // TF_GSEXX_H_VERSION (includeguard)

/* ----- END OF gsexx.h ----- */
