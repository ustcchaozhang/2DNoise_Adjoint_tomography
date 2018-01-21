/*! \file pdasread.h
 * \brief read pdas file (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read pdas file (prototypes)
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
 *  - 30/03/2004   V1.0   Thomas Forbriger
 *  - 10/09/2004   V1.1   support long format
 *  - 20/10/2004   V1.2   support scaling for floating point types
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_PDASREAD_H_VERSION

#define DATRW_PDASREAD_H_VERSION \
  "DATRW_PDASREAD_H   V1.2   "

#include<vector>
#include<istream>
#include<datrwxx/pdasflags.h>
#include<datrwxx/error.h>
#include<libtime++.h>

namespace datrw {

  /*! \brief all functions, classes etc. to read PDAS data.
   *
   * \defgroup group_pdas Reading module for: PDAS data
   */

  /*! \brief all functions, classes etc. to read PDAS data.
   *
   * \ingroup group_pdas
   */
  namespace pdas {

    //! struct to hold content of header line
    struct HeaderLine {
      std::string line, token, value;
    }; // struct HeaderLine

    //! struct to hold complete header
    struct Header {
      std::string dataset, version, signal, date, time, interval;
      std::string vertunits, horzunits, comment;
      Etype type;
      std::vector<std::string> lines;
    }; // struct Header

    typedef int Tvalue;
    typedef std::vector<Tvalue> Tdata;

    //! function to read the file header line
    HeaderLine readline(std::istream& is, const bool& verbose=false);
    //! function to read the file header
    Header readheader(std::istream& is, const bool& verbose=false);
    /*! function to read the file data
     * passes vector by reference to avoid deep copy
     */
    void readdata(std::istream& is, Tdata& data, const Etype& type=FtypeINT);
    //! function to skip the file data but count the samples
    int countdata(std::istream& is, const Etype& type=FtypeINT);
    //! function to read one sample
    Tvalue readsample(std::istream& is, const Etype& type=FtypeINT);

    //! function to convert data
    template<class C>
    C convert(const Tdata& data)
    {
      int nsamples=data.size();
      C retval(nsamples);
      typedef typename C::Tvalue Toutvalue;
      for (int i=0; i<nsamples; ++i) 
      { 
        Tvalue sample=data[i];
        retval(i)=Toutvalue(sample); 
      }
      return(retval);
    }

    //! function to scale data
    template<class C>
    C convertandscale(const Tdata& data, const Etype& type)
    {
      long unsigned int factor;
      if (type==FtypeINT)
      { factor=0x8000; }
      else if (type==FtypeLONG)
      { factor=0x80000000; }
      else if (type==FtypeGAINRANGED)
      { factor=0x10000000; }
      else
      { DATRW_abort("illegal data type"); }
      typedef typename C::Tvalue Tcontfactor;
      Tcontfactor cfactor=Tcontfactor(factor);
      return(datrw::pdas::convert<C>(data) / cfactor);
    }

    //! function to print online help
    void help(std::ostream& os=std::cout);

  } // namespace pdas

} // namespace datrw

#endif // DATRW_PDASREAD_H_VERSION (includeguard)

/* ----- END OF pdasread.h ----- */
