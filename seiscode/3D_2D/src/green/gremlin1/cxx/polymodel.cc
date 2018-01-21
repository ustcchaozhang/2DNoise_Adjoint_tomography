/*! \file polymodel.cc
 * \brief polynomial subsurface model (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 30/12/2002
 * 
 * polynomial subsurface model (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 30/12/2002   V1.0   Thomas Forbriger
 *  - 03/01/2003   V1.1   
 *                        - version with means compiles
 *                        - reinvented value function
 *  - 04/01/2003   V1.2   correction in calculation of mean values
 *  - 09/02/2010   V1.3   moved initialization of model identifier
 * 
 * ============================================================================
 */
#define TF_POLYMODEL_CC_VERSION \
  "TF_POLYMODEL_CC   V1.3"
#define TF_POLYMODEL_CC_CVSID \
  "$Id$"

#include <gremlin1/polymodel.h>
#include <tfxx/error.h>
#include <aff/shaper.h>
#include <aff/slice.h>
#include <stdio.h>
#include <string>
#include <fstream>

namespace gremlin1 {

  typedef PolynomialModelFile::Tvalue Tvalue;
  typedef PolynomialModelFile::Tarray Tarray;

  //! define identifier
  const char PolynomialModelFile::modversion2[]="ModVersion 2";
  //! define static
  const int PolynomialModelFile::norder;
  //! define static
  const int PolynomialModelFile::nparameters;

  //! default constructor
  PolynomialModelFile::PolynomialModelFile():
    Mpara(norder,1,nparameters), Mdepth(aff::Shaper(0,1)),
    Mfollow(1,nparameters), Mnpol(1,nparameters),
    Mmeans(), Mmeans_are_valid(false)
    { 
      Mpara=0.;
      Mdepth=0.;
      Mfollow=false;
      Mnpol=1;
      clean_unused_and_check(); 
    }

  /*----------------------------------------------------------------------*/

  //! read from file
  PolynomialModelFile::PolynomialModelFile(const char* filename)
  {
    std::ifstream ifs(filename);
    read_from_stream(ifs);
  }

  /*----------------------------------------------------------------------*/
 
  //! check consistency
  void PolynomialModelFile::check_consistency() const
  {
    const char message[]="PolynomialModelFile: illegal index range";
     try {
      // check Mpara
      TFXX_assert(((Mpara.f(0)==1)&&(Mpara.l(0)==norder)), message);
      TFXX_assert(((Mpara.f(1)==1)&&(Mpara.l(1)>=Mpara.f(1))), message);
      TFXX_assert(((Mpara.f(2)==1)&&(Mpara.l(2)==nparameters)), message);
      // check Mdepth
      TFXX_assert(((Mdepth.f(0)==0)&&(Mdepth.l(0)>Mdepth.f(0))), message);
      // check Mfollow
      TFXX_assert(((Mfollow.f(0)==1)&&(Mfollow.l(0)>=Mfollow.f(0))), message);
      TFXX_assert(((Mfollow.f(1)==1)&&(Mfollow.l(1)==nparameters)), message);
      // check Mnpol
      TFXX_assert(((Mnpol.f(0)==1)&&(Mnpol.l(0)>=Mnpol.f(0))), message);
      TFXX_assert(((Mnpol.f(1)==1)&&(Mnpol.l(1)==nparameters)), message);
      // check for consistent number of sections
      TFXX_assert(((Mpara.l(1)==Mdepth.l(0))&&(Mfollow.l(0)==Mdepth.l(0))&&
                   (Mnpol.l(0)==Mdepth.l(0))), message);
    }
    catch ( ... ) {
      TFXX_abort("PolynomialModelFile: internal error!");
    }
  }

  /*----------------------------------------------------------------------*/

  //! return bottom depth of section
  Tvalue PolynomialModelFile::bottom(const int& i) const
  {
    TFXX_assert((!(ishalfspace(i))), "illegal section index!");
    return(Mdepth(valid_section_index(i))); 
  }

  /*----------------------------------------------------------------------*/

  //! return bottom depth of section
  Tvalue PolynomialModelFile::top(const int& i) const
  {
    TFXX_assert((!(ishalfspace(i))), "illegal section index!");
    return(Mdepth(valid_section_index(i)-1)); 
  }

  /*----------------------------------------------------------------------*/

  //! return checked section index
  int PolynomialModelFile::valid_section_index(const int& i) const
  {
    TFXX_assert(((i>0)&&(i<=Mdepth.l(0))), "illegal section index!");
    return(i);
  }

  /*----------------------------------------------------------------------*/

  //! return checked parameter index
  int PolynomialModelFile::valid_parameter_index(const int& i) const
  {
    TFXX_assert(((i>0)&&(i<=nparameters)), "illegal parameter index!");
    return(i);
  }

  /*----------------------------------------------------------------------*/

  //! return checked polynomial index
  int PolynomialModelFile::valid_polynomial_index(const int& i) const
  {
    TFXX_assert(((i>0)&&(i<=norder)), "illegal polynomial index!");
    return(i);
  }

  /*----------------------------------------------------------------------*/

  //! find section index
  int PolynomialModelFile::secindex(const Tvalue& depth) const
  {
    static int retval=1;
    if (!((Mdepth(retval-1)<=depth)&&(depth<Mdepth(retval))))
    {
      if (depth>Mdepth(retval))
      {
        while ((depth>Mdepth(retval))&&(retval<Mdepth.l(0))) { retval++; }
      } else {
        while ((depth<Mdepth(retval-1))&&(retval>1)) { retval--; }
      }
    }
    return(retval);
  }

  /*----------------------------------------------------------------------*/

  //! calculate parameter value at given depth
  Tvalue PolynomialModelFile::polyarg(const Tvalue& depth,
                                      const int& isec) const
  {
    int i=valid_section_index(isec);
    TFXX_assert((!(ishalfspace(i))), "isec is in halfpsace"); 
    return(depth-0.5*(Mdepth(i)+Mdepth(i-1)));
  }

  /*----------------------------------------------------------------------*/

  //! set unused polynomial coefficients to zero and check values
  void PolynomialModelFile::clean_unused_and_check()
  {
    // check consistency
    check_consistency();
    // set depth of upper halfspace
    Mdepth(Mdepth.f(0))=0.;
    // cycle all sections
    for (int isec=Mpara.f(1); isec<=Mpara.l(1); ++isec)
    {
      // cycle all parameters
      for (int ipar=Mpara.f(2); ipar<=Mpara.l(2); ++ipar)
      {
        if (ishalfspace(isec))
        {
          Mnpol(isec,ipar)=1;
          Mfollow(isec,ipar)=true;
        }
        if (isec==Mpara.f(1))
        {
          Mfollow(isec,ipar)=false;
        }
        switch (Mnpol(isec,ipar)) {
          // intentionally fall through
          case 1: Mpara(2, isec,ipar)=0.;
          case 2: Mpara(3, isec,ipar)=0.;
          case 3: break;
          default: TFXX_abort("illegal polynomial order!");
        }
      }
      // check depth
      TFXX_assert(((Mdepth(isec)>Mdepth(isec-1)) || ishalfspace(isec)),
                  "depth must increase!");
    }
    make_follow();
  }

  /*----------------------------------------------------------------------*/

  //! calculate parameter value at given depth
  Tvalue PolynomialModelFile::value(const int& ipar,
                                    const Tvalue& depth,
                                    const int& isec) const
  {
    int is=valid_section_index(isec);
    int ip=valid_parameter_index(ipar);
    Tvalue retval=Mpara(1,is,ip);
    if (!ishalfspace(is))
    {
      Tvalue x=polyarg(depth,is);
      retval=Mpara(1,is,ip)+x*(Mpara(2,is,ip)+x*Mpara(3,is,ip));
    }
    return(retval);
  }

  /*----------------------------------------------------------------------*/

  //! concatenate follow-sections
  void PolynomialModelFile::make_follow()
  {
    // cycle all sections
    for (int isec=Mpara.f(1); isec<=Mpara.l(1); ++isec)
    {
      // cycle all parameters
      for (int ipar=Mpara.f(2); ipar<=Mpara.l(2); ++ipar)
      {
        if (Mfollow(isec,ipar))
        {
          TFXX_assert((isec!=Mpara.f(1)),
                      "first section has nothing to follow!");
          // calculate value at bottom of upper section
          Tvalue vup=value(ipar, Mdepth(isec-1), (isec-1));
          // calculate value at top of this section
          Tvalue vlo=value(ipar, Mdepth(isec-1), isec);
          // correct mean of this section
          Mpara(1,isec,ipar)+=(vup-vlo);
        }
      }
    }
    Mmeans_are_valid=false;
  }

  /*----------------------------------------------------------------------*/

  //! output
  void PolynomialModelFile::write_to_stream(std::ostream& os) const
  {
    check_consistency();
    os << TF_POLYMODEL_CC_CVSID << std::endl << std::endl;
    os << modversion2 << std::endl;
    os.width(5);
    os << nsections();
    os.width(15);
    os << " " << "<-- number of sections" << std::endl << std::endl;
    os << "    Vp             Vs             density        Qp             Qs"
      << std::endl;
    const int bufsize=15;
    char buffer[bufsize];
    for (int isec=Mpara.f(1); isec<Mpara.l(1); ++isec)
    {
      os << std::endl;
      std::snprintf(buffer, bufsize, "%12.4f", Mdepth(isec));
      os.width(12);
      os << buffer;
      os.width(8);
      os << " " << "<-- bottom of section";
      os.width(5);
      os << isec << " / polynomial expansion:" << std::endl;
      for (int ipar=1; ipar<=nparameters; ++ipar)
      {
        os.width(12);
        int npol=Mnpol(isec,ipar);
        if (Mfollow(isec,ipar)) npol *= -1;
        os << npol << "   ";
      }
      os << std::endl << std::endl;
      for (int ipol=1; ipol<=norder; ++ipol)
      {
        for (int ipar=1; ipar<=nparameters; ++ipar)
        {
          if (ipar<4)
          {
            std::snprintf(buffer, bufsize, "%12.6f", Mpara(ipol,isec,ipar));
          } else {
            std::snprintf(buffer, bufsize, "%12.4f", Mpara(ipol,isec,ipar));
          }
          os.width(12);
          os << buffer << "   ";
        }
        os << std::endl;
      }
    }
  }

  /*----------------------------------------------------------------------*/

  //! input 
  void PolynomialModelFile::read_from_stream(std::istream& is)
  {
    // skip comment
    const int maxskip=500;
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    std::string identifier;
    getline(is, identifier);
    TFXX_assert((identifier.find(modversion2)==0),
                "missing or wrong identifier!");
    // number of sections
    int nsec;
    is >> nsec;
    is.ignore(maxskip,'\n');
    // prepare arrays
    try {
      Mpara=Tarray(norder,nsec+1,nparameters);
      Mdepth=Tarray(aff::Shaper(0,nsec+1));
      Mfollow=Tbarray(nsec+1,nparameters);
      Mnpol=Tiarray(nsec+1,nparameters);
    }
    catch ( ... ) {
      std::cerr << "PolynomialModelFile: could not create arrays for "
        << nsec << " sections!" << std::endl;
      TFXX_abort("aborting...");
    }
    Mdepth=0.;
    Mpara=0.;
    Mfollow=false;
    Mnpol=1;
    // skip headline
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    // read
    for (int isec=1; isec<=nsec; ++isec)
    {
      is.ignore(maxskip,'\n');
      is >> Mdepth(isec);
      is.ignore(maxskip,'\n');
      for (int ipar=1; ipar<=nparameters; ++ipar)
      {
        is >> Mnpol(isec, ipar);
        if (Mnpol(isec,ipar)<0)
        {
          Mnpol(isec,ipar) *= -1;
          Mfollow(isec,ipar)=true;
        }
      }
      for (int ipol=1; ipol<=norder; ++ipol)
      {
        for (int ipar=1; ipar<=nparameters; ++ipar)
        {
          is >> Mpara(ipol, isec, ipar);
        }
      }
    }
    // finalize
    clean_unused_and_check();
    Mmeans_are_valid=false;
  }

  /*----------------------------------------------------------------------*/

  //! calculate mean values for all sections
  void PolynomialModelFile::calculate_means() const
  {
    if (!Mmeans_are_valid)
    {
      Mmeans=Tarray(norder,nsections(),nparameters);
      for (int isec=Mmeans.f(1); isec<=Mmeans.l(1); ++isec)
      {
        for (int ipar=Mmeans.f(2); ipar<=Mmeans.l(2); ++ipar)
        {
          Mmeans(3,isec,ipar)=2.*Mpara(3,isec,ipar);
          Mmeans(2,isec,ipar)=Mpara(2,isec,ipar);
          Mmeans(1,isec,ipar)=Mpara(1,isec,ipar)+
            Mpara(3,isec,ipar)*thickness(isec)*thickness(isec)/12.;
        }
      }
      Mmeans_are_valid=true;
    }
  }

  /*----------------------------------------------------------------------*/

  //! calculate mean values for depth range
  /*
  TCarray PolynomialModelFile::means(const Tvalue& top
                                     const Tvalue& bottom) const
  {
  }
  */

  /*----------------------------------------------------------------------*/

  //! calculate derivative values at given depth
  Tarray PolynomialModelFile::values(const Tvalue& depth,
                                     const int& isec) const
  {
    int is=valid_section_index(isec);
    Tarray result(norder,nparameters);
    Tarray secpar(aff::slice(Mpara)()(is));
    result.copyin(secpar);
    if (!ishalfspace(is))
    {
      Tvalue x=polyarg(depth,is);
      for (int ipar=result.f(1); ipar<=result.l(1); ++ipar)
      {
        result(3,ipar)=2.*secpar(3,ipar);
        result(2,ipar)=secpar(2,ipar)+2.*secpar(3,ipar)*x;
        result(1,ipar)=secpar(1,ipar)+x*(secpar(2,ipar)+x*secpar(3,ipar));
      }
    }
    // NOTICE: returns reference (but result should have no reference to data
    // members)
    return(result);
  }

} // namespace gremlin1

/* ----- END OF polymodel.cc ----- */
