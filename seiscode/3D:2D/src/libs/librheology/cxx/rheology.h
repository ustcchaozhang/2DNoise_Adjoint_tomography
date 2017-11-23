/*! \file rheology.h
 * \brief header file for librheology (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * header file for librheology (prototypes)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 04/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_RHEOLOGY_H_VERSION

#define TF_RHEOLOGY_H_VERSION \
  "TF_RHEOLOGY_H   V1.0   "
#define TF_RHEOLOGY_H_CVSID \
  "$Id$"

#include<complex>
#include<cmath>

using std::sqrt;

namespace rheology {

  typedef double Tvalue;
  typedef std::complex<double> Tcvalue;

  class TransIsoModuli {
    public:
      TransIsoModuli(const Tvalue& C,
                     const Tvalue& A,
                     const Tvalue& L,
                     const Tvalue& N,
                     const Tvalue& F,
                     const Tvalue& rho,
                     const Tvalue& Qp,
                     const Tvalue& Qs):
        MC(C),MA(A),ML(L),MN(N),MF(F),Mrho(rho),MQp(Qp),MQs(Qs)
        { }

      const Tvalue& C() const      { return(MC); }
      const Tvalue& A() const      { return(MA); }
      const Tvalue& L() const      { return(ML); }
      const Tvalue& N() const      { return(MN); }
      const Tvalue& F() const      { return(MF); }
      const Tvalue& rho() const    { return(Mrho); }
      const Tvalue& Qp() const     { return(MQp); }
      const Tvalue& Qs() const     { return(MQs); }

      Tcvalue cC() const { return(C()*(1.-Tcvalue(0.,-1.)/Qp())); }
      Tcvalue cA() const { return(A()*(1.-Tcvalue(0.,-1.)/Qp())); }
      Tcvalue cL() const { return(L()*(1.-Tcvalue(0.,-1.)/Qs())); }
      Tcvalue cN() const { return(N()*(1.-Tcvalue(0.,-1.)/Qs())); }
      Tcvalue cF() const { return(eta()*(cA()-2.*cL())); }

      Tvalue vpv() const { return(sqrt(C()/rho())); }
      Tvalue vph() const { return(sqrt(A()/rho())); }
      Tvalue vsv() const { return(sqrt(L()/rho())); }
      Tvalue vsh() const { return(sqrt(N()/rho())); }
      Tvalue eta() const { return(F()/(A()-2.*L())); }

      Tcvalue cvpv() const { return(sqrt(cC()/rho())); }
      Tcvalue cvph() const { return(sqrt(cA()/rho())); }
      Tcvalue cvsv() const { return(sqrt(cL()/rho())); }
      Tcvalue cvsh() const { return(sqrt(cN()/rho())); }

      Tvalue Qkappa() const 
      {
        return(9.*kappa()/(((4.*A()*(1.+eta())+C())/Qp())-
                           ((4.*(N()+2.*L()*eta()))/Qs()))); 
      }
      Tvalue Qmu() const
      {
        return(15.*mu()/(((A()*(1.-2.*eta())+C())/Qp())+
                        ((2.*L()*(3.+2.*eta())+5.*N())/Qs()))); 
      }

      Tcvalue ckappa() const { return((cC()+4.*cA()-4.*cN()+4.*cF())/9.); }
      Tcvalue cmu() const { return((cC()+cA()+6.*cL()+5.*cN()-2.*cF())/15.); }

      Tvalue kappa() const { return((C()+4.*A()-4.*N()+4.*F())/9.); }
      Tvalue mu() const { return((C()+A()+6.*L()+5.*N()-2.*F())/15.); }
    private:
      Tvalue MC,MA,ML,MN,MF,Mrho,MQp,MQs;
  }; // class TransIsoModuli

  /*----------------------------------------------------------------------*/

  class IsoModuli: public TransIsoModuli {
    public:
      IsoModuli(const Tvalue& pmod,
                const Tvalue& smod,
                const Tvalue& rho,
                const Tvalue& Qp,
                const Tvalue& Qs):
        TransIsoModuli(pmod,pmod,smod,smod,(pmod-2.*smod),rho,Qp,Qs) { }
      Tvalue vp() const { return(vpv()); } 
      Tvalue vs() const { return(vsv()); } 
      Tcvalue cvp() const { return(cvpv()); } 
      Tcvalue cvs() const { return(cvsv()); } 
  }; // class IsoModuli

  /*----------------------------------------------------------------------*/

  class IsoFromVelocity: public IsoModuli {
    public:
      IsoFromVelocity(const Tvalue& vp,
                      const Tvalue& vs,
                      const Tvalue& rho,
                      const Tvalue& Qp,
                      const Tvalue& Qs):
        IsoModuli((vp*vp*rho),(vs*vs*rho),rho,Qp,Qs) { }
  }; // class IsoFromVelocity

  /*----------------------------------------------------------------------*/

  class IsoFromVelocityQmoduli: public IsoFromVelocity {
    public:
      IsoFromVelocityQmoduli(const Tvalue& vp,
                             const Tvalue& vs,
                             const Tvalue& rho,
                             const Tvalue& Qkappa,
                             const Tvalue& Qmu):
        IsoFromVelocity(vp,vs,rho,
                        Qp_from_vel_and_Qmod(vp,vs,Qkappa,Qmu),Qmu) { }
    private:
        static Tvalue Qp_from_vel_and_Qmod(const Tvalue& vp, const Tvalue& vs,
                                           const Tvalue& Qkappa, 
                                           const Tvalue& Qmu) 
        { return(1./((1./Qkappa)+((vs*vs)/(vp*vp))*
                    ((1./Qmu)-(4./(3.*Qkappa))))); }
  }; // class IsoFromVelocityQmoduli

  /*----------------------------------------------------------------------*/

  class IsoFromModuli: public IsoFromVelocityQmoduli {
    public:
      IsoFromModuli(const Tvalue& kappa,
                    const Tvalue& mu,
                    const Tvalue& rho,
                    const Tvalue& Qkappa,
                    const Tvalue& Qmu):
        IsoFromVelocityQmoduli(sqrt((kappa+4.*mu/3.)/rho),
                               (sqrt(mu/rho)),rho,Qkappa,Qmu) { }
  }; // class IsoFromModuli

  /*----------------------------------------------------------------------*/

  class IsoFromModuliQvelocity: public IsoFromVelocity {
    public:
      IsoFromModuliQvelocity(const Tvalue& kappa,
                             const Tvalue& mu,
                             const Tvalue& rho,
                             const Tvalue& Qp,
                             const Tvalue& Qs):
        IsoFromVelocity(sqrt((kappa+4.*mu/3.)/rho),
                        sqrt(mu/rho),rho,Qp,Qs) { }
  }; // class IsoFromModuliQvelocity

}

#endif // TF_RHEOLOGY_H_VERSION (includeguard)

/* ----- END OF rheology.h ----- */
