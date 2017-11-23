/*! \file stfinvnormalize.h
 * \brief a Fourier domain engine which finds a normalizing source wavelet (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/05/2011
 * 
 * a Fourier domain engine which finds a normalizing source wavelet (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 08/05/2011   V1.0   Thomas Forbriger
 *  - 04/10/2011   V1.1   renamed Fourier domain least squares engine
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVNORMALIZE_H_VERSION

#define STFINV_STFINVNORMALIZE_H_VERSION \
  "STFINV_STFINVNORMALIZE_H   V1.1"

#include<stfinv/stfinvfourier.h>

namespace stfinv {

  /*! \brief Normalization engine
   * \ingroup group_engines
   *
   * \par Motivation
   * On the down-side of Fourier domain least squares as is implemented
   * in stfinv::STFEngineFDLeastSquares is the least squares approach.
   * Least squares aims in minimizing the least squares misfit between
   * recorded data and synthetics. 
   * In cases where it is difficult to minimize the misfit, e.g. when the wave
   * propagation properties of the synthetics differ from those of the
   * recorded data, the least squares misfit optimum might be a 
   * source correction filter equal zero.
   * The current approach (stfinv::STFEngineNormalize) seeks a source
   * correction filter such that the average energy as well as the average
   * phase of data and synthetics equal.
   * This is what you usually want in a early stage of inversion.
   *
   * \par Concept of this algorithm
   * Consider that
   * - \f$d_{lk}\f$ is the Fourier coefficient of recorded data at Frequency
   *   \f$f_l\f$ and receiver \f$k\f$ at offset \f$r_k\f$ and
   * - \f$s_{lk}\f$ is the Fourier coefficient of the corresponding
   *   synthetics.
   *
   * Then we seek a source correction filter with Fourier coefficients
   * \f$q_l\f$ such that
   * \f[
   *   \sum\limits_{k}\left|d_{lk}\right|^2
   *   =
   *   \sum\limits_{k}\left|s_{lk}\,q_l\right|^2
   *   \quad\forall\, l
   * \f]
   * and
   * \f[
   *   \sum\limits_{k}\Im(\ln(d_{lk}))
   *   =
   *   \sum\limits_{k}\Im(\ln(s_{lk}\,q_l))
   *   \quad\forall\, l.
   * \f]
   *
   * Actually we will apply weights \f$w_k\f$ to data from different offsets
   * \f$r_k\f$ such that
   * \f[
   *   \sum\limits_{k}\left|w_k\,d_{lk}\right|^2
   *   =
   *   \sum\limits_{k}\left|w_k\,s_{lk}\,q_l\right|^2
   *   \quad\forall\, l
   * \f]
   * and
   * \f[
   *   \sum\limits_{k}w_k\,\Im(\ln(d_{lk}))
   *   =
   *   \sum\limits_{k}w_k\,\Im(\ln(s_{lk}\,q_l))
   *   \quad\forall\, l.
   * \f]
   * Additionally we will apply a waterlevel \f$\epsilon^2\f$, such that
   * Fourier coefficients for which the average weighted energy of the
   * synthetics is smaller than a fraction \f$\epsilon^2\f$ of the total
   * average energy of the synthetics, will be damped.
   *
   * These conditions are satisfied by
   * \f[
   *   q_l=A_l\,e^{i\Phi_{l}}
   * \f]
   * with
   * \f[
   *   A_l=\sqrt{\frac{\sum\limits_{k}\left|w_k\,d_{lk}\right|^2}{
   *     \sum\limits_{k}\left|w_k\,s_{lk}\right|^{2}
   *     +
   *     \frac{\epsilon^2}{N_{f}}\,\sum\limits_{k}\sum\limits_{l=0}^{N_{f}-1}
   *     \left|w_k\,s_{lk}\right|^2}}
   * \f]
   * and
   * \f[
   *   \Phi_{l}=\frac{\sum\limits_{k}w_{k}\left(
   *             \Im\left(\ln(d_{lk})\right)-\Im\left(\ln(s_{lk})\right)
   *          \right)}{\sum\limits_k w_{k}}
   * \f]
   */
  class STFEngineNormalize: public stfinv::STFFourierDomainEngine {
    public:
      //! \brief typedef to refer to base class
      typedef stfinv::STFFourierDomainEngine Tbase;
      //! \brief ID used to select thsi engine
      static const char* const ID;
      //! \brief short description of this engine
      static const char* const description;
      /*! \brief Constructor.
       */
      STFEngineNormalize(const stfinv::Tvectoroftriples& triples,
                         const stfinv::Waveform& stf,
                         const std::string& parameters)
        : Tbase(triples, stf, parameters)
      { this->initialize(); }
      //! \brief abstract base requires virtual destructor
      virtual ~STFEngineNormalize() { }
      //! \brief Start engine 
      virtual void exec();
      //! \brief print online help
      virtual void help(std::ostream& os=std::cout) const;
      //! \brief print online help
      static void classhelp(std::ostream& os=std::cout);
      //! \brief return name of engine
      virtual const char* name() const;
    private:
      //! \brief initialize work space
      void initialize();

      // member data
    private:
      //! \brief waterlevel
      double Mwaterlevel;
  }; // class STFEngineNormalize

} // namespace stfinv

#endif // STFINV_STFINVNORMALIZE_H_VERSION (includeguard)

/* ----- END OF stfinvnormalize.h ----- */
