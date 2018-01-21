/*! \file stfinvfdleastsquares.h
 * \brief least squares in the frequency domain (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * least squares in the frequency domain (prototypes)
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
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 30/09/2011   V1.1   implemented handling of additional time series pairs
 *  - 04/10/2011   V1.2   renamed engine
 *  - 14/10/2015   V1.3   new end-user usage functions
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVFDLEASTSQUARES_H_VERSION

#define STFINV_STFINVFDLEASTSQUARES_H_VERSION \
  "STFINV_STFINVFDLEASTSQUARES_H   V1.3"

#include<stfinv/stfinvfourier.h>

namespace stfinv {

  /*! \brief Fourier domain least squares engine
   * \ingroup group_engines
   *
   * \par Concept behind this engine
   * If
   * - \f$d_{lk}\f$ is the Fourier coefficient of recorded data at Frequency
   *   \f$f_l\f$ and receiver \f$k\f$ at offset \f$r_k\f$,
   * - \f$s_{lk}\f$ is the Fourier coefficient of the corresponding
   *   synthetics and
   * - \f$q_l\f$ is that of the sought source tim function,
   * .
   * then this engine will minimize the objective function
   * \f[
   *   E=\sum\limits_{l,k}\left|w_{lk}\,
   *      \left(d_{lk}-s_{lk}q_l\right)
   *   \right|^2+\sum\limits_{l}\lambda^2\left|q_l\right|^2
   *   =\chi^2+\psi^2
   * \f]
   * with respect to the real part \f$q_l^\prime\f$ and the
   * imaginary part \f$q_l^{\prime\prime}\f$ of 
   * \f[
   *   q_l=q_l^\prime+i\,q_l^{\prime\prime}.
   * \f]
   * In the above expression
   * \f[
   *   \chi^2=\sum\limits_{l,k}\left|w_{lk}\,
   *           \left(d_{lk}-s_{lk}q_l\right)
   *            \right|^2
   * \f]
   * is the data misfit with weights \f$w_{lk}\f$ and
   * \f[
   *   \psi^2=\sum\limits_{l}\lambda^2\left|q_l\right|^2
   * \f]
   * is used for regularization and will introduce a water-level in the
   * deconvolution.
   * \f$\lambda\f$ will balance both contributions.
   * The conditions
   * \f[
   *   \frac{\partial E}{\partial q_l^\prime}\stackrel{!}{=}0
   *   \quad\wedge\quad
   *   \frac{\partial E}{\partial q_l^{\prime\prime}}\stackrel{!}{=}0
   * \f]
   * result in (Forbriger, 2001, appendix A.3)
   * \f[
   *   q_l=\frac{
   *     \eta^2\sum\limits_{k}f_k^2\,s_{kl}^\ast\,d_{kl}
   *   }{
   *     \lambda^2+\eta^2\sum\limits_{k}f_k^2\,s_{kl}^\ast\,s_{kl}
   *   }
   *   \quad\forall\, l
   * \f]
   * where
   * \f[
   *   w_{lk}=\eta\,f_k
   * \f]
   * and \f$f_k\f$ is a receiver specific weighting factor.
   * Now \f$\eta\f$ and \f$\lambda\f$ have to be used to balance the
   * regularization.
   * We aim to specify a waterlevel as a fraction of synthetic data energy.
   *  
   * \par Setting up the waterlevel
   * The misfit equals one if the scaled energy of the residual
   * \f$d_{lk}-s_{lk}q_l\f$ equals the scaled energy of the synthetics
   * \f$s_{lk}\f$ and
   * \f[
   *   \eta^2=\frac{1}{\sum\limits_k f_k^2\sum\limits_l \left|s_{lk}\right|^2}
   * \f]
   * is the reciprocal of the scaled energy of the synthetics.
   * If we then choose
   * \f[
   *   \frac{\lambda^2}{\eta^2}=\frac{\epsilon^2}{N\eta^2}=
   *     \frac{\epsilon^2}{N}\sum\limits_k f_k^2\sum\limits_{l=0}^{N-1}
  *     \left|s_{lk}\right|^2
   * \f]
   * where \f$N\f$ is the number of frequencies, then \f$\epsilon^2\f$
   * will specify a waterlevel as a fraction of the scaled energy of the
   * synthetics.
   *
   * \par Using Parceval's Theorem to calculate signal energy
   * Parceval's Theorem for a signal \f$a(t)\f$ and its Fourier transform 
   * \f$\tilde{a}(\omega)\f$ is
   * \f[
   *   \int\limits_{-\infty}^{+\infty}\bigl|a(t)\bigr|^2\,\textrm{d} t=
   *   \int\limits_{-\infty}^{+\infty}\bigl|\tilde{a}(\omega)\bigr|^2\,
   *     \frac{\textrm{d} \omega}{2\pi}.
   * \f]
   * If \f$S_{jk}\f$ are the time series samples corresponding to the Fourier
   * coefficients \f$\tilde{s}_{lk}\f$ and \f$\Delta t\f$ is the sampling
   * interval then
   * \f[
   *   \sum\limits_{k=0}^{M-1}\left|S_{jk}\right|^2\,\Delta t=
   *   \sum\limits_{l=0}^{M-1}\left|\tilde{s}_{lk}\right|^2\,\frac{1}{M\,\Delta t},
   * \f]
   * where \f$M=2N\f$ is the number of samples in the time series.
   * In the above calculation the energy sum only uses the positive
   * frequencies and
   * \f[
   *   \sum\limits_k f_k^2\sum\limits_{l=0}^{N-1}\left|\tilde{s}_{lk}\right|^2
   *   =
   *     N\,(\Delta t)^2\,
   *     \sum\limits_k f_k^2
   *     \sum\limits_{j=0}^{2N-1}\left|S_{jk}\right|^2.
   * \f]
   * Fourier coefficients \f$s_{lk}\f$ calculated by the
   * stfinv::STFFourierDomainEngine are not scaled (see documentation of
   * libfourierxx and libfftw3), such that
   * \f[
   *   \Delta t\,s_{lk}=\tilde{s}_{lk}
   * \f]
   * (both, \f$s_{lk}\f$ and \f$\tilde{s}_{lk}\f$ are Fourier coefficients).
   * Consequently
   * \f[
   *   \sum\limits_k f_k^2\sum\limits_{l=0}^{N-1}\left|s_{lk}\right|^2
   *   =
   *     N\,
   *     \sum\limits_k f_k^2
   *     \sum\limits_{j=0}^{2N-1}\left|S_{jk}\right|^2.
   * \f]
   *
   * \par Final calculation recipe
   * The solution to our problem is
   * \f[
   *   q_l=\frac{
   *     \sum\limits_{k}f_k^2\,s_{lk}^\ast\,d_{lk}
   *   }{
   *     \epsilon^2\,\sum\limits_k f_k^2
   *                \sum\limits_{j=0}^{2N-1}\left|S_{jk}\right|^2
   *           +\sum\limits_{k}f_k^2\,s_{lk}^\ast\,s_{lk}
   *   }
   *   \quad\forall\, l,
   * \f]
   * where
   * \f[
   *   \sum\limits_{j=0}^{2N-1}\left|S_{jk}\right|^2
   * \f]
   * is the sum of the squared sample values \f$S_{jk}\f$ of the synthetic
   * time series for receiver \f$k\f$, \f$f_k\f$ are the scaling factors
   * provided by stfinv::STFBaseEngine::weight(), and \f$\epsilon^2\f$
   * is the water level parameter passed to STFEngineFDLeastSquares.
   *
   * \par References
   * Forbriger, T., 2001. Inversion flachseismischer Wellenfeldspektern.
   *   PhD thesis, University of Stuttgart. 
   *   http://elib.uni-stuttgart.de/opus/volltexte/2001/861/
   *
   * \par Why was it renamed?
   * This engine was called 'blind deconvolution' engine.
   * It was renamed to fourier domain least squares engine in October 2011.
   *
   * This engine understands the recorded data as being the synthetic data
   * being convoled with the STF (source correction filter)
   * and having some noise
   * added.
   * The true impulse response of the subsurface could be obtained by
   * deconvolution of the recorded data with the STF.
   * Neither the impulse response of the subsurface nor the STF are known. 
   * For this reason the STF has to be found by minimizing an objective
   * function.
   * For this reason the it is called 'blind' deconvolution.
   *
   * However, the approach of libstfinv is to convolved the synthetics with
   * the STF to reducde the misfit to the recorded data, not to deconvolve the
   * recorded data. 
   * For this reason, maybe, we should better call the approach 'blind
   * convolution'.
   * With respect to image processing techniques, all engines in libstfinv do
   * some kind of 'blind convolution', not only the blind deconvolution
   * engine.
   * Consequently this engine should better be called 'least-squares engine'.
   */
  class STFEngineFDLeastSquares: public stfinv::STFFourierDomainEngine {
    public:
      //! \brief typedef to refer to base class
      typedef stfinv::STFFourierDomainEngine Tbase;
      //! \brief ID used to select this engine
      static const char* const ID;
      //! \brief short description of this engine
      static const char* const description;
      /*! \brief Constructor.
       */
      STFEngineFDLeastSquares(const stfinv::Tvectoroftriples& triples,
                                  const stfinv::Waveform& stf,
                                  const std::string& parameters)
        : Tbase(triples, stf, parameters), Mwaterlevel(1.e-3)
      { this->initialize(); }
      /*! \brief Constructor.
       */
      STFEngineFDLeastSquares(const stfinv::Tvectoroftriples& triples,
                                  const stfinv::Waveform& stf,
                                  const stfinv::Tvectorofpairs& pairs,
                                  const std::string& parameters)
        : Tbase(triples, stf, pairs, parameters), Mwaterlevel(1.e-3)
      { this->initialize(); }
      //! \brief abstract base requires virtual destructor
      virtual ~STFEngineFDLeastSquares() { }
      //! \brief Start engine 
      virtual void exec();
      //! \brief print online help
      virtual void help(std::ostream& os=std::cout) const;
      //! \brief print online help
      static void classhelp(std::ostream& os=std::cout);
      //! \brief print detailed description
      virtual void usage(std::ostream& os=std::cout) const;
      //! \brief print detailed description
      static void classusage(std::ostream& os=std::cout);
      //! \brief return name of engine
      virtual const char* name() const;
    private:
      //! \brief initialize work space
      void initialize();

      // member data
    private:
      //! \brief waterlevel
      double Mwaterlevel;
  }; // class STFEngineFDLeastSquares

} // namespace stfinv

#endif // STFINV_STFINVFDLEASTSQUARES_H_VERSION (includeguard)

/* ----- END OF stfinvfdleastsquares.h ----- */
