/*! \file stfinvfinitecausal.h
 * \brief finite and causal stf by time-domain convolution (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id: $
 * \author Thomas Forbriger
 * \date 15/02/2014
 * 
 * finite and causal stf by time-domain convolution (prototypes)
 * 
 * Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 15/02/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVFINITECAUSAL_H_VERSION

#define STFINV_STFINVFINITECAUSAL_H_VERSION \
  "STFINV_STFINVFINITECAUSAL_H   V1.0   "
#define STFINV_STFINVFINITECAUSAL_H_CVSID \
  "$Id: $"

#include<stfinv/stfinvbase.h>

namespace stfinv {

  /*! \brief Engine to find a finite, causal source time-history in time domain
   * \ingroup group_engines
   *
   * \par Concept behin this engine
   *
   * - \f$d_l\f$: data samples
   * - \f$s_l\f$: raw synthetics
   * - \f$q_l\f$: source time-history to be derived
   * - \f$s^c_l=\sum\limits_k s_{(l-k)} q_k\f$: convolved synthetics
   * - \f$E=\sum\limits_l (d_l-s^c_l)^2\f$: misfit to be minized
   *
   * \f[
   *   \frac{\partial E}{\partial q_j}
   *   =\frac{\partial}{\partial q_j}
   *   \left(\sum\limits_l 
   *   \left(d_l-\sum\limits_k s_{(l-k)} q_k\right)^2\right)
   *   =
   *   \sum\limits_l 
   *   \frac{\partial}{\partial q_j}
   *   \left(d_l-\sum\limits_k s_{(l-k)} q_k\right)^2
   *   =
   *   -2 \sum\limits_l 
   *   s_{(l-j)}
   *   \left(d_l-\sum\limits_k s_{(l-k)} q_k\right)
   *   =
   *   -2 \sum\limits_l 
   *   s_{(l-j)} d_l
   *   +2 \sum\limits_l 
   *   s_{(l-j)} \sum\limits_k s_{(l-k)} q_k
   * \f]
   *
   * Least-squares condition:
   * \f[
   * \frac{\partial E}{\partial q_j}\stackrel{!}{=}0,\quad \forall j
   * \f]
   * \f[
   * \sum\limits_l s_{(l-j)} \sum\limits_k s_{(l-k)} q_k
   * =
   * \sum\limits_l s_{(l-j)} d_l ,\quad \forall j
   * \f]
   *
   * System of linear equations:
   * \f[
   * \sum\limits_k
   * \underbrace{\left(\sum\limits_l s_{(l-j)} s_{(l-k)}\right)}_{=M_{jk}}
   * q_k =
   * \underbrace{\sum\limits_l s_{(l-j)} d_l}_{=r_j}
   * \f]
   *
   * Next:
   * - index range is defined on base of index range for data and desired
   *   finite length of source time-history
   * - additional traces are to be added by adding further terms to the misfit
   *   function, which results in further terms in the system of linear
   *   equations. 
   *   These terms should be given appropriate weights
   * - data should be normalized, such that a misfit of 1 is meaningful;
   *   this is easily done, by scaling all seismograms samples with the same
   *   factor (data as well as synthetics)
   * - an additional constraint could be applied to the \f$q_l\f$ by adding a
   *   term \f$\sum\limits_l\left(Q_l q_l\right)^2\f$ to the msifit \f$E\f$;
   *   this will result in a simple additional term on the left-hand-side of
   *   the system of linear equations;
   *   a reasonable weight must be given to this term
   *
   * \todo
   * STFEngineTDCausalConvolution is a rudiment to be filled with reasonable
   * code. To date the source code file is used to develop the theory.
   */
  class STFEngineFiniteCausal: public stfinv::STFBaseEngine {
    public:
      //! \brief typedef to refer to base class
      typedef stfinv::STFBaseEngine Tbase;
      //! \brief ID used to select thsi engine
      static const char* const ID;
      //! \brief short description of this engine
      static const char* const description;
      /*! \brief Constructor.
       */
      STFEngineFiniteCausal(const stfinv::Tvectoroftriples& triples,
                        const stfinv::Waveform& stf,
                        const std::string& parameters)
        : Tbase(triples, stf, parameters),
        Mscaleenergy(false)
      { this->initialize(); }
      //! \brief abstract base requires virtual destructor
      virtual ~STFEngineFiniteCausal() { }
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
      //! \brief scale energy
      bool Mscaleenergy;
  }; // class STFEngineFiniteCausal

}

#endif // STFINV_STFINVFINITECAUSAL_H_VERSION (includeguard)

/* ----- END OF stfinvfinitecausal.h ----- */
