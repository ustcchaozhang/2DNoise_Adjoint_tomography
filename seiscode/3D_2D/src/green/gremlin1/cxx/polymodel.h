/*! \file polymodel.h
 * \brief polynomial subsurface model (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 30/12/2002
 * 
 * polynomial subsurface model (prototypes)
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
 *  - 31/12/2002   V1.1   introduced read access to arrays
 *  - 09/02/2010   V1.2   moved initialization of model identifier
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_POLYMODEL_H_VERSION

#define TF_POLYMODEL_H_VERSION \
  "TF_POLYMODEL_H   V1.2"
#define TF_POLYMODEL_H_CVSID \
  "$Id$"

#include<iostream>
#include<aff/array.h>
#include<aff/subarray.h>

//! contains all gremlin1 stuff
namespace gremlin1 {

  /*! \brief Handle model file structure of gremlin1
   *
   * This class uses libaff arrays and thus has reference semantics
   * inherently.
   */
  class PolynomialModelFile {
    public:
      //! my value type
      typedef double Tvalue;
      //! my array type
      typedef aff::Array<Tvalue> Tarray;
      //! read access array type
      typedef Tarray::Tcoc TCarray;
      //! bool array
      typedef aff::Array<bool> Tbarray;
      //! int array
      typedef aff::Array<int> Tiarray;

      //! model file identifier
      static const char modversion2[];
      //! number of polynomial orders
      static const int norder=3;
      //! number of physical parameters
      static const int nparameters=5;
      
      //! physical model parameters
      enum Epara {
        //! P-velocity
        mi_alpha=1, mivp=mi_alpha,
        //! S-velocity
        mi_beta=2, mivs=mi_beta,
        //! density
        mi_density=3, mirho=mi_density,
        //! Q_P
        mi_Qalpha=4, miqp=mi_Qalpha,
        //! Q_S
        mi_Qbeta=5, miqs=mi_Qbeta
      };

      //! default constructor
      PolynomialModelFile();
      //! create from file
      PolynomialModelFile(const char* filename);

      //! read model from stream
      void read_from_stream(std::istream& is);
      //! write model to stream
      void write_to_stream(std::ostream& os) const;

      //! return number of sections
      int nsections() const { return(Mpara.size(1)-1); }
      //! lower interface of section \p i
      Tvalue bottom(const int& i) const;
      //! upper interface of section \p i
      Tvalue top(const int& i) const;
      //! thickness of section \p i
      Tvalue thickness(const int& i) const
      { return(bottom(i)-top(i)); }
      //! is this section index the half-space
      bool ishalfspace(const int& i) const
      { return(i==Mdepth.l(0)); }
      //! full acces to parameter 
      Tvalue coeff(const int& ipol, 
                   const int& isec,
                   const Epara& ipar) const
      { return(Mpara(valid_polynomial_index(ipol),
                     valid_section_index(isec),
                     valid_parameter_index(ipar))); }
      //! return section for given depth
      int secindex(const Tvalue& depth) const;
      //! return parameter value at given depth
      Tvalue value(const Epara& ipar,
                   const Tvalue& depth) const
      { return(value(ipar, depth, secindex(depth))); }
      //! return parameter value at given depth calculated from
      //! polynomial for given section \p isec
      Tvalue value(const int& ipar,
                   const Tvalue& depth,
                   const int& isec) const;
      //! return values for derivatives too at given depth
      Tarray values(const Tvalue& depth) const
      { return(values(depth, secindex(depth))); }
      //! return values for derivatives at given depth calculated from
      //! polynomial for given section \p isec
      Tarray values(const Tvalue& depth,
                    const int& isec) const;
      //! return agrument of polynomial for given depth and section
      Tvalue polyarg(const Tvalue& depth, const int& isec) const;

      //! section means
      TCarray sectionmeans() const
      { calculate_means(); return(Mmeans); }
    private:
      //! check for valid section index
      int valid_section_index(const int& i) const;
      //! check for valid polynomial index
      int valid_polynomial_index(const int& i) const;
      //! check for valid parameter index
      int valid_parameter_index(const int& i) const;

      //! clean all data (should be called after modifications)
      //! calls check_consistency and make_follow
      void clean_unused_and_check();
      //! check consistency of stored data
      void check_consistency() const;
      //! make parameters continuous, where follow flag is set
      void make_follow();

      /*! parameter values
       *
       * This is a 3 x N x 5 matrix, where N-1 is the number of sections.
       * - first index: polynomial coefficient
       * - second index: section
       * - third index: model parameter
       */
      Tarray Mpara;
      /*! section interface depth
       *
       * This is a vector of size N+1, where N-1 is the number of sections.
       * The first index is zero, which is convenient in section search. The
       * last depth is meaningless. The last section represents the halfspace.
       * This again is convenient when deriving parameter value for a given
       * depth.
       */
      Tarray Mdepth;
      /*! follow flags
       *
       * This is a matrix of size N x 5, where N-1 is the number of sections.
       */
      Tbarray Mfollow; 
      /*! number of active polynomial coefficients
       *
       * This is a matrix of size N x 5, where N-1 is the number of sections.
       */
      Tiarray Mnpol;

      /*----------------------------------------------------------------------*/
      // some mutable data

      // prepare polynomial means
      void calculate_means() const;
      // polynmial means within section
      mutable Tarray Mmeans;
      // are means valid
      mutable bool Mmeans_are_valid;

  }; // class PolynomialModelFile

  /*----------------------------------------------------------------------*/

  //! output operator
  inline
  std::ostream& operator<<(std::ostream& os, 
                           const PolynomialModelFile& pmf)
  { pmf.write_to_stream(os); return(os); }

  //! output operator
  inline
  std::istream& operator>>(std::istream& is, 
                           PolynomialModelFile& pmf)
  { pmf.read_from_stream(is); return(is); }

} // namespace gremlin1

#endif // TF_POLYMODEL_H_VERSION (includeguard)

/* ----- END OF polymodel.h ----- */
