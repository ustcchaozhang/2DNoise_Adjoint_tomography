/*! \file strided.h
 * \brief shape of s strided array (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * shape of s strided array (prototypes)
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
 * \sa aff::Strided
 *
 * \note
 * You usually will not include this directly. It is included through array.h
 * and binarray.h
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 11/12/2002   V1.1   revision of this part of code (thof)
 *                        - remove template parameter
 *                        - replace by maximum dimensionality constant
 *                        - does not need to know anything about stepper
 *                        - introduced Mbase (is necessary for new concept of
 *                          multidemensional arrays proposed by wolle)
 *  - 12/12/2002   V1.2   (thof)
 *                        - basic functionality is complete
 *                        - documentation is still missing
 *  - 13/12/2002   V1.3   (thof)
 *                        - correct definition of mapping formula
 *                        - provides enough basic functionality 
 *                          (including access needed by stepper)
 *  - 14/12/2002   V1.4   (thof)
 *                        - template-free definition part will now always
 *                          be placed in the binary library
 *  - 16/12/2002   V1.5   (thof)
 *                        - renamed subarray and slice function
 *                        - added shift and setfirst
 *  - 20/12/2002   V1.6   (thof)
 *                        - forward declaration of StridedStepper
 *                        - typedef Tstepper
 *  - 29/12/2002   V1.7   (thof)
 *                        - now provides basic Fortran style constructor
 *  - 15/05/2011   V1.8   (thof)
 *                        - added dense 1D array check
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_STRIDED_H_VERSION

#define AFF_STRIDED_H_VERSION \
  "AFF_STRIDED_H   V1.8"

#include<aff/lib/types.h>
#include<aff/lib/simplearray.h>

namespace aff {

  namespace util {
    //! forward declaration of stepper class
    class StridedStepper;
  } // namespace util

  /*! \brief Shape for a rectangular array layout.
   *
   * A rectangular array layout is the usual layout for multi-dimensional
   * arrays. This is in contrast to packed layouts for banded matrices or
   * triangular matrices that do not hold every array element.
   *
   * A Strided shape typically is constrcuted through an aff::Shaper class.
   *
   * \note
   * dimension index is 0,1,2,... (zero-based) when calling size(i) member
   * function e.g.
   *
   * \todo 
   * rework documentation of Strided
   *
   * \internal
   * \par Mapping of index values into linear representation range
   * The shape class defines how index values \a i0, \a i1, etc. are mapped
   * into the linear adressing range used by the memory representation (see
   * SharedHeap).
   * The adress in the linear array is
   * \f[
   * a=o+\sum\limits_{k=0}^{M-1}\;(i_k-f_k)\,s_k
   *  =\sum\limits_{k=0}^{M-1}\;i_k\,s_k+
   *   \underbrace{o-\sum_{k=0}^{M-1}f_k\,s_k}_{=b_{M-1}},
   * \f]
   * if \a M is the number of index values \f$ i_k \f$ passed to the access
   * operator. In formula \f$ f_k \f$ are the elements of \c Mfirst,
   * \f$ s_k \f$ are the elements of \c Mstride,
   * \f$ b_l \f$ are the elements of \c Mbase, and
   * \f$ o \f$ is \c Mbase[0] (i.e. the first accessible position within the
   * representation.
   *
   */
  class Strided {
    public:
      /*! maximum dimensionality
       * \note
       * Some parts of the code rely on \c Mmax_dimen>=2.
       */
      static const Tdim Mmax_dimen=4;
        
      //! type of size and stride array (always positive)
      typedef SimpleRigidArray<Tsize, Mmax_dimen> TSizeVec;
      //! type of limit arrays (any sign)
      typedef SimpleRigidArray<Tsubscript, Mmax_dimen> TIndexVec;
      //! type of stepper
      typedef aff::util::StridedStepper Tstepper;

      /*-----------------------------------------------------------------*/

      /*! \name Constructors
       */
      //@{
      //! construct and initialize to zero
      Strided(): Mstride(0), Mfirst(0), Mlast(0), Mbase(0) { }
      /*! \brief construct do given size and first index
       *
       * \param sizes vector defining index range size for each dimension
       * \param first defines first index for each dimension
       * \param shift defines offset shift (i.e. first element accessed in
       *              memory representation)
       */
      explicit Strided(const TSizeVec& sizes, const Tsubscript& first=1,
              const Tsubscript& shift=0)
      { setup_from_size(sizes, first, shift); }
      /*! \brief construct Fortran layout to given first and last index
       *
       * \param first vector of first index value in each dimension
       * \param last vector of last index value in each dimension
       * \param shift defines offset shift (i.e. first element accessed in
       *              memory representation)
       *
       * This constructor together with the aff::Strided::shrink() function is
       * used by aff::Shaper in aff::Shaper::operator Tshape () to create a
       * strided shape.
       * \sa \ref page_array_layout
       */
      explicit Strided(const TIndexVec& first, const TIndexVec& last, 
              const Tsubscript& shift=0);
      /*! \brief construct from given set of Fortran style sizes
       *
       * \param s0 size of first dimension
       * \param s1 size of second dimension
       * \param s2 size of third dimension
       * \param s3 size of fourth dimension
       *
       * \sa \ref page_array_layout
       */
      explicit Strided(const Tsize& s0, const Tsize& s1=1, 
              const Tsize& s2=1, const Tsize& s3=1);
      //@}

      /*-----------------------------------------------------------------*/

      /*! \name Access to shape
       *
       */
      //@{
      //! total size of mapped memory range
      Tsize memory_size() const
      { return (offset(Mlast)-offset(Mfirst)+1); }
      //! total number of mapped elements
      Tsize size() const;
      //! first mapped position
      Tsubscript first_offset() const
      { return offset(Mfirst[0]); }
      //! last mapped position
      Tsubscript last_offset() const
      { return offset(Mlast); }
      //! first index of dimension \par i
      const Tsubscript& first(const Tsubscript& i) const
      { return Mfirst[i]; }
      //! last index of dimension \par i
      const Tsubscript& last(const Tsubscript& i) const
      { return (Mlast[i]); }
      //! size of dimension \par i
      Tsize size(const Tsubscript& i) const
      { return (static_cast<Tsize>(Mlast[i]-Mfirst[i]+1)); }
      //! stride of dimension \par i
      const Tsize& stride(const Tsubscript& i) const
      { return Mstride[i]; }
      //! return vector of first index values
      const TIndexVec& first() const { return(Mfirst); }
      //! return vector of last index values
      const TIndexVec& last() const { return(Mlast); }
      //! return vector of stride values
      const TSizeVec& stride() const { return(Mstride); }
      //@}

      /*-----------------------------------------------------------------*/

      /*! \name Map index values to linear representation range
       *
       */
      //@{
      //! full dimensionality access
      Tsubscript offset(const TIndexVec& index) const
      { return(inline_innerproduct(index,Mstride)+Mbase[Mmax_dimen-1]); }
      //! offset from 1 index value
      Tsubscript offset(const Tsubscript& i0) const
      { return(i0*Mstride[0]+Mbase[0]); }
      //! offset from 2 index values
      Tsubscript offset(const Tsubscript& i0,
                        const Tsubscript& i1) const
      { return(i0*Mstride[0]+i1*Mstride[1]+Mbase[1]); }
      //! offset from 3 index values
      Tsubscript offset(const Tsubscript& i0,
                        const Tsubscript& i1,
                        const Tsubscript& i2) const
      { return(i0*Mstride[0]+i1*Mstride[1]+i2*Mstride[2]+Mbase[2]); }
      //! offset from 4 index values
      Tsubscript offset(const Tsubscript& i0,
                        const Tsubscript& i1,
                        const Tsubscript& i2,
                        const Tsubscript& i3) const
      { return(i0*Mstride[0]+i1*Mstride[1]+i2*Mstride[2]+
               i3*Mstride[3]+Mbase[3]); }
      //@}

      /*-----------------------------------------------------------------*/

      /*! \name Support for slices and subarrays
       *
       * \note 
       * We intentionally call the subarray and slice functions \c shrink and
       * \c collapse (rather than \c subarray and \c slice), because they do
       * not return another shape instance, which is a subarray or slice. They
       * in fact manipulate the existing instance and return itself! To get a
       * copy, you must first create a copy and then call shrink, collapse,
       * shift or setfirst for this copy.
       */
      //@{
      //! subarray
      Strided& shrink(const TIndexVec& last);
      //! subarray
      Strided& shrink(const TIndexVec& first, const TIndexVec& last);
      //! subarray
      Strided& shrink(const Tdim& i, 
                      const Tsubscript& first, const Tsubscript& last);
      //! subarray
      Strided& shrink(const Tdim& i, 
                      const Tsubscript& last);
      //! slice
      Strided& collapse(const Tdim& i, const Tsubscript& index);
      //! shift
      Strided& shift(const Tdim& i, const Tsubscript& index);
      //! shift
      Strided& shift(const TIndexVec& index);
      //! setfirst
      Strided& setfirst(const Tdim& i, const Tsubscript& index);
      //! setfirst
      Strided& setfirst(const TIndexVec& index);
      //@}

      /*-----------------------------------------------------------------*/

    private:
      void calculate_base(const Tsubscript& offset);
      void setup_from_size(const TSizeVec& size, 
                           const Tsubscript& first=1,
                           const Tsubscript& shift=0);

      TSizeVec Mstride;   //<! strides for each dimension
      TIndexVec Mfirst;   //<! first index of each dimension
      TIndexVec Mlast;    //<! last index of each dimension
      TIndexVec Mbase;    //<! base for each dimension (see index operators)
  };

  /*======================================================================*/
  // helper functions
  
  namespace util {

    /*! \brief check whether array shape describes a 1D array with dense
     * layout in the memory.
     *
     * \ingroup group_array_extensions
     * \param shape array shape
     * \return true if shape is dense and 1D
     */
    bool is_dense_1D_array(const aff::Strided& shape);

  } // namespace util

} // namespace aff

#endif // AFF_STRIDED_H_VERSION (includeguard)

/* ----- END OF strided.h ----- */
