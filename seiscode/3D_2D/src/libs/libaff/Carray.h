/*! \file Carray.h
 * \brief classes to interface raw C arrays (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/05/2011
 * 
 * classes to interface raw C arrays (prototypes)
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
 *  - 14/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_CARRAY_H_VERSION

#define AFF_CARRAY_H_VERSION \
  "AFF_CARRAY_H   V1.0   "

#include<aff/array.h>
#include<aff/lib/checkedcast.h>

namespace aff {

  /*! \brief Interface class to raw memory (C style array)
   *
   * This class together with aff::CArray is provided as an interface for
   * users which have to provide the contents of an array through raw memory
   * pointers in C style.
   * Both classes take an aff::ConstArray or aff::Array instance respectively
   * as an argument for their constructor.
   * They provide a pointer to raw memory through their member functions
   * pointer() and castedpointer().
   * If a pointer \c p to the raw memeory behind \c array is defined by
   * \code
   * aff::Array<T> array;
   * aff::CArray<T> carray(array);
   * T* p=carray.pointer();
   * \endcode
   * then the array element \c array(i0,i1,i2,i3) is addressed by
   * \code
   * p[i0*s0+i1*s1+i2*s2+i3*s3]
   * \endcode
   * where the index value ranges are
   * - \c i0=0,1,... \c n0-1
   * - \c i1=0,1,... \c n1-1
   * - \c i2=0,1,... \c n2-1
   * - \c i3=0,1,... \c n3-1
   *
   * and
   * - \c n0=carray.size(0)
   * - \c n1=carray.size(1)
   * - \c n2=carray.size(2)
   * - \c n3=carray.size(3)
   *
   * and
   * - \c s0=carray.stride(0)
   * - \c s1=carray.stride(1)
   * - \c s2=carray.stride(2)
   * - \c s3=carray.stride(3)
   *
   * \note
   * The pointers passed by the access functions of this class are (certainly)
   * not reference counted.
   * Make sure to keep an instance of one of the arrays maintining the
   * underlying memory allocation as long as these pointers are in use.
   *
   * \sa helpertest.cc
   */
  template<class T>
    class ConstCArray
    {
      public:
        /*! \name Various types
         *
         * In particular due to our concept of const-correctness we need
         * several typedefs to declare types derived from the element type of
         * the array.
         *
         * \sa \ref sec_design_interface_typedef
         * \sa \ref sec_design_const
         */
        //@{
        //! Type of array to be interfaced
        typedef ConstArray<T> Tarray;
        //! Type of representation
        typedef typename Tarray::Trepresentation Trepresentation;
        //! Type of shape
        typedef typename Tarray::Tshape Tshape;
        //! we use this for one of the access operators
        typedef typename Tshape::TSizeVec TSizeVec;
        //! Element type
        typedef typename Tarray::Tvalue Tvalue;
        //! Type of pointer to element
        typedef typename Tarray::Tpointer Tpointer;
        //! Type of reference to element
        typedef typename Tarray::Treference Treference;
        //! const element type
        typedef typename Tarray::Tconst_value Tconst_value;
        //! Type of pointer to const element
        typedef typename Tarray::Tconst_pointer Tconst_pointer;
        //! Type of reference to const element
        typedef typename Tarray::Tconst_reference Tconst_reference;
        //! Type of this array
        typedef ConstCArray<T> Tcontainer;
        //! Type of the array of const values
        typedef Tcontainer Tcontainer_of_const;
        //! Short for Tcontainer_of_const
        typedef Tcontainer Tcoc;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * No copy constructors or copy operators are provided since this is
         * provided as an interface class only.
         */
        //@{
        //! construct from shape and representation
        ConstCArray(const Tarray& array)
          : Mrepresentation(array.representation())
        {
          Tshape shape=array.shape();
          // index range is zero based
          shape.setfirst(typename Tshape::TIndexVec(0)); 
          // offset to first element in represented memory
          Moffset=shape.first_offset();
          // strides
          Mstride=shape.stride();
          // sizes
          Msize=shape.last();
          for (unsigned i=0; i<Msize.size(); ++i)
          {
            ++Msize[i];
          }
        }
        //@}

        /*------------------------------------------------------------------*/

        /*! \name Shape access
         */
        //@{
        //! size of dimension \par i
        const Tsize& size(const Tsubscript& i) const
        { return (Msize[i]); }
        //! stride of dimension \par i
        const Tsize& stride(const Tsubscript& i) const
        { return Mstride[i]; }
        //! size of dimensions
        const TSizeVec& size() const
        { return (Msize); }
        //! strides of dimensions
        const TSizeVec& stride() const
        { return Mstride; }
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Memory access
         *
         */
        //@{
        //! return pointer to first element in Fortran layout
        Tconst_pointer pointer() const 
        { return(&Mrepresentation[this->offset()]); }
        /*! \brief return type-casted pointer to first element in Fortran
         * layout
         *
         * The cast checks for const-correctness and type-size. But you have
         * to ensure that there is a meaningful relation between both types
         * involved.
         *
         * \sa aff::util::SizeCheckedCast
         */
        template<class TT>
          TT* castedpointer() const
          { return(SizeCheckedCast<Tconst_value,TT>::cast(this->pointer())); }
        //@}

      protected:
        //! pass offset to derived class
        const typename TSizeVec::Tvalue& offset() const { return Moffset; }
          
      private:
        //! representation member
        Trepresentation Mrepresentation;
        //! sizes of dimensions
        TSizeVec Msize;
        //! strides of dimensions
        TSizeVec Mstride;
        //! offset to first index
        typename TSizeVec::Tvalue Moffset;

    }; // class ConstCArray

  /*======================================================================*/

  /*! \brief Interface class to raw memory (C style array)
   *
   * See aff::ConstCArray for the basic concepts used here.
   *
   * \sa aff::ConstCArray
   */
  template<class T>
    class CArray:
      public ConstCArray<T>
    {
      public:
        /*! \name Various types
         *
         * In particular due to our concept of const-correctness we need
         * several typedefs to declare types derived from the element type of
         * the array.
         *
         * \sa \ref sec_design_interface_typedef
         * \sa \ref sec_design_const
         */
        //@{
        //! Type of array to be interfaced
        typedef aff::Array<T> Tarray;
        //! Base class
        typedef aff::ConstCArray<T> Tbase;
        //! Type of representation
        typedef typename Tarray::Trepresentation Trepresentation;
        //! Type of shape
        typedef typename Tarray::Tshape Tshape;
        //! we use this for one of the access operators
        typedef typename Tshape::TSizeVec TSizeVec;
        //! Element type
        typedef typename Tarray::Tvalue Tvalue;
        //! Type of pointer to element
        typedef typename Tarray::Tpointer Tpointer;
        //! Type of reference to element
        typedef typename Tarray::Treference Treference;
        //! const element type
        typedef typename Tarray::Tconst_value Tconst_value;
        //! Type of pointer to const element
        typedef typename Tarray::Tconst_pointer Tconst_pointer;
        //! Type of reference to const element
        typedef typename Tarray::Tconst_reference Tconst_reference;
        //! Type of this array
        typedef CArray<T> Tcontainer;
        //! Type of the array of const values
        typedef Tbase Tcontainer_of_const;
        //! Short for Tcontainer_of_const
        typedef Tcontainer Tcoc;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * No copy constructors or copy operators are provided since this is
         * provided as an interface class only.
         */
        //@{
        //! construct from shape and representation
        CArray(const Tarray& array)
          : Tbase(array), Mrepresentation(array.representation()) 
        { }
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Memory access
         *
         */
        //@{
        //! return pointer to first element in Fortran layout
        Tpointer pointer() const 
        { return(&Mrepresentation[this->offset()]); }
        /*! \brief return type-casted pointer to first element in Fortran
         * layout
         *
         * The cast checks for const-correctness and type-size. But you have
         * to ensure that there is a meaningful relation between both types
         * involved.
         *
         * \sa aff::util::SizeCheckedCast
         */
        template<class TT>
          TT* castedpointer() const
          { return(SizeCheckedCast<Tvalue,TT>::cast(this->pointer())); }
        //@}

      private:
        //! my (mutable) data representation
        Trepresentation Mrepresentation;

    }; // class CArray

} // namespace aff

#endif // AFF_CARRAY_H_VERSION (includeguard)

/* ----- END OF Carray.h ----- */
