/*! \file array.h
 * \brief full template array class headers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * full template array class headers (prototypes)
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
 * Include this file to have access to the basic modules in libaff.a.
 *
 * \sa aff
 * \sa aff::Array
 * \sa \ref page_using
 * \sa \ref sec_main_modules
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * ============================================================================
 * 
 * REVISIONS and CHANGES from former array_dec.h
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 16/12/2002   V1.1   (thof)
 *                        - introduced new concept of const-correctness;
 *                          this should work for passing arrays as intrinsic
 *                          references for const element type
 *  - 17/12/2002   V1.2   (thof)
 *                        - introduced access declarations
 *  - 19/12/2002   V1.3   (thof)
 *                        - provide read access to base classes
 *                          and read/write access to representation of
 *                          non-const array
 *                        - replaced four size-argument constructors by one
 *                          constructor with default arguments
 *                        - distinguish between mutable and non-mutable
 *                          representation
 *  - 20/12/2002   V1.4   (thof)
 *                        - Access declarations are nor useable with functions
 *                          that return a reference to *this. They must be
 *                          reimplemented.
 *  - 23/12/2002   V1.5   (thof)
 *                        - copyin() and copyout() work
 *  - 28/12/2002   V1.6   (thof)
 *                        - changed base class from specialization to
 *                          independent class template
 *  - 29/12/2002   V1.7   (thof)
 *                        - ConstArray now inherits from ConstSharedHeap
 *                          (see "\ref sec_design_replicated")
 *                        - use "using" syntax for access declarations
 *                        - arrays do no longer derive from shared heap
 *                          classes
 *                        - reworked shared heap access concept fundamentally;
 *                          code looks much simpler now 
 *                        - member template must be defined here
 *                        - factored out copyin code
 *  - 31/12/2002   V1.8   (thof)
 *                        - Removed non-initializing constructors in
 *                          ConstArray (except default constructor, which is
 *                          needed, when object is a container element) as
 *                          suggested by Wolfgang.
 *  - 02/01/2003   V1.9   (thof)
 *                        - type TIndexVec was not declared
 *  - 03/01/2003   V1.10  (thof)
 *                        - copyout function now returns an aff::Array rather
 *                          than an aff::ConstArray
 *                        - aff::Array inherits copyout from aff::ConstArray
 *                        - deepcopy is now in namespace aff
 *  - 04/07/2005   V1.11  (thof)
 *                        - provide data modification through const Array
 *  - 05/07/2005   V1.12  (thof)
 *                        - expose const representation too if declared const 
 *  - 19/06/2006   V1.13  (thof)
 *                        - offer Tvalue and friends as non-const types
 *
 * ============================================================================
 * 
 * REVISIONS and CHANGES from former array_def.h
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 16/12/2002   V1.1   (thof)
 *                        - definitions are now for base class
 *                          (i.e. spcialization for const T)
 *                        - added standard constructors
 *  - 18/12/2002   V1.2   (thof)
 *                        - added stepper code to scalar assignment operator
 *  - 19/12/2002   V1.3   (thof)
 *                        - size constructors did not work
 *                        - replaced four size-argument constructors by one
 *                          constructor with default arguments
 *                        - distinguish between mutable and non-mutable
 *                          representation
 *  - 23/12/2002   V1.4   (thof)
 *                        - copyin() and copyout() work
 *  - 28/12/2002   V1.5   (thof)
 *                        - changed base class from specialization to
 *                          independent class template
 *  - 29/12/2002   V1.6   (thof)
 *                        - due to clearer concept of representation access
 *                          constructors became more simple and are defined in
 *                          declaration - only copy operations remain here
 *                        - member template cannot be explicitely instantiated
 *  - 03/01/2003   V1.7   (thof)
 *                        - copyout function now returns an aff::Array rather
 *                          than an aff::ConstArray
 *
 * ============================================================================
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 10/11/2010   V1.1   merged array_def.h and array_dec.h into this file
 *  - 21/06/2011   V1.2   allow shape manipulation by explicite access
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_ARRAY_H_VERSION

#define AFF_ARRAY_H_VERSION \
  "AFF_ARRAY_H   V1.2"

#include <aff/lib/sharedheap.h>
#include <aff/lib/strided.h>
#include <aff/lib/stridedstepper.h>
#include <aff/lib/deepcopy.h>
#include <aff/lib/error.h>

/*======================================================================*/
// declaration part
// ===============

namespace aff {

  // forward declaration
  template<class T> class Array;

  /*! \brief Array base class
   * \ingroup group_array
   *
   * This is a multidimensional (array) container that uses a strided memory
   * layout (Fortran shape) and counted references to data in global memory.
   * For examples see the various test programs.
   *
   * \sa \ref sec_design_multidimensional
   * \sa \ref sec_design_interface
   * \sa \ref sec_design_const
   * \sa \ref page_representation
   * \sa \ref sec_design_replicated
   * \sa aff::Strided
   * \sa aff::SharedHeap
   * \sa tests/arraytest.cc
   * \sa tests/f77test.cc
   * \sa tests/helpertest.cc
   */
  template<class T>
    class ConstArray:
      private aff::Strided
    {
      public:
        /*! \name Various types
         *
         * In particular due to our concept of const-correctness we need
         * several typedefs to declare types derived from the element type of
         * the array.
         *
         * This is in particular usefull for function templates that may take
         * any container class as argument (e.g. aff::subarray) or for other
         * classes dealing with any container (e.g. aff::Iterator).
         * By means of these typedefs they may retrieve appropriate types they
         * have to deal with.
         *
         * \sa \ref sec_design_interface_typedef
         * \sa \ref sec_design_const
         */
        //@{
        //! Type of representation
        typedef aff::ConstSharedHeap<T> Trepresentation;
        //! Type of shape
        typedef aff::Strided Tshape;
        //! Type of shape stepper
        typedef Tshape::Tstepper Tstepper;
        //! we use this for one of the access operators
        typedef Tshape::TIndexVec TIndexVec;
        //! Element type
        typedef T Tvalue;
        //! Type of pointer to element
        typedef T* Tpointer;
        //! Type of reference to element
        typedef T& Treference;
        //! const element type
        typedef const T Tconst_value;
        //! Type of pointer to const element
        typedef const T* Tconst_pointer;
        //! Type of reference to const element
        typedef const T& Tconst_reference;
        //! Type of this array
        typedef ConstArray<T> Tcontainer;
        //! Type of the array of const values
        typedef Tcontainer Tcontainer_of_const;
        //! Short for Tcontainer_of_const
        typedef Tcontainer Tcoc;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * \note
         * We use the default copy constructor, which automatically invokes
         * the copy constructors of the base classes aff::Strided and
         * aff::SharedHeap<T>. This essentially is a shallow copy of the
         * array, i.e. the copy will reference to the same elements in memory.
         * See aff::Array<T>::copyin() and aff::Array<T>::copyout() for deep
         * copy operations.
         */
        //@{
        //! construct from nothing (empty)
        ConstArray() { }
        //! construct from shape and representation
        ConstArray(const Tshape& shape, 
                   const Trepresentation& representation):
          Tshape(shape), Mrepresentation(representation)
          { check_consistency(); }
        //@}

        /*-----------------------------------------------------------------*/
      
        /*! \name Const access operators
         *
         * Although we generally distinguish between the constness of the
         * container and the constness of the contained data (see 
         * \ref sec_design_const), we provide const element access operators
         * the prohibit element modification. We assume that a const version
         * of the array is usually meant to be used only for reading.
         */
        //@{
        //! full dimensionality access
        const T& operator()(const TIndexVec& index) const
        { return(Mrepresentation[offset(index)]); }
        //! access from 1 index value
        const T& operator()(const Tsubscript& i0) const
        { return(Mrepresentation[offset(i0)]); }
        //! access from 2 index values
        const T& operator()(const Tsubscript& i0,
                            const Tsubscript& i1) const
        { return(Mrepresentation[offset(i0, i1)]); }
        //! access from 3 index values
        const T& operator()(const Tsubscript& i0,
                            const Tsubscript& i1,
                            const Tsubscript& i2) const
        { return(Mrepresentation[offset(i0, i1, i2)]); }
        //! access from 4 index values
        const T& operator()(const Tsubscript& i0,
                            const Tsubscript& i1,
                            const Tsubscript& i2,
                            const Tsubscript& i3) const
        { return(Mrepresentation[offset(i0, i1, i2, i3)]); }
        //@}

        /*-----------------------------------------------------------------*/

        /*! \name Shape access
         */
        //@{
        //! return first index of dimension i
        const Tsubscript& f(const Tsubscript& i) const
        { return(this->Tshape::first(i)); }
        //! return last index of dimension i
        const Tsubscript& l(const Tsubscript& i) const
        { return(this->Tshape::last(i)); }
        //@}

        /*! \brief create an identical copy (deep copy) of this array
         *
         * This is mainly used to create a copy of a truely identical array
         * (i.e. array with same element type or at least const version of
         * same element type). Use this function in conjunction with the
         * assignment operator. E.g.:
         * \code
         *   aff::Array<int> A(3,4);
         *   A=5.;
         *   aff::Array<int> B, C;
         *   B=A.copyout();
         *   C=A;
         * \endcode
         * Here arrays \c A and \c B have exactly the same contents but use
         * different memory. While changes to elements of \c C will also
         * affect elements of \c A, this not the case for changes applied to
         * \c B.
         */
        Array<T> copyout() const;
      
        //! \name access declarations
        //@{
        //! access to base class function
        using Tshape::first;
        using Tshape::last;
        using Tshape::size;
        //@}
     
        //! provide access to const shape
        const Tshape& shape() const { return(*this); }
     
        //! allow shape manipulation
        Tshape& shape() { return(*this); }
     
        //! provide restricted access representation
        const Trepresentation& representation() const
        { return (Mrepresentation); }

      protected:
        //! provide access to shape offset functions for derived class
        using Tshape::offset;
          
      private:
        //! check consitency between representation and shape
        void check_consistency() const;
        
        //! representation member
        Trepresentation Mrepresentation;

    }; // class ConstArray

  /*======================================================================*/

  /*! \brief Full multi-dimensional array functionality.
   * \ingroup group_array
   *
   * This is the full array class template. It adds no additional
   * functionality to its base class aff::ConstArray. But it provied acess to
   * all member functions of the base, also to functions that allow data
   * modification.
   *
   * \sa aff::ConstArray
   * \sa \ref sec_design_const
   * \sa \ref sec_design_replicated
   */
  template<class T>
    class Array:
      public ConstArray<T>
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
        //! Type of this array
        typedef Array<T> Tcontainer;
        //! base is container of const (see specialization below)
        typedef aff::ConstArray<T> Tbase;
        //! Type of the array of const values
        typedef Tbase Tcontainer_of_const;
        /*! \brief short for Tcontainer_of_const
         *
         * We generally distinguish between constness of the array and
         * constness of the contained data (see \ref sec_design_const).
         * There will be situations, when you want to promise that a function
         * will not change the contents of an array. In this case you may use
         * a declaration (prototype) like
         * \code
         *   typedef aff::Array<int> Tmyarray;
         *   void myfunction(const Tmyarray::Tcoc& array);
         * \endcode
         * and you may use this function like
         * \code
         *   Tmyarray A(6,7);
         *   A=4;
         *   myfunction(A);
         * \endcode
         *
         * \sa \ref sec_design_const
         */
        typedef Tbase Tcoc;
        //! Type of representation
        typedef aff::SharedHeap<T> Trepresentation;
        //! Type of subscriptor
        typedef aff::Strided Tshape;
        //! we use this for one of the access operators
        typedef Tshape::TIndexVec TIndexVec;
        //! Element type
        typedef T Tvalue;
        //! Type of pointer to element
        typedef T* Tpointer;
        //! Type of reference to element
        typedef T& Treference;
        //! const element type
        typedef const T Tconst_value;
        //! Type of pointer to const element
        typedef const T* Tconst_pointer;
        //! Type of reference to const element
        typedef const T& Tconst_reference;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * \note
         * We use the default copy constructor, which automatically invokes
         * the copy constructors of the base classes aff::Strided and
         * aff::SharedHeap<T>. This essentially is a shallow copy of the
         * array, i.e. the copy will reference to the same elements in memory.
         * See aff::Array<T>::copyin() and aff::Array<T>::copyout() for deep
         * copy operations.
         */
        //@{
        //! construct from nothing (empty)
        Array() { }
        //! construct from shape and representation
        Array(const Tshape& shape, 
              const Trepresentation& representation):
          Tbase(shape, representation),
          Mrepresentation(representation) { }
        //! construct from shape (defines size and layout)
        explicit Array(const Tshape& shape)
          { 
            Tshape newshape(shape.first(), shape.last());
            Mrepresentation=Trepresentation(newshape.memory_size());
            this->Tbase::operator=(Tbase(newshape, Mrepresentation));
          }
        //! construct from dimension sizes
        explicit Array(const Tsize& s0, const Tsize& s1=1, 
                       const Tsize& s2=1, const Tsize& s3=1)
          { 
            Tshape newshape(s0, s1, s2, s3);
            Mrepresentation=Trepresentation(newshape.memory_size());
            this->Tbase::operator=(Tbase(newshape, Mrepresentation));
          }
        //@}

        /*-----------------------------------------------------------------*/
      
        //! \name access declarations
        //@{
        //! access to base class function
        using Tbase::operator();
        using Tbase::shape;
        using Tbase::copyout;
        //@}

        /*! \brief copy values (deep copy) from other array of convertible type
         *
         * This member function reads the element values of another array of
         * same shape and applies them to this array. In fact the shape needs
         * not be the same. The copy is done through sequential access and as
         * most number as possible will be copied in increasing memory address
         * order.
         *
         * Example:
         * \code
         *   aff::Array<float> A(24);
         *   A=15.
         *   aff::Array<double> B(3,8);
         *   B.copyin(A);
         * \endcode
         * \c B will preserve its shape but is filled with the contents of 
         * \c A (which are not where interesting in this example). Changes
         * applied to the contents of B will not affect the contents of A.
         *
         * \param a other container with element type convertible to element
         *        type of this array and appropriate stepper class
         * \return itself
         */
        template<class C>
          Array& copyin(const C& a)
          {
            aff::deepcopy(a, *this);
            return(*this);
          }
     
        //! return full access representation
        const Trepresentation& representation() const
        { return (Mrepresentation); }

        /*-----------------------------------------------------------------*/
          
        //! set whole array to value
        Tcontainer& operator=(const T& value);

        /*-----------------------------------------------------------------*/

        //! full dimensionality access
        T& operator()(const TIndexVec& index) const
        { return(Mrepresentation[this->Tbase::offset(index)]); }
        //! access from 1 index value
        T& operator()(const Tsubscript& i0) const
        { return(Mrepresentation[this->Tbase::offset(i0)]); }
        //! access from 2 index values
        T& operator()(const Tsubscript& i0,
                      const Tsubscript& i1) const
        { return(Mrepresentation[this->Tbase::offset(i0, i1)]); }
        //! access from 3 index values
        T& operator()(const Tsubscript& i0,
                      const Tsubscript& i1,
                      const Tsubscript& i2) const
        { return(Mrepresentation[this->Tbase::offset(i0, i1, i2)]); }
        //! access from 4 index values
        T& operator()(const Tsubscript& i0,
                      const Tsubscript& i1,
                      const Tsubscript& i2,
                      const Tsubscript& i3) const
        { return(Mrepresentation[this->Tbase::offset(i0, i1, i2, i3)]); }

      private:
        //! my (mutable) data representation
        Trepresentation Mrepresentation;

    }; // class Array

/*----------------------------------------------------------------------*/

} // namespace aff

/*======================================================================*/
// definition part
// ===============

namespace aff {

/*----------------------------------------------------------------------*/

  //! check shape and representation
  template<class T>
    void ConstArray<T>::check_consistency() const
    {
      AFF_assert((this->Tshape::first_offset()>=0),
                 "ERROR (ConstArray): invalid shape");
      AFF_assert((this->Tshape::last_offset()<
                  Tsubscript(Mrepresentation.size())),
              "ERROR (ConstArray): shape and representation are inconsistent");
    }

/*----------------------------------------------------------------------*/

  //! create a value (deep) copy
  template<class T>
    Array<T> ConstArray<T>::copyout() const
    {
      aff::Array<T> copy(Tshape(this->first(),this->last()));
      copy.copyin(*this);
      return(copy);
    }

/*======================================================================*/

  //! set whole array to scalar value
  template<class T>
    Array<T>& Array<T>::operator=(const T& value)
    {
      Tshape::Tstepper st(this->shape());
      for(st.tofirst(); st.valid(); st.incr())
      { Mrepresentation[st.current()]=value; } 
      return(*this);
    }

} // namespace aff

#endif // AFF_ARRAY_H_VERSION (includeguard)

/* ----- END OF array.h ----- */
