/*! \file series.h
 * \brief linear series class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/12/2002
 * 
 * linear series class (prototypes)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/12/2002   V1.0   Thomas Forbriger
 *  - 20/12/2002   V1.1   (thof)
 *                        - introduced LinearShape
 *                        - forward declaration of StridedStepper
 *                        - typedef Tstepper
 *  - 28/12/2002   V1.2   (thof)
 *                        - changed base class from specialization to
 *                          independent class template
 *                        - transformed conversion operator into memeber
 *                          function representation()
 *  - 29/12/2002   V1.3   (thof)
 *                        - ConstSeries now inherits from ConstSharedHeap
 *                          (see "\ref sec_design_replicated")
 *                        - use "using" syntax for access declarations
 *                        - reworked to use shared heap member data
 *                        - factored out copyin code
 *  - 31/12/2002   V1.4   (thof)
 *                        - Removed non-initializing constructors in
 *                          ConstSeries (except default constructor, which is
 *                          needed, when object is a container element) as
 *                          suggested by Wolfgang.
 *  - 19/12/2003   V1.5   (thof)
 *                        - cleaning all constructors and parts of the
 *                          documentation
 *                        - some changes in LinearShape
 *  - 23/12/2003   V1.6   (thof)
 *                        - return Trepresentation::Tcoc if requested
 *                        - typedef Tstepper is not needed, is it?
 *                        - well, it is needed. but you will find LinearShape
 *                          in lib/linearshape.h now - anyway
 *  - 13/01/2004   V1.7   (thof)
 *                        - corrected error in constructor from shape and
 *                          representation
 *  - 07/02/2004   V1.8   (thof)
 *                        - provide index range manipulation access to user
 *  - 04/07/2005   V1.9   (thof)
 *                        - provide pointer access
 *                        - provide data modification through const Series
 *  - 05/07/2005   V1.10  (thof)
 *                        - expose const representation too if declared const 
 *  - 27/07/2005   V1.11  (thof)
 *                        - deepcopy moved to namespace aff
 *                        - copyout() used wrong index values in constructor
 *                        - ConstSeries::copyout() must use a writeable
 *                          intermediate container
 *  - 19/06/2006   V1.12  (thof)
 *                        - offer Tvalue as a non-const type
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SERIES_H_VERSION

#define AFF_SERIES_H_VERSION \
  "AFF_SERIES_H   V1.12"

#include <aff/lib/sharedheap.h>
#include <aff/lib/linearshape.h>
#include <aff/lib/seriesstepper.h>
#include <aff/lib/error.h>
#include <aff/lib/deepcopy.h>

namespace aff {

  /*! \brief base class
   *
   * \sa \ref sec_design_interface
   * \sa \ref sec_design_const
   * \sa \ref page_representation
   * \sa \ref sec_design_replicated
   * \sa aff::LinearShape
   * \sa aff::SharedHeap
   */
  template<class T>
    class ConstSeries: 
      private aff::LinearShape
    {
      public:
        /*! \name Various types
         *
         * In particular due to our concept of const-correctness we need
         * several typedefs to declare types derived from the element type of
         * the array.
         *
         * \sa \ref sec_design_const
         */
        //@{
        //! Type of representation
        typedef aff::ConstSharedHeap<T> Trepresentation;
        //! Type of shape
        typedef aff::LinearShape Tshape;
        //! Type of stepper
        typedef Tshape::Tstepper Tstepper;
        //! Element type
        typedef T Tvalue;
        //! Type of pointer to element
        typedef T* Tpointer;
        //! Type of reference to element
        typedef T& Treference;
        //! Element type
        typedef const T Tconst_value;
        //! Type of pointer to element
        typedef const T* Tconst_pointer;
        //! Type of reference to element
        typedef const T& Tconst_reference;
        //! Type of this array
        typedef ConstSeries<T> Tcontainer;
        //! Type of the array of const values
        typedef Tcontainer Tcontainer_of_const;
        //! short for Tcontainer_of_const
        typedef Tcontainer Tcoc;
        //@}

        /*-----------------------------------------------------------------*/
        
        /*! \name Constructors
         *
         * There is no reason to construct an aff::ConstSeries from scratch,
         * since you can only read from ist. The only three reasons might be:
         *
         * 1. As an element in another container. In that case we will use the
         *    default constructor. 
         *
         * 2. Within a constructor of aff::Series. In that case we will pass a
         *    shape and a representation.
         *
         * 3. To construct a series from an aff::SharedHeap just to read it
         *    with tools of aff::ConstSeries.
         */
        //@{
        //! construct from nothing (empty)
        ConstSeries() { }
        //! construct from shape and representation
        ConstSeries(const Tshape& shape,
                    const Trepresentation& representation):
          Tshape(shape), Mrepresentation(representation) 
          { check_consistency(); }
        //! construct from representation 
        ConstSeries(const Trepresentation& representation):
          Tshape(0,representation.size()-1,0),
          Mrepresentation(representation) { }
        //@}

        /*-----------------------------------------------------------------*/

        //! Data read access
        const T& operator()(const Tsubscript& i) const
        { return(Mrepresentation[this->Tshape::offset(i)]); }

        /*! pointer to first element
         *
         * The shape guarantees that the memory layout is dense. This means
         * that the elements of a series can be accessed like a C-style array
         * through a pointer to the first element of the series. For this
         * reason it is appropriate to provide direct access to the underlying
         * memory block.
         */
        const T* pointer() const
        { return &this->operator()(this->first()); }

        /*-----------------------------------------------------------------*/

        /*! \name Shape access
         */
        //@{
        //! (short for) first valid index
        const Tsubscript& f() const
        { return(this->first()); }
        //! (short for) last valid index
        const Tsubscript& l() const
        { return(this->last()); }
        //@}
      
        //! \name access declarations
        //@{
        //! access to base class function
        using Tshape::first;
        using Tshape::last;
        using Tshape::size;
        using Tshape::setfirstindex;
        using Tshape::setlastindex;
        using Tshape::setindexrange;
        using Tshape::shift;
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
        Tcontainer copyout() const;

        //! offer conversion only to constant version of representation
        const Trepresentation& representation() const
        { return (Mrepresentation); }
     
        //! provide access to const shape
        const Tshape& shape() const
        { return(*this); }

      protected:
        //! provide offset function to derived class
        using Tshape::offset;
        
      private:
        //! check consistency between shape and representation
        void check_consistency() const;

        //! my memory representation
        Trepresentation Mrepresentation;

    }; // class ConstSeries<T>

  /*======================================================================*/
  
  /*! \brief A base class for time series and spectra
   *
   * \sa aff::ConstSeries
   * \sa \ref sec_design_const
   * \sa \ref sec_design_replicated
   */
  template<class T>
    class Series: public ConstSeries<T> {
      public:
        /*! \name Various types
         *
         */
        //@{
        //! Type of this array
        typedef Series<T> Tcontainer;
        //! base is container of const (see specialization below)
        typedef ConstSeries<T> Tbase;
        //! Type of the array of const values
        typedef Tbase Tcontainer_of_const;
        //! Type of shape
        typedef aff::LinearShape Tshape;
        //! short for Tcontainer_of_const
        typedef Tbase Tcoc;
        //! Type of representation
        typedef aff::SharedHeap<T> Trepresentation;
        //! Element type
        typedef T Tvalue;
        //! Type of pointer to element
        typedef T* Tpointer;
        //! Type of reference to element
        typedef T& Treference;
        //! Element const type
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
         * the copy constructors of the base classes aff::LinearShape and
         * aff::SharedHeap<T>. This essentially is a shallow copy of the
         * array, i.e. the copy will reference to the same elements in memory.
         * See aff::Series<T>::copyin() and aff::Series<T>::copyout() for deep
         * copy operations.
         */
        //@{
        //! construct from nothing (empty)
        Series() { }
        //! construct for a given size
        Series(const Tsize& size)
        {
          Tshape newshape(0, size-1, 0);
          Mrepresentation=Trepresentation(newshape.memory_size());
          Tbase::operator=(Tbase(newshape, Mrepresentation)); 
        }
        //! construct from index range limits
        Series(const Tsubscript& first, const Tsubscript& last)
        {
          Tshape newshape(first, last, 0);
          Mrepresentation=Trepresentation(newshape.memory_size());
          Tbase::operator=(Tbase(newshape, Mrepresentation)); 
        }
        //! construct from shape 
        Series(const Tshape& shape)
        {
          Tshape newshape(shape.first(), shape.last(), 0);
          Mrepresentation=Trepresentation(newshape.memory_size());
          this->Tbase::operator=(Tbase(newshape, Mrepresentation));
        }
        //! construct from shape and representation
        Series(const Tshape& shape,
               const Trepresentation& representation):
          Tbase(shape, representation), Mrepresentation(representation) 
        { }
        //! construct from representation 
        Series(const Trepresentation& representation, 
               const Tsubscript& shift=0)
        {
          Tshape newshape(shift, shift+representation.size()-1, 0);
          Mrepresentation=representation;
          Tbase::operator=(Tbase(newshape, Mrepresentation)); 
        }
        //@}

        /*-----------------------------------------------------------------*/
      
        //! \name access declarations
        //@{
        //! access to base class function
        using Tbase::operator();
        using Tbase::shape;
        //@}
      
        //! Data modification access
        T& operator()(const Tsubscript& i) const
        { return(Mrepresentation[this->Tbase::offset(i)]); }

        /*! pointer to first element
         *
         * The shape guarantees that the memory layout is dense. This means
         * that the elements of a series can be accessed like a C-style array
         * through a pointer to the first element of the series. For this
         * reason it is appropriate to provide direct access to the underlying
         * memory block.
         */
        T* pointer() const
        { return &this->operator()(this->first()); }
          
        //! set whole series to value
        Tcontainer& operator=(const T& value);

        //! copy in is allowed here 
        template<class C>
          Tcontainer& copyin(const C& a)
          {
            aff::deepcopy(a, *this);
            return(*this);
          }

        //! create a (deep) copy of this
        Tcontainer copyout() const;
     
        //! expose representation
        const Trepresentation& representation() const
        { return (Mrepresentation); }

      private:
        //! my (mutable) memory representation
        Trepresentation Mrepresentation;

    }; // class series

  /*======================================================================*/
  // definition part

  //! check shape and representation
  template<class T>
    void ConstSeries<T>::check_consistency() const
    {
      AFF_assert((this->Tshape::offset(this->first())>=0),
                 "ERROR (ConstSeries): invalid shape");
      AFF_assert((this->Tshape::offset(this->last())<
                  Tsubscript(Mrepresentation.size())),
              "ERROR (ConstSeries): shape and representation are inconsistent");
    }

  /*----------------------------------------------------------------------*/

  //! set value to whole series
  template<class T>
    Series<T>& Series<T>::operator=(const T& value)
    {
      for (Tsubscript i=this->first(); i<=this->last(); i++)
      { Mrepresentation[this->Tbase::offset(i)]=value; }
      return(*this);
    }

  /*----------------------------------------------------------------------*/

  //! create a copy of this array
  template<class T>
    Series<T> Series<T>::copyout() const
    {
      Tcontainer copy(this->first(),this->last());
      copy.copyin(*this);
      return(copy);
    }

  /*----------------------------------------------------------------------*/

  //! create a copy of this array
  template<class T>
    ConstSeries<T> ConstSeries<T>::copyout() const
    {
      // must use a writeable container as intermediate object
      Series<T> copy(this->first(),this->last());
      copy.copyin(*this);
      return(copy);
    }
}

#endif // AFF_SERIES_H_VERSION (includeguard)

/* ----- END OF series.h ----- */
