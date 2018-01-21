/*! \file sharedheap.h
 * \brief shared heap representation (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * shared heap representation (prototypes)
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
 * \sa aff::SHeap
 * \sa aff::ConstSharedHeap
 * \sa aff::SharedHeap
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 16/12/2002   V1.1   (thof)
 *                        - introduced new concept of const correct containers
 *                        - moved AllocException to error.h
 *  - 17/12/2002   V1.2   (thof)
 *                        - use access declarations (hint by wolle)
 *  - 20/12/2002   V1.3   (thof)
 *                        - moved SHeap to namespace util
 *  - 28/12/2002   V1.4   (thof)
 *                        - changed base class from specialization to
 *                          independent class template
 *                        - added class documentation
 *  - 29/12/2002   V1.5   (thof)
 *                        - use "using" syntax for access declarations
 *                        - resolved conflicts with protected operator[]
 *                          in derived classes
 *  - 31/12/2002   V1.6   (thof)
 *                        - removed constructor from Theadstruct
 *                        - made the non-initializing constructor
 *                          protected. Default constructor remains public (it
 *                          is needed if we construct a container for
 *                          ConstSharedHeap elements).
 *  - 19/12/2003   V1.7   (thof)
 *                        - index check now should work
 *  - 04/07/2005   V1.8   (thof)
 *                        - allows data modification through instances being
 *                          declared const
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SHAREDHEAP_H_VERSION

#define AFF_SHAREDHEAP_H_VERSION \
  "AFF_SHAREDHEAP_H   V1.8"

#include<new>
#include<aff/lib/error.h>
#include<aff/lib/types.h>

namespace aff {

#ifdef AFF_PREBUILT
namespace prebuilt {
#endif

  /*----------------------------------------------------------------------*/

namespace util {

  /*! \brief A structure to be shared between different SharedHeap instances.
   *
   * This is the core part of represented shared heap. Only objects of this
   * class allocate and delete memory. The SharedHeap representation has holds
   * a pointer to an instance of this struct. The representation is
   * responsible for doing the reference counting by incrementing and
   * decrementing Mnref and to call the destructor of SHeap once the reference
   * count drops to zero. The actual deallocation of the memory block used for
   * the array is, however, done within the destructor of SHeap. 
   *
   * For arrays taken from Fortran code we use a flag \c Mextern. If it is
   * true, this indicates that memory should neither be allocated be the
   * constructor nor given free by the destructor.
   *
   * \sa aff::SharedHeap
   */
  template<typename T>
    struct SHeap
    {
      //! Allocate memory on heap.
      SHeap(const Tsize& size);
      //! Take memory reference from elsewhere
      SHeap(T* pointer, const Tsize& size);

      //! Free heap memory.
      ~SHeap();

      T*     Mheap;   //!< shared raw array on heap
      Tsize  Msize;   //!< size (number of elements) of array
      Tsize  Mnref;   //!< number of referencing instances
      bool   Mextern; //!< true if memory allocation is handled elsewhere
    }; // struct SHeap

} // namespace util

  /*======================================================================*/

  /*! \brief This is the base class for const elements
   *
   * This is the base class for aff::SharedHeap. Although it accesses the data
   * in memory through a pointer of type \c T*, it does not allow
   * modification of the data through public access. Only for derived classes
   * protected members are provided that allow modification of the data.
   *
   * The whole functionality of shared heap (see "\ref page_representation")
   * is implemented within this base class. aff::SharedHeap is only a sparse
   * wrapper.
   *
   * \sa aff::SharedHeap
   * \sa aff::SHeap
   * \sa \ref sec_design_const
   * \sa \ref page_representation
   */
  template <class T>
    class ConstSharedHeap
    {
      public:
        //! Element type
        typedef const T Tvalue;
        //! Type of pointer to element
        typedef const T* Tpointer;
        //! Type of reference to element
        typedef const T& Treference;
        //! this class
        typedef aff::ConstSharedHeap<T> Tcontainer;
        //! Type of const version of SharedHeap
        typedef aff::ConstSharedHeap<T> Tcontainer_of_const;
        //! short for container of const
        typedef aff::ConstSharedHeap<T> Tcoc;

      private:
        //! Type of SHeap struct to be used in any case
        typedef aff::util::SHeap<T> Theapstruct;

      public:
        //! Create from nothing.
        ConstSharedHeap();

        //! Create representation for externally managed memory
        ConstSharedHeap(T* pointer, const Tsize& size);

        //! Copy representation to share heap.
        ConstSharedHeap(const Tcontainer& sharedheap);

        //! Deallocate heap memory if this is the last referencing instance.
        ~ConstSharedHeap();

        //! Copy representation to share heap.
        ConstSharedHeap<T>& operator=(const Tcontainer& sharedheap);

        /*! Return size (always inline).
         * \retval size of array on heap.
         */
        inline
        const Tsize& size() const
        { return(Mheapstruct->Msize); }

        /*! Return array access.
         * \return pointer to array on heap
         */
        const T* array() const
        { return(Mheapstruct->Mheap); }

        //! Index operator (always inline).
        inline
        const T& operator[](const Tsubscript& i) const
        { 
#ifdef AFF_INDEXCHECK
          AFF_assert(((i<Tsubscript(Mheapstruct->Msize)) && (i>=0)),
                     "SharedHeap: illegal index position!");
#endif
          return(Mheapstruct->Mheap[i]); 
        }

      /*-----------------------------------------------------------------*/
      /* here starts the PROTECTED section!
       * ----------------------------------
       *
       * These functions are needed by the derived aff::SharedHeap class to
       * access its base class and the data elements contained therein in a
       * defined way.
       */
      protected:
        //! Allocate new heap memory.
        ConstSharedHeap(const Tsize& size);

        //! write access to data
        inline
        T& write_access(const Tsubscript& i) const
        { 
#ifdef AFF_INDEXCHECK
          AFF_assert(((i<Tsubscript(Mheapstruct->Msize)) && (i>=0)),
                     "SharedHeap: illegal index position!");
#endif
          return(Mheapstruct->Mheap[i]); 
        }

        /*! Return array access.
         * \return pointer to array on heap
         */
        T* writable_array()
        { return(Mheapstruct->Mheap); }

      private:
        Theapstruct* Mheapstruct; //!< shared heap structure

    }; // class ConstSharedHeap<T>

  /*======================================================================*/

  /*! \brief A template class to share heap memory for different array
   * projections.
   *
   * This is a wrapper class that inherits publicly from aff::ConstSharedHeap.
   * Thus containers of type aff::SharedHeap and aff::ConstSharedHeap may
   * share the reference to memory through which they access the data. And the
   * conversion to the base class that does not allow modification of elements
   * is a trivial conversion.
   *
   * This class essentially adds no functionality. It just provides public
   * access to the protected members of aff::ConstSharedHeap. For further
   * documentation see there.
   *
   * \sa \ref page_representation
   * \sa aff::ConstSharedHeap
   */
  template <class T>
    class SharedHeap: public aff::ConstSharedHeap<T>
    {
      public:
        //! Element type
        typedef T Tvalue;
        //! Type of pointer to element
        typedef T* Tpointer;
        //! Type of reference to element
        typedef T& Treference;
        //! this class
        typedef aff::SharedHeap<T> Tcontainer;
        //! base class
        typedef aff::ConstSharedHeap<T> Tbase;
        //! Type of const version of SharedHeap
        typedef Tbase Tcontainer_of_const;
        //! short for container of const
        typedef Tbase Tcoc;

      public:
        //! Create from nothing.
        SharedHeap(): Tbase() { }

        //! Allocate new heap memory.
        SharedHeap(const Tsize& size): Tbase(size) { }

        //! Create representation for externally managed memory
        SharedHeap(T* pointer, const Tsize& size):
          Tbase(pointer, size) { }

        //! Copy representation to share heap.
        SharedHeap(const Tcontainer& sharedheap):
          Tbase(sharedheap) { }

        //! Deallocate heap memory if this is the last referencing instance.
        ~SharedHeap() { }

        //! \name access declarations
        //@{
        //! access to base class function
        using Tbase::size;
        using Tbase::operator[];
        using Tbase::array;
        //@}

        //! delegate to base
        T& operator[](const Tsubscript& i) const
        { return(this->Tbase::write_access(i)); }

        /*! Return array access.
         * \return pointer to array on heap
         */
        T* array() const
        { return(this->Tbase::writable_array()); }
        
    }; // class SharedHeap

#ifdef AFF_PREBUILT
} // namespace prebuilt
#endif

} // namespace aff

#ifndef AFF_NO_DEFINITIONS
#include <aff/lib/sharedheap_def.h>
#endif

#endif // AFF_SHAREDHEAP_H_VERSION (includeguard)

/* ----- END OF sharedheap.h ----- */
