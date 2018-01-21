/*! \file sharedheap_def.h
 * \brief shared heap definitions (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * shared heap definitions (prototypes)
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
 * \note
 * Never include this header directly. Here you find code factored out from
 * the aff::SharedHeap class. This code may be compiled into a binary library.
 *
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 20/12/2002   V1.1   (thof)
 *                        - moved SHeap to namespace util
 *  - 28/12/2002   V1.2   (thof)
 *                        - changed base class from specialization to
 *                          independent class template
 *  - 31/12/2002   V1.3   (thof)
 *                        - NULL is deprecated as pointed out by Wolfgang
 *                        - removed constructor from Theadstruct
 *  - 19/12/2003   V1.4   (thof)
 *                        - allocate at least one element (default
 *                          constructor)
 * 
 * ============================================================================
 */

#ifndef AFF_SHAREDHEAP_H_VERSION 
#error "include this only through sharedheap.h"
#endif

// include guard
#ifndef AFF_SHAREDHEAP_DEF_H_VERSION

#define AFF_SHAREDHEAP_DEF_H_VERSION \
  "AFF_SHAREDHEAP_DEF_H   V1.4"

namespace aff {

#ifdef AFF_PREBUILT
namespace prebuilt {
#endif

#ifdef AFF_PREBUILT
#ifndef AFF_COMPILING_LIBRARY
#error "definition read in prebuilt mode and not during library compilation"
#endif
#endif

/*======================================================================*/

namespace util {

  //! create counted reference object
  template <typename T>
    inline
    SHeap<T>::SHeap(const Tsize& size)
    : Mheap(0), Msize(size), Mnref(1), Mextern(false)
    { 
      if (size>0) {
        try { Mheap=new T[size]; }
        catch (std::bad_alloc) {
          throw AllocException(size, sizeof(T));
        }
      } else { 
        Msize=0;
      }
    }

  /*----------------------------------------------------------------------*/

  //! create counted reference object from external memory
  template <typename T>
    inline
    SHeap<T>::SHeap(T* pointer, const Tsize& size)
    : Mheap(pointer), Msize(size), Mnref(1), Mextern(true) { }

  /*----------------------------------------------------------------------*/

  //! remove counted reference
  template <typename T>
    inline
    SHeap<T>::~SHeap()
    {
      if ((Mheap!=0) && (Msize>0) && (!Mextern))
      { 
        delete[] Mheap; 
      }
    }

} // namespace util

/*======================================================================*/

  //! create representation from nothing
  template <typename T>
    inline
    ConstSharedHeap<T>::ConstSharedHeap()
    : Mheapstruct(new Theapstruct(1)) { }

  /*----------------------------------------------------------------------*/

  //! create representation for given number of elements
  template <typename T>
    inline
    ConstSharedHeap<T>::ConstSharedHeap(const Tsize& size)
    : Mheapstruct(new Theapstruct(size)) { }

  /*----------------------------------------------------------------------*/

  //! create from externally managed memory
  template <typename T>
    inline
    ConstSharedHeap<T>::ConstSharedHeap(T* pointer, const Tsize& size)
    : Mheapstruct(new Theapstruct(pointer, size)) { }

  /*----------------------------------------------------------------------*/

  //! create representation from another representation
  template <typename T>
    inline
    ConstSharedHeap<T>::ConstSharedHeap(const Tcontainer& sharedheap)
    : Mheapstruct(sharedheap.Mheapstruct) 
    { ++(Mheapstruct->Mnref); }

  /*----------------------------------------------------------------------*/

  //! delete representation
  template <typename T>
    inline
    ConstSharedHeap<T>::~ConstSharedHeap()
    {
      if (--(Mheapstruct->Mnref) < 1)
      {
        delete Mheapstruct;
      }
    }

  /*----------------------------------------------------------------------*/

  //! copy representation by exchanging counted reference
  template <typename T>
    inline
    ConstSharedHeap<T>&
    ConstSharedHeap<T>::operator=(const ConstSharedHeap<T>& sharedheap)
    {
      if (Mheapstruct != sharedheap.Mheapstruct)
      {
        if (--(Mheapstruct->Mnref) < 1)
        {
          delete Mheapstruct;
        }
        Mheapstruct=sharedheap.Mheapstruct;
        ++(Mheapstruct->Mnref);
      }
      return(*this);
    }

#ifdef AFF_PREBUILT
} // namespace prebuilt
#endif

} // namespace aff

#endif // AFF_SHAREDHEAP_DEF_H_VERSION (includeguard)

/* ----- END OF sharedheap_def.h ----- */
