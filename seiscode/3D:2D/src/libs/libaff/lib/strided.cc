/*! \file strided.cc
 * \brief shape of a strided array (definitions)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * shape of a strided array (definitions)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 11/12/2002   V1.1   major revision (thof)
 *                        - removed template parameter
 *                        - calculates base for index operator of
 *                          each dimensionality
 *  - 12/12/2002   V1.2   (thof)
 *                        - added shrink and collapse
 *  - 13/12/2002   V1.3   (thof)
 *                        - reworked shape definition and mapping
 *                        - added straightforward implementations for
 *                          subarrays and slicing
 *  - 14/12/2002   V1.4   (thof)
 *                        - this now is a compilable source file (no longer a
 *                          header) the code contains not template parameter
 *                          and fits well in the binary library
 *  - 16/12/2002   V1.5   (thof)
 *                        - wolle asked for shift and setfirst functions...
 *  - 18/12/2002   V1.6   (thof)
 *                        - essential correction in collapse
 *  - 29/12/2002   V1.7   (thof)
 *                        - now provides basic Fortran style constructor
 *  - 15/05/2011   V1.8   (thof)
 *                        - added dense 1D array check
 * 
 * ============================================================================
 */

#define AFF_STRIDED_CC_VERSION \
  "AFF_STRIDED_DEF_H   V1.8"

#include<aff/lib/strided.h>
#include<aff/lib/error.h>

namespace aff {

  /*----------------------------------------------------------------------*/

  //! instantiate static member (otherwise the linker won't find it)
  const Tdim Strided::Mmax_dimen; 
  
  /*----------------------------------------------------------------------*/

  //! setup to given size and first index
  void Strided::setup_from_size(const TSizeVec& size, 
                                const Tsubscript& first,
                                const Tsubscript& shift)
  {
    TSizeVec one(1);
    AFF_assert((!inline_anylarger(one,size)),
               "ERROR (Strided::setup_from_size): size must be at least one");
    Mfirst=first;
    Mstride[0]=1;
    Mlast[0]=Mfirst[0]+size[0]-1;
    for(Tdim i=1; i<Mmax_dimen; i++) 
    { 
      Mstride[i]=Mstride[i-1]*size[i-1]; 
      Mlast[i]=Mfirst[i]+size[i]-1;
    }
    calculate_base(shift);
  }

  /*----------------------------------------------------------------------*/

  //! construct from given Fortran sizes
  Strided::Strided(const Tsize& s0, const Tsize& s1, const Tsize& s2,
                   const Tsize& s3)
  {
      TSizeVec size;
      size[0]=s0;
      size[1]=s1;
      size[2]=s2;
      size[3]=s3;
      setup_from_size(size, 1, 0);
  }

  /*----------------------------------------------------------------------*/

  //! construct do given first and last index
  Strided::Strided(const TIndexVec& first, const TIndexVec& last, 
                   const Tsubscript& shift)
  {
    AFF_assert((!inline_anylarger(first,last)),
               "ERROR (Strided): first index must be smaller than last");
    Mfirst=first;
    Mlast=last;
    Mstride[0]=1;
    for(Tdim i=1; i<Mmax_dimen; i++) 
    { 
      Mstride[i]=Mstride[i-1]*(1+Mlast[i-1]-Mfirst[i-1]); 
    }
    calculate_base(shift);
  }

  /*----------------------------------------------------------------------*/

  //! construct do given first and last index
  void Strided::calculate_base(const Tsubscript& offset) 
  {
    Mbase[0]=offset-Mfirst[0]*Mstride[0];
    for (Tdim i=1; i<Mmax_dimen; i++) 
    {
      Mbase[i]=Mbase[i-1]-Mfirst[i]*Mstride[i];
    }
  }

  /*----------------------------------------------------------------------*/

  //! calculate total size
  Tsize Strided::size() const
  {
    // could be done more elegant by inlining - but this will do it here
    TSizeVec size(Mlast);
    for (int i=0; i<Mmax_dimen; i++) { size[i]-=(Mfirst[i]-1); }
    return (inline_product(size));
  }

  /*----------------------------------------------------------------------*/
  
  //! subarray
  Strided& Strided::shrink(const TIndexVec& last)
  { 
    AFF_assert((!inline_anysmaller(last,Mfirst)),
               "ERROR (Strided::shrink): index must not be smaller than first");
    AFF_assert((!inline_anylarger(last,Mlast)),
               "ERROR (Strided::shrink): index must not be larger than last");
    Mlast=last;
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! subarray
  Strided& Strided::shrink(const TIndexVec& first, const TIndexVec& last)
  {
    AFF_assert((!(inline_anysmaller(last,Mfirst)
                  ||inline_anylarger(last,Mlast))),
               "ERROR (Strided::shrink): illegal last index vector");
    AFF_assert((!(inline_anysmaller(first,Mfirst)
                  ||inline_anylarger(first,Mlast))),
               "ERROR (Strided::shrink): illegal first index vector");
    AFF_assert((!inline_anysmaller(last,first)),
               "ERROR (Strided::shrink): last must not be smaller than first");
    Tsubscript offset=this->offset(first);
    Mfirst=first;
    Mlast=last;
    this->calculate_base(offset);
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! subarray
  Strided& Strided::shrink(const Tdim& i, 
                           const Tsubscript& first, 
                           const Tsubscript& last)
  {
    AFF_assert((i<Mmax_dimen),
               "ERROR (Strided::shrink): illegal dimension");
    AFF_assert(((Mfirst[i]<=first)&&(first<=last)&&(last<=Mlast[i])),
               "ERROR (Strided::shrink): index range error");
    // take the long way home
    TIndexVec newfirst(Mfirst);
    newfirst[i]=first;
    Tsubscript offset=this->offset(newfirst);
    Mfirst[i]=first;
    Mlast[i]=last;
    this->calculate_base(offset);
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! subarray
  Strided& Strided::shrink(const Tdim& i, 
                           const Tsubscript& last)
  {
    AFF_assert((i<Mmax_dimen),
               "ERROR (Strided::shrink): illegal dimension");
    AFF_assert(((Mfirst[i]<=last)&&(last<=Mlast[i])),
               "ERROR (Strided::shrink): illegal index value");
    Mlast[i]=last;
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  /*! \brief slice
   * \todo
   * This is implementation of slicing is straightforward but rather
   * inefficient. If you do a lot of slicing it might be worth to review and
   * rework the code.
   */
  Strided& Strided::collapse(const Tdim& i, const Tsubscript& index)
  {
    AFF_assert((i<Mmax_dimen),
               "ERROR (Strided::collapse): illegal dimension");
    AFF_assert(((Mfirst[i]<=index)&&(index<=Mlast[i])),
               "ERROR (Strided::collapse): index range error");
    this->shrink(i,index,index); 
    Tsubscript offset=this->first_offset();
    Tdim j=i+1;
    while (j<Mmax_dimen)
    {
      Mfirst[j-1]=Mfirst[j];
      Mlast[j-1]=Mlast[j];
      Mstride[j-1]=Mstride[j];
      j++;
    }
    Mfirst[Mmax_dimen-1]=1;
    Mlast[Mmax_dimen-1]=1;
    Mstride[Mmax_dimen-1]=this->size(Mmax_dimen-2)*Mstride[Mmax_dimen-2];
    this->calculate_base(offset);
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! shift
  Strided& Strided::shift(const Tdim& i, const Tsubscript& index)
  {
    AFF_assert((i<Mmax_dimen),
               "ERROR (Strided::shift): illegal dimension");
    Tsubscript offset=this->first_offset();
    Mfirst[i]+=index;
    Mlast[i]+=index;
    this->calculate_base(offset);
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! shift
  Strided& Strided::shift(const TIndexVec& index)
  {
    Tsubscript offset=this->first_offset();
    for (Tdim i=0; i<Mmax_dimen; i++)
    {
      Mfirst[i]+=index[i];
      Mlast[i]+=index[i];
    }
    this->calculate_base(offset);
    return(*this);
  }

  /*----------------------------------------------------------------------*/
  
  //! setfirst
  Strided& Strided::setfirst(const Tdim& i, const Tsubscript& index)
  {
    AFF_assert((i<Mmax_dimen),
               "ERROR (Strided::setfirst): illegal dimension");
    Tsubscript ishift=index-Mfirst[i];
    return(this->shift(i, ishift));
  }

  /*----------------------------------------------------------------------*/
  
  //! setfirst
  Strided& Strided::setfirst(const TIndexVec& index)
  {
    TIndexVec ishift(index);
    for (Tdim i=0; i<Mmax_dimen; i++)
    {
      ishift[i]-=Mfirst[i];
    }
    return(this->shift(ishift));
  }

  /*======================================================================*/

  bool aff::util::is_dense_1D_array(const aff::Strided& shape)
  {
    bool retval
      =((shape.size(1)==1)
        && (shape.size(2)==1)
        && (shape.size(3)==1)
        && (shape.stride(0)==1));
    return retval;
  } // bool is_dense_1D_array(const aff::Strided& shape)

} // namespace aff

/* ----- END OF strided.cc ----- */
