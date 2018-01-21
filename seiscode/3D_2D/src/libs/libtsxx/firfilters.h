/*! \file firfilters.h
 * \brief all we need to implement FIR filters (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/11/2006
 * 
 * all we need to implement FIR filters (prototypes)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 21/11/2006   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FIRFILTERS_H_VERSION

#define TF_FIRFILTERS_H_VERSION \
  "TF_FIRFILTERS_H   V1.0   "

#include<iostream>
#include<string>
#include<aff/series.h>
#include<aff/iterator.h>
#include<tsxx/error.h>

namespace ts {

  namespace fir {

    /*! container for FIR filters and their description
     */
    struct FIR {
      const char* name;
      const char* description;
      const int points;
      const int delay;
      const bool symmetric;
      const int decimation_factor;
      const double gain;
      const double* c;
    }; // struct FIR

    /*! SeisComp Filters
     */
    extern const FIR SeisCompMP;
    extern const FIR SeisCompLP;
    extern const FIR SeisCompVLP;

    /*! collection of FIR filters
     */
    extern const FIR* FIRfilters[];

    /*----------------------------------------------------------------------*/

    /*! dump filter characteristics
     */
    std::ostream& operator<<(std::ostream& os, const FIR& fir);

    /*----------------------------------------------------------------------*/

    /*! print information on available FIR filters
     */
    void help(std::ostream& os);

    /*----------------------------------------------------------------------*/

    /*! Class to evaluate a FIR filter
     *
     * Add new sample through member function push().
     * Member function pop() returns result.
     *
     * \note
     * This class highly depends on the property of AFF container iterators to
     * by cyclic.
     */
    template<class T>
      class FIRfilter {
        public:
          typedef T Tvalue;
          typedef aff::Series<Tvalue> Tseries;
          typedef aff::Iterator<Tseries> Titerator;
          FIRfilter(const FIR* fir): Mfir(fir), Ms(fir->points), Mi(Ms) 
          { this->clear(); }
          FIRfilter(const std::string name): Mfir(0), Ms(1), Mi(Ms)
          { 
            int i=0;
            while (FIRfilters[i]!=0)
            { 
              if (name == FIRfilters[i]->name) { Mfir=FIRfilters[i]; } 
              ++i;
            }
            TSXX_assert(Mfir != 0, "FIRfilter: unknown FIR filter name"); 
            Ms=Tseries(Mfir->points);
            Mi=Titerator(Ms);
            this->clear(); 
          }
          void push(const Tvalue& v)
          {
            *Mi = v;
            ++Mi;
          }
          Tvalue pop() const
          {
            Titerator Ml=Mi;
            double sum=0;
            if (Mfir->symmetric)
            {
              for (int i=0; i<Mfir->points / 2; ++i)
              { sum += *Ml * Mfir->c[i] * Mfir->gain; --Ml; }
              for (int i= Mfir->points/2 -1; i>=0; --i)
              { sum += *Ml * Mfir->c[i] * Mfir->gain; --Ml; }
            }
            else
            {
              for (int i=0; i<Mfir->points; ++i)
              { sum += *Ml * Mfir->c[i] * Mfir->gain; --Ml; }
            }
            return(Tvalue(sum));
          }
          void clear()
          {
            Mi.tofirst();
            while (Mi.valid()) { *Mi = 0; ++Mi; }
            Mi.tofirst();
          }
          const FIR& fir() const { return(*Mfir); }
        private:
          const FIR* Mfir;
          Tseries Ms;
          Titerator Mi;
      }; // class FIRfilter

  } // namespace fir

} // namespace ts

#endif // TF_FIRFILTERS_H_VERSION (includeguard)

/* ----- END OF firfilters.h ----- */
