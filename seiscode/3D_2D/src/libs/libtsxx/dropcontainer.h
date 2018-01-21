/*! \file dropcontainer.h
 * \brief a container to drop samples into it (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/11/2006
 * 
 * a container to drop samples into it (prototypes)
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
#ifndef TF_DROPCONTAINER_H_VERSION

#define TF_DROPCONTAINER_H_VERSION \
  "TF_DROPCONTAINER_H   V1.0"

#include<string>
#include<tsxx/tsxx.h>
#include<tsxx/firfilters.h>

namespace ts {

  namespace drop {

    /*! Abstract base class
     *
     * The DropContainer class is an abstract base class.
     * It is used to collect time series samples and to decimate them on the
     * fly. Derived classes have to be defined for this interface in oder to
     * provide proper downsampling.
     *
     * The interface works as follows (code is given only for illustration; in
     * your application you will use different classes):
     * -# Declare the required downsamplers:
     *    \code
          ts::drop::DropDropContainer<int> d1(3);
          ts::drop::DropDropContainer<int> d2(2);
          ts::drop::DropDropContainer<int> d3(2);
          \endcode
     * -# Combine downsamplers:
     *    \code
          d1.attach(d2)->attach(d3);
          \endcode
          d1 will pass samples to a copy of d2 which again passes its output
          to a copy of d3.
     * -# Initizalize the linked list of downsamplers:
     *    \code
          d1.initialize(a.size());
          \endcode
     *    This will create a series container at the other end of the process.
     *    Initialization of the same downsampler list may be done for
     *    different numers of samples several times.
     * -# Perform downsampling.
     * -# Extract result through
          \code
          DUMP( d1.container() );
          \endcode
     * 
     * The full code as used in tstest.cc is as follows:
       \code
       aff::Series<int> a(1,36);
       for (int i=a.f(); i<=a.l(); ++i) { a(i)=i; }
       ts::drop::DropDropContainer<int> d1(3);
       ts::drop::DropDropContainer<int> d2(2);
       ts::drop::DropDropContainer<int> d3(2);
       d1.attach(d2)->attach(d3);
       d1.initialize(a.size());
       for (int i=a.f(); i<=a.l(); ++i) { d1.drop(a(i)); }
       DUMP( a );
       DUMP( d1.container() );
       \endcode
     *
     * The DropContainer class sets up a linked list of pointers. It keeps
     * track of initialized pointers and call the delete appropriately.
     *
     * The total downsampling factor is returned by member function
     * downsampling_factor() and the overall gain is provided by member
     * function gain().
     */
    template<class T>
    class DropContainer {
      public:
        typedef T Tvalue;
        typedef DropContainer<Tvalue>* PDropContainer;
        typedef aff::Series<Tvalue> Tseries;
        DropContainer(): Moutput(0) { }
        virtual ~DropContainer()
        { if (Moutput != 0) { delete Moutput; } }
        //! initialize for n samples input
        virtual void initialize(const int& n);
        //! this function accepts samples
        virtual void drop(const Tvalue& v)=0;
        virtual PDropContainer attach(const DropContainer& c)
        { 
          if (Moutput!=0) { Moutput->attach(c); }
          else { Moutput=c.clone(); }
          return this; 
        }
        virtual const typename Tseries::Tcoc& container() const
        { return(attached()->container()); }
        virtual int downsampling_factor() const
        { 
          if (Moutput!=0) { return(Moutput->downsampling_factor()); } 
          return 1;
        }
        virtual Tvalue gain() const
        { 
          if (Moutput!=0) { return(Moutput->gain()); } 
          return 1;
        }
      protected:
        virtual PDropContainer clone() const=0;
        virtual void initializenext(const int& n)=0;
        PDropContainer attached() const
        {
          TSXX_assert(Moutput != 0,
                      "DropContainer: not properly initialized!");
          return(Moutput);
        }
        PDropContainer Moutput;
    }; // class DropContainer

    /*----------------------------------------------------------------------*/

    /*! Series Container
     *
     * This class is used at the other end of the linked list of downsamplers
     * to collect the results.
     */
    template<class T>
    class SeriesDropContainer: public DropContainer<T> {
      public:
        typedef DropContainer<T> Tbase;
        typedef typename Tbase::Tvalue Tvalue;
        typedef DropContainer<Tvalue>* PDropContainer;
        typedef typename Tbase::Tseries Tseries;
        typedef aff::Tsubscript Tsubscript;
        SeriesDropContainer(): Ms(1), Mi(0) { }
        virtual ~SeriesDropContainer() { }
        virtual void drop(const Tvalue& v) 
        { 
          TSXX_assert(Mi <= Ms.l(),
                      "SeriesDropContainer: index out of bounds");
          Ms(Mi)=v; 
          ++Mi; 
        }
        virtual int downsampling_factor() const { return(1); }
        virtual Tvalue gain() const { return(1); }
        virtual PDropContainer attach(const Tbase& c)
        { TSXX_abort("SeriesDropContainer: "
                     "do not attach after initialization!"); }
      protected:
        virtual PDropContainer clone() const
        { 
          SeriesDropContainer<T>* retval(new SeriesDropContainer);
          retval->initializenext(Ms.size());
          return(retval);
        }
        virtual void initializenext(const int& n) 
        { Ms=Tseries(n); Mi=Ms.f(); }
        virtual const typename Tseries::Tcoc& container() const { return(Ms); }
      private:
        Tseries Ms;
        Tsubscript Mi;
    }; // class SeriesDropContainer

    /*----------------------------------------------------------------------*/

    /*! Simple decimating Drop Container
     *
     * This class performs downsampling without low-pass filtering. Its
     * rathers used as an example than as a serious tool of signal processing.
     * The downsampling ratio must be passed to the constructor.
     */
    template<class T>
    class DropDropContainer: public DropContainer<T> {
      public:
        typedef DropContainer<T> Tbase;
        typedef typename Tbase::Tvalue Tvalue;
        typedef DropContainer<Tvalue>* PDropContainer;
        DropDropContainer(const int& n): Mn(n), Mi(0) { }
        virtual ~DropDropContainer() { }
        virtual void drop(const Tvalue& v)
        {
          --Mi;
          if (Mi<1) 
          {
            Mi=Mn;
            Tbase::attached()->drop(v);
          }
        }
        virtual int downsampling_factor() const 
        { return(Mn*Tbase::downsampling_factor()); }
      protected:
        virtual PDropContainer clone() const
        { 
          DropDropContainer<T>* retval(new DropDropContainer(Mn)); 
          return(retval);
        }
        virtual void initializenext(const int& n)
        { Tbase::Moutput->initialize(n/Mn); Mi=0; }
      private:
        int Mn, Mi;
    }; // class DropDropContainer

    /*----------------------------------------------------------------------*/

    /*! Simple allpass Drop Container
     *
     * This class simply passes all samples.
     */
    template<class T>
    class PassDropContainer: public DropContainer<T> {
      public:
        typedef DropContainer<T> Tbase;
        typedef typename Tbase::Tvalue Tvalue;
        typedef DropContainer<Tvalue>* PDropContainer;
        PassDropContainer() { }
        virtual ~PassDropContainer() { }
        virtual void drop(const Tvalue& v)
        { Tbase::attached()->drop(v); }
      protected:
        virtual PDropContainer clone() const
        { 
          PassDropContainer<T>* retval(new PassDropContainer); 
          return(retval);
        }
        virtual void initializenext(const int& n)
        { Tbase::Moutput->initialize(n); }
      private:
    }; // class PassDropContainer

    /*----------------------------------------------------------------------*/

    /*! Decimation FIR drop container
     *
     * This class decimates the input by applying an FIR filter
     */
    template<class T>
    class FIRDropContainer: public DropContainer<T> {
      public:
        typedef DropContainer<T> Tbase;
        typedef typename Tbase::Tvalue Tvalue;
        typedef DropContainer<Tvalue>* PDropContainer;
        typedef ts::fir::FIRfilter<Tvalue> Tfirfilter;
        FIRDropContainer(const std::string& name): 
          Mf(new Tfirfilter(name)), Mi(Mf->fir().delay) { }
        virtual ~FIRDropContainer() { delete Mf; }
        virtual void drop(const Tvalue& v);
      protected:
        virtual PDropContainer clone() const
        { 
          FIRDropContainer<T>* retval(new FIRDropContainer(Mf->fir().name)); 
          return(retval);
        }
        virtual void initializenext(const int& n)
        { 
          Tbase::Moutput->initialize(n/Mf->fir().decimation_factor); 
          Mf->clear();
          Mi=Mf->fir().delay;
        }
        virtual int downsampling_factor() const 
        { return(Mf->fir().decimation_factor*Tbase::downsampling_factor()); }
        virtual Tvalue gain() const 
        { return(Mf->fir().gain*Tbase::gain()); }
      private:
        Tfirfilter* Mf;
        int Mi;
    }; // class FIRDropContainer

    /*======================================================================*/

    template<class T>
      void DropContainer<T>::initialize(const int& n)
      {
        if (Moutput == 0)
        { Moutput=new SeriesDropContainer<T>; } 
        this->initializenext(n); 
      }

    /*----------------------------------------------------------------------*/

    template<class T>
      void FIRDropContainer<T>::drop(const Tvalue& v)
      {
        Mf->push(v);
        --Mi;
        if (Mi<1)
        {
          Mi=Mf->fir().decimation_factor; 
          Tbase::attached()->drop(Mf->pop());
        }
      }

  } // namespace drop

} // namespace ts

#endif // TF_DROPCONTAINER_H_VERSION (includeguard)

/* ----- END OF dropcontainer.h ----- */
