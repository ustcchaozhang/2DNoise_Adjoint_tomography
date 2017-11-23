/*! \file seifeclass.h
 * \brief provide all needed to use BasicFilter with seife code (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/07/2005
 * 
 * provide all needed to use BasicFilter with seife code (prototypes)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 04/07/2005   V1.0   Thomas Forbriger
 *  - 18/12/2007   V1.1   
 *                        - support debugging
 *                        - replace comma delimiter by whitespace
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SEIFECLASS_H_VERSION

#define TF_SEIFECLASS_H_VERSION \
  "TF_SEIFECLASS_H   V1.1"

#include<string>
#include<tsxx/filterbase.h>

namespace ts {

  namespace seife {

    /*! Butterworth lowpass (period t0, order o) */
    class LPB: public ts::filter::BasicFilter {
      public:
        LPB(double t0, int o):
          Mt0(t0), Mo(o) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
        int Mo;
    }; // class LPB

    /*! Butterworth highpass (period t0, order o) */
    class HPB: public ts::filter::BasicFilter {
      public:
        HPB(double t0, int o):
          Mt0(t0), Mo(o) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
        int Mo;
    }; // class HPB

    /*! 2nd order lowpass (period t0, damping h) */
    class LP2: public ts::filter::BasicFilter {
      public:
        LP2(double t0, double h):
          Mt0(t0), Mh(h) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0, Mh;
    }; // class LP2

    /*! 2nd order highpass (period t0, damping h) */
    class HP2: public ts::filter::BasicFilter {
      public:
        HP2(double t0, double h):
          Mt0(t0), Mh(h) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0, Mh;
    }; // class HP2

    /*! 2nd order bandpass (period t0, damping h) */
    class BP2: public ts::filter::BasicFilter {
      public:
        BP2(double t0, double h):
          Mt0(t0), Mh(h) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0, Mh;
    }; // class BP2

    /*! 1st order lowpass (period t0) */
    class LP1: public ts::filter::BasicFilter {
      public:
         LP1(double t0):
           Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
    }; // class LP1

    /*! 1st order highpass (period t0) */
    class HP1: public ts::filter::BasicFilter {
      public:
        HP1(double t0):
          Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
    }; // class HP1

    /*! integration (time constant t0) */
    class INT: public ts::filter::BasicFilter {
      public:
        INT(double t0):
          Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
    }; // class INT

    /*! 1st order highpass equalizer (former period t0s, new period t0) */
    class HE1: public ts::filter::BasicFilter {
      public:
        HE1(double t0s, double t0):
          Mt0s(t0s), Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0s, Mt0;
    }; // class HE1

    /*! 1st order lowpass equalizer (former period t0s, new period t0) */
    class LE1: public ts::filter::BasicFilter {
      public:
        LE1(double t0s, double t0):
          Mt0s(t0s), Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0s, Mt0;
    }; // class LE1

    /*! 2nd order highpass equalizer (former: period t0s and damping hs,
     *                               new: period t0 and damping h) 
     */
    class HE2: public ts::filter::BasicFilter {
      public:
        HE2(double t0s, double hs, double t0, double h):
          Mt0s(t0s), Mhs(hs), Mt0(t0), Mh(h) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0s, Mhs;
        double Mt0, Mh;
    }; // class HE2

    /*! 2nd order lowpass equalizer (former: period t0s and damping hs,
     *                              new: period t0 and damping h) 
     */
    class LE2: public ts::filter::BasicFilter {
      public:
        LE2(double t0s, double hs, double t0, double h):
          Mt0s(t0s), Mhs(hs), Mt0(t0), Mh(h) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0s, Mhs;
        double Mt0, Mh;
    }; // class LE2

    /*! detide with synthetic tides interpolated over ni samples */
    class TID: public ts::filter::BasicFilter {
      public:
        TID(int ni):
          Mni(ni) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        int Mni;
    }; // class TID

    /*! derivative (time constant t0) */
    class DIF: public ts::filter::BasicFilter {
      public:
        DIF(double t0):
          Mt0(t0) { }
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
      private:
        double Mt0;
    }; // class DIF

    /*! set baseline to first value */
    class FIRST: public ts::filter::BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
    }; // class FIRST

    //! create a seife filter
    ts::filter::Tfilterhandle make_seife_filter(std::string s,
                                                const bool& debug=false);

    //! print information on available filters
    void print_help(std::ostream& os);

  } // namespace seife

} // namespace ts

#endif // TF_SEIFECLASS_H_VERSION (includeguard)

/* ----- END OF seifeclass.h ----- */
