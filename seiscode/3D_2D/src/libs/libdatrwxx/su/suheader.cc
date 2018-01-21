/*! \file suheader.cc
 * \brief handle a Seismic Unix trace header (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/11/2010
 * 
 * handle a Seismic Unix trace header (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/11/2010   V1.0   Thomas Forbriger
 *  - 17.12.2010   V1.1   bug fixed in scalcof() and scalelf()
 *  - 17.07.2011   V1.2   make scalel==0 non-fatal
 *  - 24.08.2011   V1.3   static cast to integer type just truncates
 *                        round sampling interval properly to nearest integer
 *  - 21/01/2012   V1.4   provide online help regarding header fields
 *  - 22/01/2012   V1.5
 *                        - pass control parameters to ScalCoo and Coordinates
 *                        - delegate to scale value handling functions
 *                        - indicate ultrasonic data
 *                        - renamed absdelay -> absdelrt
 *  - 23/01/2012   V1.6
 *                        - scaling of delay for ultrasonic data
 *                        - reading of ultrasonic data fully supported
 *                        - scaling of dt and delrt when setting header values
 *                          for ultrasonic data is implemented
 *                 V1.7
 *                        - added debug output
 *                        - check ultrasonic vs. seismic scaling for dt and
 *                          delrt; works fine after correction of a few errors
 * 
 * ============================================================================
 */
#define DATRW_SUHEADER_CC_VERSION \
  "DATRW_SUHEADER_CC   V1.7"

#include <datrwxx/suheader.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/sucomanager.h>
#include <sstream>
#include <cstring>
#include <cmath>
#include <climits>

namespace datrw  {

  namespace su {


    /*----------------------------------------------------------------------*/

    void SUheader::read(std::istream& is)
    {
      char *ipointer=reinterpret_cast<char *>(&Mheader);
      DATRW_Xassert(is.read(ipointer, sizeof(TraceHeaderStruct)),
                    "ERROR (SUheader::read): reading SU header",
                    SUReadException);
      DATRW_debug(Mdebug, "SUheader::read",
                  "some fields upon input:\n"
                  "trid="<<Mheader.trid<<" "
                  "scalco="<<Mheader.scalco<<" "
                  "scalel="<<Mheader.scalel<<"\n");
      fixscalevalue(Mheader.scalco,
                    Mheadercontrol.spatialsampling.bestrict);
      fixscalevalue(Mheader.scalel,
                    Mheadercontrol.spatialsampling.bestrict);
      DATRW_debug(Mdebug, "SUheader::read",
                  "some fields upon return:\n"
                  "trid="<<Mheader.trid<<" "
                  "scalco="<<Mheader.scalco<<" "
                  "scalel="<<Mheader.scalel<<"\n");
    } // void SUheader::read(std::istream& is)

    /*----------------------------------------------------------------------*/

    void SUheader::write(std::ostream& os) const
    {
      const char *ipointer=reinterpret_cast<const char *>(&Mheader);
      DATRW_assert(os.write(ipointer, sizeof(TraceHeaderStruct)),
                   "ERROR (SUheader::write): writing SU header");
      DATRW_debug(Mdebug, "SUheader::write(std::ostream& os)",
                  "wrote "
                  << sizeof(TraceHeaderStruct) << " bytes; "
                  << "os.good() returns " << os.good());
    } // void write(std::ostream& os) const
      
    /*----------------------------------------------------------------------*/

    //! constructor
    SUheader::SUheader(const datrw::su::options::SUHeaderControl& hc,
                       const bool& debug)
      : Mheadercontrol(hc), Mdebug(debug)
    {
      DATRW_debug(Mdebug, "SUheader::SUheader",
                  "hc.spatialsampling.scalco "
                  << hc.spatialsampling.scalco);
      this->setdefaults();
    } // SUheader::SUheader()

    /*----------------------------------------------------------------------*/

    //! clear header struct
    void SUheader::clear()
    {
      memset(static_cast<void *>(&Mheader), 0, sizeof(TraceHeaderStruct));
      Mhaswid2=false;
      Mhasinfo=false;
      Mhassrce=false;
      DATRW_debug(Mdebug, "SUheader::clear()", "cleared all fields");
    } // void SUheader::clear()

    /*----------------------------------------------------------------------*/

    //! set defaults to struct
    void SUheader::setdefaults()
    {
      this->clear();
      Mheader.scalco=Mheadercontrol.spatialsampling.scalco;
      Mheader.scalel=Mheadercontrol.spatialsampling.scalco;
      fixscalevalue(Mheader.scalco,
                    Mheadercontrol.spatialsampling.bestrict);
      fixscalevalue(Mheader.scalel,
                    Mheadercontrol.spatialsampling.bestrict);
      Mheader.trid=1;
      Mheader.cdpt=1;
      Mheader.counit=1;
      Mheader.gain=1;
      Mheader.igc=1;
      Mheader.igi=1;
      Mheader.corr=1;
    } // void SUheader::setdefaults()

    /*----------------------------------------------------------------------*/

    //! return sampling interval
    double SUheader::dt() const
    {
      double factor=1.e-6;
      if (this->isultrasonic()) { factor=1.e-9; }
      return (static_cast<double>(Mheader.dt)*factor);
    } // double SUheader::dt() const

    /*----------------------------------------------------------------------*/

    //! true if header defines ultrasonic data sampling
    bool SUheader::isultrasonic() const
    {
      bool retval=false;
      DATRW_assert(Mheader.d1>=0.,
                   "negative d1 value (header byte offset 180) is illegal");
      if (Mheader.d1 > subformat::def::thresholdzero)
      {
        float seismicdt=static_cast<double>(Mheader.dt)*1.e-6;
        float ultrasonicdt=static_cast<double>(Mheader.dt)*1.e-9;
        float seismicres=1.-(seismicdt/Mheader.d1);
        float ultrasonicres=1.-(ultrasonicdt/Mheader.d1);
        seismicres=seismicres<0 ? -seismicres : seismicres;
        ultrasonicres=ultrasonicres<0 ? -ultrasonicres : ultrasonicres;
        if (seismicres > subformat::def::thresholdcmp)
        {
          DATRW_assert(ultrasonicres < subformat::def::thresholdcmp,
                       "d1 value (header byte offset 180) neither matches "
                       "seismic nor ultrasonic data");
          retval=true;
        }
      }
      return(retval);
    } // bool SUheader::isultrasonic() const

    /*----------------------------------------------------------------------*/

    /*! return factor defined by scalel
     *
     * \todo better handle non-standard values only if appropriate file type
     * modifiers are set; file type modifiers still have to be implemented
     */
    double SUheader::scalelf() const
    {
      return(scalefactor(Mheader.scalel,
                         Mheadercontrol.spatialsampling.bestrict));
    } // double SUheader::scalelf() const

    /*----------------------------------------------------------------------*/

    //! return scaling factor defined by scalco
    double SUheader::scalcof() const
    {
      return(scalefactor(Mheader.scalco,
                         Mheadercontrol.spatialsampling.bestrict));
    } // double SUheader::scalcof() const

    /*----------------------------------------------------------------------*/

    //! return date of first sample
    libtime::TAbsoluteTime SUheader::dateoffirstsample() const
    {
      libtime::TAbsoluteTime date=this->dateofshot();
      if (this->delayispositive())
      {
        date += this->delay();
      } else {
        date -= this->delay();
      }
      return(date);
    } // libtime::TAbsoluteTime SUheader::dateoffirstsample() const

    /*----------------------------------------------------------------------*/

    // is delay positive
    bool SUheader::delayispositive() const
    {
      return(Mheader.delrt>=0);
    } // bool SUheader::delayispositive() const

    /*----------------------------------------------------------------------*/

    // absolute delay value
    int SUheader::absdelrt() const
    {
      return(Mheader.delrt>=0 ? Mheader.delrt : -Mheader.delrt);
    } // int SUheader::absdelrt() const

    /*----------------------------------------------------------------------*/

    /*! return recording delay
     *
     * libtime::TRelativeTime has microsecond resolution such that the return
     * type of the function is also appropriate for ultrasonic data
     */
    libtime::TRelativeTime SUheader::delay() const
    {
      double factor=1.e-3;
      if (this->isultrasonic()) { factor=1.e-6; }
      return(libtime::double2time(factor
                                  *static_cast<double>(this->absdelrt())));
    } // libtime::TRelativeTime SUheader::delay() const

    /*----------------------------------------------------------------------*/

    //! return date of shot
    libtime::TAbsoluteTime SUheader::dateofshot() const
    {
      libtime::TAbsoluteTime retval(Mheader.year, 0,
                                    Mheader.day,
                                    Mheader.hour,
                                    Mheader.minute,
                                    Mheader.sec);
      retval.setdoy(Mheader.day);
      return(retval);
    } // libtime::TAbsoluteTime SUheader::dateofshot() const

    /*----------------------------------------------------------------------*/

    //! return SRCE line
    ::sff::SRCE SUheader::srce() const
    {
      DATRW_assert((Mheader.counit == 1) || (Mheader.counit == 0),
                   "ERROR (SUheader::srce): coordinate units not supported");
      ::sff::SRCE retval;
      double scalcof=this->scalcof();
      retval.cs=::sff::CS_cartesian;
      retval.cx=static_cast<double>(Mheader.sx)*scalcof;
      retval.cy=static_cast<double>(Mheader.sy)*scalcof;
      retval.cz=-static_cast<double>(Mheader.sdepth)*this->scalelf();
      retval.date=this->dateofshot();
      return(retval);
    } // ::sff::SRCE SUheader::srce() const

    /*----------------------------------------------------------------------*/

    //! return INFO line
    ::sff::INFO SUheader::info() const
    {
      DATRW_assert((Mheader.counit == 1) || (Mheader.counit == 0),
                   "ERROR (SUheader::info): coordinate units not supported");
      ::sff::INFO retval;
      double scalcof=this->scalcof();
      retval.cs=::sff::CS_cartesian;
      retval.cx=static_cast<double>(Mheader.gx)*scalcof;
      retval.cy=static_cast<double>(Mheader.gy)*scalcof;
      retval.cz=static_cast<double>(Mheader.gelev)*this->scalelf();
      retval.nstacks=Mheader.nvs;
      return(retval);
    } // ::sff::INFO SUheader::info() const

    /*----------------------------------------------------------------------*/

    //! return WID2 line
    ::sff::WID2 SUheader::wid2() const
    {
      DATRW_assert(Mheader.trid == 1,
                   "ERROR (SUheader::wid2): no seismic data");
      ::sff::WID2 retval;
      retval.nsamples=Mheader.ns;
      retval.dt=this->dt();
      retval.date=this->dateoffirstsample();
      std::ostringstream oss;
      /* set channel and station to tracf:
       * Trace number within original field record
       */
      oss.width(3); oss.fill('0'); oss << Mheader.tracf;
      retval.channel=oss.str();
      retval.station=oss.str();
      oss.str("");
      /* set auxid to fldr
       * Original field record number
       */
      oss.width(3); oss.fill('0'); oss << Mheader.fldr;
      retval.auxid=oss.str();
      return(retval);
    } // ::sff::WID2 SUheader::wid2() const

    /*----------------------------------------------------------------------*/

    //! set values from SRCE line
    void SUheader::set(const ::sff::SRCE &srce)
    {
      DATRW_debug(Mdebug, "SUheader::set(const ::sff::SRCE &srce)",
                  "set SRCE values:"
                  " cx: " << srce.cx <<
                  " cy: " << srce.cy <<
                  " cz: " << srce.cz);
      DATRW_assert(srce.cs == ::sff::CS_cartesian,
                   "ERROR (SUheader::set SRCE): "
                   "can only handle cartesian cooridnates");
      ::datrw::su::Coordinates coo(Mheadercontrol.spatialsampling, Mdebug);
      coo.getvaluesfrom(Mheader);
      coo.sx.set(srce.cx);
      coo.sy.set(srce.cy);
      coo.sdepth.set(-srce.cz);
      coo.setvaluesin(Mheader);
      Msrcedate=srce.date;
      Mhassrce=true;
      this->settimes();
    } // void SUheader::set(const ::sff::SRCE &srce)

    /*----------------------------------------------------------------------*/

    //! set values from INFO line
    void SUheader::set(const ::sff::INFO &info)
    {
      DATRW_debug(Mdebug, "SUheader::set(const ::sff::INFO &info)",
                  "set INFO values:"
                  " cx: " << info.cx <<
                  " cy: " << info.cy <<
                  " cz: " << info.cz);
      DATRW_assert(info.cs == ::sff::CS_cartesian,
                   "ERROR (SUheader::set INFO): "
                   "can only handle cartesian cooridnates");
      DATRW_debug(Mdebug, "SUheader::set(const ::sff::INFO &info)",
                  "values upon entry:"
                  << DATRW_value(Mheader.gx) << ", "
                  << DATRW_value(Mheader.gy) << ", "
                  << DATRW_value(Mheader.scalco));
      ::datrw::su::Coordinates coo(Mheadercontrol.spatialsampling);
      coo.getvaluesfrom(Mheader);
      coo.gx.set(info.cx);
      coo.gy.set(info.cy);
      coo.gelev.set(info.cz);
      coo.setvaluesin(Mheader);
      Mheader.nvs=static_cast<short>(info.nstacks);
      DATRW_debug(Mdebug, "SUheader::set(const ::sff::INFO &info)",
                  "values upon exit:"
                  << DATRW_value(Mheader.gx) << ", "
                  << DATRW_value(Mheader.gy) << ", "
                  << DATRW_value(Mheader.scalco));
    } // void SUheader::set(const ::sff::INFO &info)

    /*----------------------------------------------------------------------*/

    //! set values from WID2 line
    void SUheader::set(const ::sff::WID2 &wid2)
    {
      DATRW_debug(Mdebug, "SUheader::set(const ::sff::WID2 &wid2)",
                  "set WID2 values");
      Mheader.ns=static_cast<unsigned short>(wid2.nsamples);
      Mwid2dt=wid2.dt;
      Mwid2date=wid2.date;
      Mhaswid2=true;
      {
        std::istringstream iss(wid2.channel);
        iss >> Mheader.tracf;
      }
      {
        std::istringstream iss(wid2.auxid);
        iss >> Mheader.fldr;
      }
      this->settimes();
    } // void SUheader::set(const ::sff::WID2 &wid2)

    /*----------------------------------------------------------------------*/

    //! set date
    void SUheader::set(const libtime::TAbsoluteTime &date)
    {
      Mheader.year=static_cast<short>(date.year());
      Mheader.day=static_cast<short>(date.doy());
      Mheader.hour=static_cast<short>(date.hour());
      Mheader.minute=static_cast<short>(date.minute());
      Mheader.sec=static_cast<short>(date.second());
    } // void SUheader::set(const libtime::TAbsoluteTime &date)

    /*----------------------------------------------------------------------*/

    //! set time values
    void SUheader::settimes()
    {
      // report inpu data for debugging
      DATRW_debug(Mdebug,
                  "ERROR SUheader::settimes: ",
                  "set times: Mhaswid2 " << Mhaswid2
                  << " Mhassrce " << Mhassrce);
      DATRW_debug(Mdebug && Mhaswid2,
                  "ERROR SUheader::settimes: ",
                  "WID2 time: " << Mwid2date.timestring()
                  << " WID2 dt: " << Mwid2dt);
      DATRW_debug(Mdebug && Mhassrce,
                  "ERROR SUheader::settimes: ",
                  "SRCE time: " << Msrcedate.timestring());
      // check whether evaluation of this function makes sense at all
      DATRW_assert(Mhaswid2 || Mhassrce,
                   "ERROR SUheader::settimes: "
                   "function called at wrong instance");

      // ultrasonic flag is required for setting delrt
      bool ultrasonic=false;
      // set sampling interval if WID2 data is present
      // ---------------------------------------------
      if (Mhaswid2)
      {
        DATRW_assert(this->Mwid2dt>0,
                     "received non-positive sampling interval");
        // evaluate dt and check for ultrasonic data
        double dtfactor=1.e6;
        double scaleddt=nearbyint(dtfactor*this->Mwid2dt);
        if (Mheadercontrol.temporalsampling.forceseismic
            || Mheadercontrol.temporalsampling.forceultrasonic)
        {
          if (Mheadercontrol.temporalsampling.forceultrasonic)
          {
            ultrasonic=true;
            dtfactor=1.e9;
            scaleddt=nearbyint(dtfactor*this->Mwid2dt);
            DATRW_debug(Mdebug,
                        "SUheader::settimes",
                        "ultrasonic scaling is forced");
          }
          else
          {
            DATRW_debug(Mdebug,
                        "SUheader::settimes",
                        "seismic scaling is forced");
          }
        }
        else
        {
          if ((!ultrasonic) && scaleddt <1.)
          {
            ultrasonic=true;
            dtfactor=1.e9;
            scaleddt=nearbyint(dtfactor*this->Mwid2dt);
            DATRW_debug(Mdebug,
                        "SUheader::settimes",
                        "ultrasonic scaling because of dt "
                        << scaleddt << "ns value");
          }
          else
          {
            DATRW_debug(Mdebug,
                        "SUheader::settimes",
                        "seismic scaling because of dt "
                        << scaleddt << "us value");
          }
        }
        if (scaleddt > USHRT_MAX)
        {
          std::cerr << "received sampling interval: " << scaleddt;
          if (ultrasonic)
          {
            std::cerr << " ns";
          }
          else
          {
            std::cerr << " us";
          }
          std::cerr << std::endl;
        }
        DATRW_assert(scaleddt <= USHRT_MAX,
                     "ERROR (SUheader::set): "
                     "sampling interval cannot be represented by "
                     "a field of type unsigend short");
        Mheader.dt=static_cast<unsigned short>(scaleddt);
        Mheader.d1=0.;
        if (ultrasonic) 
        { Mheader.d1=static_cast<double>(Mheader.dt)/dtfactor; }
      }

      // set source time and trigger delay
      // ---------------------------------
      if (Mhaswid2 && Mhassrce)
      {
        libtime::TRelativeTime del=Mwid2date-Msrcedate;
        this->set(Msrcedate);
        double delfactor=1.e3;
        if (ultrasonic) { delfactor=1.e6; }
        double delvalue=std::floor(delfactor*libtime::time2double(del));
        if (Mwid2date<Msrcedate) { delvalue *= -1.; }
        DATRW_assert(((delvalue >= SHRT_MIN) && (delvalue <= SHRT_MAX)),
                     "ERROR (SUheader::set): "
                     "trigger delay cannot be represented by "
                     "a field of type short.");
        this->Mheader.delrt=static_cast<short>(delvalue);
      }
      else if (Mhaswid2)
      {
        this->set(Mwid2date);
        this->Mheader.delrt=0;
      }
      else if (Mhassrce)
      {
        this->set(Msrcedate);
        this->Mheader.delrt=0;
      }
    } // void SUheader::settimes();

    /*----------------------------------------------------------------------*/
    
    void SUheader::help(std::ostream& os) 
    {
      os <<
        "The library uses only few header fields. Upon reading the\n"
        "following fields are used:\n";
      os <<
        "wid2.nsamples = header.ns\n"
        "wid2.dt       = header.dt*1.e-6\n"
        "wid2.date     = srce.date+header.delrt*1.e-3\n"
        "wid2.channel  = header.tracf\n"
        "                (tracf = Trace number within original field record)\n"
        "wid2.station  = header.tracf\n"
        "wid2.auxid    = header.fldr\n"
        "                (fldr = Original field record number)\n";
      os <<
        "info.cx       = header.gx*scalcof\n"
        "info.cy       = header.gy*scalcof\n"
        "info.cz       = header.gelev*scalelf\n"
        "info.nstacks  = header.nvs\n";
      os <<
        "srce.cx       = header.sx*scalcof\n"
        "srce.cy       = header.sy*scalcof\n"
        "srce.cz       = header.sdepth*scalelf\n"
        "srce.date     = date(year,day,hour,minute,sec)\n";
      os <<
        "header.scalco and header.scalel are as defined for the SEG-Y "
        "format:\n"
        "http://www.seg.org/SEGportalWEBproject/prod/SEG-Publications/Pub-Technical-Standards/Documents/seg_y_rev1.pdf\n"
        "except when their modulus is smaller than 10 and larger than 0.\n"
        "In this case the scaling factors scalcof and scalelf are taken to be\n"
        "10 to the power of header.scalco or header.scalel, respectively\n";
      os <<
        "Notice that in contrast to the definition of the SeismicUn*x format\n"
        "this library also supports high frequency sampling for ultrasonic\n"
        "data. A special meaning of header fields was defined within the TOAST\n"
        "project:\n";
      os <<
        "Small sampling interval (smaller than 1 microsecond) as used in\n"
        "ultrasonic recordings are to be stored in SeismicUn*x data format\n"
        "within TOAST as follows:\n";
      os <<
        "1. Time values for ultrasonic data will be given in nanoseconds for\n"
        "   dt and microseconds for delrt.\n"
        "2. This applies to fields\n"
        "   a) dt (byte# 117-118): sample interval\n"
        "   b) delrt (byte# 109-110): delay recording time\n";
      os <<
        "3. Time units other than microseconds or nanoseconds for dt and\n"
        "   milliseconds or microseconds for delrt are not allowed.\n"
        "4. Field d1 (byte# 181-184) in TOAST data will be\n"
        "   a) either zero, indicating standard seismic data with a\n"
        "      micro seconds time scale or\n"
        "   b) provide the sampling interval in seconds, thus indicating\n"
        "      i)  either standard seismic data, if d1 in seconds matches\n"
        "          dt if the latter is taken in microseconds or\n"
        "      ii) ultrasonic data, if d1 in seconds matches dt if the\n"
        "          latter is taken in nanoseconds and delrt is taken\n"
        "          in microseconds\n";
      os <<
        "This way it is possible to use SU tools to sort traces or similar\n"
        "or even waveform filters. The user just has to take care to pass\n"
        "filter frequencies in kHz rather than Hz in the case of ultrasonic\n"
        "data.\n";
    } // void SUheader::help(std::ostream& os) const

  } // namespace su

} // namespace datrw

/* ----- END OF suheader.cc ----- */
