/*! \file seedstructs.cc
 * \brief provide memeber functions for SEED structs (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/03/2006
 * 
 * provide memeber functions for SEED structs (implementation)
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
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/03/2006   V1.0   Thomas Forbriger
 *  - 09/05/2006   V1.1   introduced Steim 2 code
 *  - 11/05/2006   V1.2   correction for reading bytes in Steim 2 Word
 *  - 29/06/2007   V1.3   no longer use libtfxx
 * 
 * ============================================================================
 */
#define DATRW_SEEDSTRUCTS_CC_VERSION \
  "DATRW_SEEDSTRUCTS_CC   V1.3"

#include<datrwxx/seedstructs.h>
#include<datrwxx/error.h>
#include<datrwxx/bytesex.h>

#include<iostream>
#include<iomanip>

namespace datrw {

  namespace mseed {

    namespace SEED {

#define SWAPIT( A ) A=datrw::util::swap(A)

      void BTIME::swap() 
      {
        SWAPIT(year);
        SWAPIT(doy);
        SWAPIT(hour);
        SWAPIT(min);
        SWAPIT(sec);
        SWAPIT(zero);
        SWAPIT(tmilsec);
      } // void BTIME::swap() 

      void FixedDataRecordHeader::swap() 
      {
        stime.swap();
        SWAPIT(nsamp); 
        SWAPIT(srate);
        SWAPIT(srmult);
        SWAPIT(tcorr);
        SWAPIT(dbeg);
        SWAPIT(fblock);
      } // void FixedDataRecordHeader::swap() 

      void DataRecordBlocketteHeader::swap() 
      {
        SWAPIT(type);
        SWAPIT(next);
      } // void DataRecordBlocketteHeader::swap() 

      void DataOnlySEEDBlockette::swap() 
      {
        blocketteheader.swap();
      } // void DataOnlySEEDBlockette::swap() 

      void DataExtensionBlockette::swap() 
      {
        blocketteheader.swap();
      } // void DataExtensionBlockette::swap() 

      /*======================================================================*/
      
      SteimFrame::ESteimControl SteimFrame::ctrl(const int& i) const
      {
        unsigned int code = ((this->control() >> ((nwords-i-1)*2)) & 0x03); 
        return(ESteimControl(code));
      } // SteimFrame::ESteimControl SteimFrame::ctrl(const int& i) const

      /*======================================================================*/

      void Steim1Frame::swap() 
      {
        Steim1Word w;
        SWAPIT(this->Mdata.control);
        for (int i=0; i<nwords; ++i)
        {
          w.fw=this->Mdata.word[i]; 
          switch(this->ctrl(i)) {
            case Fspecial:
            case Ffw:
              SWAPIT(w.fw);
              break;
            case Fbyte:
              SWAPIT(w.byte[0]);
              SWAPIT(w.byte[1]);
              SWAPIT(w.byte[2]);
              SWAPIT(w.byte[3]);
              break;
            case Fhw:
              SWAPIT(w.hw[0]);
              SWAPIT(w.hw[1]);
              break;
            default:
              DATRW_abort("ERROR (Steim1Frame::swap()): illegal code");
          }
          this->Mdata.word[i]=w.fw; 
        }
      } // virtual void Steim1Frame::swap() 

      /*======================================================================*/

      void Steim2Frame::swap() 
      {
        Steim1Word w;
        SWAPIT(this->Mdata.control);
        for (int i=0; i<nwords; ++i)
        {
          w.fw=this->Mdata.word[i]; 
          switch(this->ctrl(i)) {
            case Fspecial:
            case Fdnib1:
            case Fdnib2:
              SWAPIT(w.fw);
              break;
            case Fbyte:
              SWAPIT(w.byte[0]);
              SWAPIT(w.byte[1]);
              SWAPIT(w.byte[2]);
              SWAPIT(w.byte[3]);
              break;
            default:
              DATRW_abort("ERROR (Steim2Frame::swap()): illegal code");
          }
          this->Mdata.word[i]=w.fw; 
        }
      } // virtual void Steim2Frame::swap() 

      /*----------------------------------------------------------------------*/

      void SteimFrame::reset()
      {
        Miword=0;
        Midiff=0;
        Mvalid=true;
        setn();
      } // void SteimFrame::reset()

      /*----------------------------------------------------------------------*/

      void SteimFrame::next()
      {
        ++Midiff;
        if (Midiff == Mn) { ++Miword; Midiff=0; }
        if (Miword == Steim1Frame::nwords) { Mvalid=false; }
        if (Mvalid) { setn(); }
      } // void SteimFrame::next()

      /*======================================================================*/
      /* Steim 1 Reader code
       * -------------------
       */

      void Steim1Frame::setn()
      {
        switch(this->ctrl()) {
          case SteimFrame::Fspecial:
            Mn=1;
            break;
          case SteimFrame::Fbyte:
            Mn=4;
            break;
          case SteimFrame::Fhw:
            Mn=2;
            break;
          case SteimFrame::Ffw:
            Mn=1;
            break;
          default:
            DATRW_abort("ERROR (Steim1Frame::setn()): illegal code");
        }
      } // void Steim1Frame::setn()

      /*----------------------------------------------------------------------*/

      int Steim1Frame::diff() const
      {
        int retval=0;
        Steim1Word w;
        w.fw=this->word(); 
        if (this->valid())
        {
          switch(this->ctrl()) {
            case SteimFrame::Fspecial:
              retval=w.fw;
              break;
            case SteimFrame::Fbyte:
              retval=int(w.byte[this->idiff()]);
              break;
            case SteimFrame::Fhw:
              retval=w.hw[this->idiff()];
              break;
            case SteimFrame::Ffw:
              retval=w.fw;
              break;
            default:
              DATRW_abort("ERROR (Steim1Reader::diff()): illegal code");
          }
        }
        // std::cout << " ** diff is " << retval << std::endl;
        return(retval);
      } // int Steim1Frame::diff()

      /*======================================================================*/
      /* Steim 2 Reader code
       * -------------------
       */

      void Steim2Frame::setn()
      {
        Steim2Word w(Steim2Word::ESteim2Control(this->ctrl()), this->word()); 
        Mn=w.nval();
      } // void Steim2Frame::setn()

      /*----------------------------------------------------------------------*/

      int Steim2Frame::diff() const
      {
        Steim2Word w(Steim2Word::ESteim2Control(this->ctrl()), this->word()); 
        int retval=w.value(this->idiff());
        return(retval);
      } // int Steim2Frame::diff()

      /*======================================================================*/
      /* Steim 2 Word code
       * -----------------
       */

      int Steim2Word::dnib() const
      {
        return(int((Mword >> 30) & 0x03));
      } // int Steim2Word::dnib() const

      /*----------------------------------------------------------------------*/

      int Steim2Word::nval() const
      {
        int retval=0;
        switch (Mctrl) {
          case Steim2Word::Fspecial:
            retval=1;
            break;
          case Steim2Word::Fbyte:
            retval=4;
            break;
          case Steim2Word::Fdnib1:
            switch (this->dnib()) {
              case Steim2Word::Fdnib01:
                retval=1;
                break;
              case Steim2Word::Fdnib10:
                retval=2;
                break;
              case Steim2Word::Fdnib11:
                retval=3;
                break;
              default:
                DATRW_abort("ERROR (Steim2Word::value): "
                              "illegal decode nibble");
            }
            break;
          case Steim2Word::Fdnib2:
            switch (this->dnib()) {
              case Steim2Word::Fdnib00:
                retval=5;
                break;
              case Steim2Word::Fdnib01:
                retval=6;
                break;
              case Steim2Word::Fdnib10:
                retval=7;
                break;
              default:
                DATRW_abort("ERROR (Steim2Word::value): "
                              "illegal decode nibble");
            }
            break;
          default:
            DATRW_abort("ERROR (Steim2Word::value): illegal ctrl code");
        }
        return(retval);
      } // int Steim2Word::nval() const

      /*----------------------------------------------------------------------*/

      int Steim2Word::value(const int& i) const
      {
        int nval=this->nval();
        DATRW_assert((i<nval),
                       "ERROR (Steim2Word::value): "
                       "illegal value index");
        int retval=0;
        --nval;
        switch (Mctrl) {
          case Steim2Word::Fspecial:
            retval=Mword;
            break;
          case Steim2Word::Fbyte:
            // bytes must be read in a different order
            // since we do not swap this word
            // which is consistent with decode_steim2.c in rdseed
            retval=this->extract(8,i);
            break;
          case Steim2Word::Fdnib1:
            switch (this->dnib()) {
              case Steim2Word::Fdnib01:
                retval=this->extract(30,0);
                break;
              case Steim2Word::Fdnib10:
                retval=this->extract(15,nval-i);
                break;
              case Steim2Word::Fdnib11:
                retval=this->extract(10,nval-i);
                break;
              default:
                DATRW_abort("ERROR (Steim2Word::value): "
                              "illegal decode nibble");
            }
            break;
          case Steim2Word::Fdnib2:
            switch (this->dnib()) {
              case Steim2Word::Fdnib00:
                retval=this->extract(6,nval-i);
                break;
              case Steim2Word::Fdnib01:
                retval=this->extract(5,nval-i);
                break;
              case Steim2Word::Fdnib10:
                retval=this->extract(4,nval-i);
                break;
              default:
                DATRW_abort("ERROR (Steim2Word::value): "
                              "illegal decode nibble");
            }
            break;
          default:
            DATRW_abort("ERROR (Steim2Word::value): illegal ctrl code");
        }
        return(retval);
      } // int Steim2Word::value(const int& p) const

      /*----------------------------------------------------------------------*/

      int Steim2Word::extract(const int& b, const int& p)  const
      {
        DATRW_assert((b>0), "ERROR (Steim2Word::extract): "
                       "use at least 1 byte");
        DATRW_assert((p>=0), "ERROR (Steim2Word::extract): "
                       "illegal position");
        DATRW_assert((p<int(32/b)), "ERROR (Steim2Word::extract): "
                       "illegal position");
        int signmask=int(1 << (b-1));
        int valmask=signmask - 1;
        int nshift=b*p;
        // extract sign bit
        int sign=int((Mword >> nshift) & signmask);
        // extract value bit code
        int value=int((Mword >> nshift) & valmask);
        // construct 32-bit (int size) two's complement value
        int retval= sign ? (value | ~valmask) : value;
        return(retval);
      } // int Steim2Word::extract(const int& p)  const

    } // namespace SEED

  } // namespace mseed

} // namespace datrw

#undef SWAPIT

/* ----- END OF seedstructs.cc ----- */
