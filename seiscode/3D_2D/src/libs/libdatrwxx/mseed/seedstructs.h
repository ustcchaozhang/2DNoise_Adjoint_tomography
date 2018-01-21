/*! \file seedstructs.h
 * \brief structures defined in the SEED (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/07/2004
 * 
 * structures defined in the SEED (prototypes)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 16/07/2004   V1.0   Thomas Forbriger
 *  - 28/04/2006   V1.1   long int appears to be of 
 *                        implementation dependent size
 *  - 09/05/2006   V1.2   introduced Steim 2 code
 *                        SteimFrame now does all that Steim1Reader did
 *  - 12/07/2016   V1.3   thof:
 *                        - fix [1001] Data Extension Blockette
 *                          field 4 (usec) explicitely is signed an can be
 *                          negative (see SEED V2.4 Manual, August 2012, page
 *                          124)
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SEEDSTRUCTS_H_VERSION

#define DATRW_SEEDSTRUCTS_H_VERSION \
  "DATRW_SEEDSTRUCTS_H   V1.3"

namespace datrw {

  namespace mseed {

    /*! SEED data structures
     *
     * The definitions for data structures are based an the
     *
     *   Standard for the Exchange of Earthquake Data \n
     *   Refernce Manual \n
     *   SEED Format Version 2.3 \n
     *   February, 1993 \n
     *
     * All references initially refer to this manual.
     *
     * With SEED V2.4 some of the definitions have changed.
     * Where changes are applied, they are documented in the comments in the
     * source code.
     *
     * The structure of a data record is given in Fig. 8 in chapter 2.
     *
     * A useful command to investigate binary data files is
     *
     *   od -A x -t x1z filename
     *
     * This dumps hex values at hex addresses and adds printable characters to
     * the end of each line.
     *
     * This namespace should contain raw SEED structure. Any structure that
     * provides additional facilities to read from a file should be placed in
     * namespace mseed (an thus in a different source file).
     */
    namespace SEED {

      template<class C1, class C2>
        void copy_from_pointer(C1* target, const C2* p)
        { *target = *(reinterpret_cast<const C1*>(p)); }

      /*----------------------------------------------------------------------*/
      
      /*! encoding formats
       *
       * used in Data Only SEED Blockette
       * see chapter 8, page 107
       */
      enum EEncodingFormat {
        ascii=0, //!< ascii text
        int16=1, //!< 16 bit integer
        int24=2, //!< 24 bit integer
        int32=3, //!< 32 bit integer
        ieeefp=4, //!< IEEE floating point
        ieeedp=5, //!< IEEE double precision floating point
        steim1=10, //!< Steim (1) compression
        steim2=11, //!< Steim (2) compression
        geoscope24=12, //!< GEOSCOPE multiplexed 24 bit integer
        geoscope163=13, //!< GEOSCOPE multiplexed 16 bit gain ranged 3 bit exp
        geoscope164=14, //!< GEOSCOPE multiplexed 16 bit gain ranged 4 bit exp
        us=15, //!< US national network compression
        cdsn=16, //!< CDSN 16 bit gain ranged
        grf=17, //!< Graefenberg 16 bit gain ranged
        ipg=18, //!< IPG Strasbourg 16 bit gain ranged
        steim3=19, //!< Steim (3) compression
        sro=30, //!< SRO format
        hglp=31, //!< HGLP format
        dwwssn=32, //!< DWWSSN gain ranged
        rstn=33 //!< RSTN 16 bit gain ranged
      }; // EEncodingFormat

      /*----------------------------------------------------------------------*/
      
      /*! byte swapping order
       *
       * used in Data Only SEED Blockette
       * see chapter 8, page 108
       */
      enum EByteOrder {
        vax8086=0, //!< VAX or 8086 byte order
        sparc68000=1, //!< SPARC or 68000 byte order
      }; // EByteOrder

      /*----------------------------------------------------------------------*/
      
      /*! a structure to hold any control header 
       *
       * see chapter 3, page 34 (old SEED manual) 
       * on "How to Assemble Control Headers"
       *
       * This structure is used in datrw::mseed::FixedDataRecordHeader
       * 
       * See recent SEED V2.4 Manual:
       * see chapter 8, page 108: "Fixed Section of Data Header (48 bytes)"
       */
      struct ControlHeader {
        ControlHeader() { }
        ControlHeader(const char* p) { copy_from_pointer(this, p); }
        char seqno[6];
        char type;
        char cont;
      }; // ControlHeader

      /*----------------------------------------------------------------------*/
      
      /*! a structure to hold a blockette header 
       *
       * see chapter 3, page 34 on "How to Assemble Control Headers"
       * 
       * This differs from the blockette header of data record blockettes.
       * See DataRecordBlocketteHeader below.
       */
      struct BlocketteHeader {
        BlocketteHeader() { }
        BlocketteHeader(const char* p) { copy_from_pointer(this, p); }
        char type[3];
        char len[4];
      }; // BlocketteHeader

      /*----------------------------------------------------------------------*/
      
      /*! a structure to hold a telemtry volume identifier blockette 
       *
       * only used within COMSERV data
       *
       * [8] Telemetry Volume Identifier Blockette
       */
       struct TelemetryVolumeIdentifierBlockette {
        ControlHeader controlheader;
        BlocketteHeader blocketteheader;
        char ver[4];
        char len[2];
        char stat[5];
        char loc[2];
        char chan[3];
        char rest[1];
      }; // TelemetryVolumeIdentifierBlockette

      /*----------------------------------------------------------------------*/
       
      /* a structure to hold an extracted 
       * telemtry volume identifier blockette 
       */
       /*
      struct TelemetryVolumeIdentifier {
        char ver[5];
        char len[3];
        char stat[6];
        char loc[3];
        char chan[4];
        char begv[23];
        char endv[23];
        char sied[23];
        char cied[23];
        char net[3];
      };
      */

      /*----------------------------------------------------------------------*/
       
      /*! a strcuture to hold a starting time etc. 
       *
       * see chapter 3, page 35.
       */
      struct BTIME {
        BTIME() { }
        BTIME(const char* p, const bool& swap=false)
        { 
          copy_from_pointer(this, p); 
          if (swap) { this->swap(); }
        }
        void swap();
        unsigned short int year;
        unsigned short int doy;
        unsigned char hour;
        unsigned char min;
        unsigned char sec;
        unsigned char zero;
        unsigned short int tmilsec;
      }; // BTIME

      /*----------------------------------------------------------------------*/

      /*! a structure to hold activity flags
       *
       * used in Fixed Data Record Header
       * see chapter 8, page 93
       */
      struct ActivityFlags {
        ActivityFlags() { }
        ActivityFlags(const unsigned char& p) { copy_from_pointer(this, &p); }
        bool calpres: 1; //!< calibration signals present
        bool tcorrapp: 1; //!< time correction applied
        bool begevent: 1; //!< beginning of event
        bool endevent: 1; //!< end of event
        bool posleap: 1; //!< a positive leap second happend
        bool negleap: 1; //!< a negative leap second happend
        bool event: 1; //!< event in progress
        bool unused1: 1;
      }; // ActivityFlags

      /*----------------------------------------------------------------------*/

      /*! a structure to hold I/O flags
       *
       * used in Fixed Data Record Header
       * see chapter 8, page 93
       */
      struct IOFlags {
        IOFlags() { }
        IOFlags(const unsigned char& p) { copy_from_pointer(this, &p); }
        bool parityerr: 1; //!< station volume parity error possibly present
        bool longrec: 1; //!< long record read
        bool shortrec: 1; //!< short record read
        bool start: 1; //!< start of time series
        bool end: 1; //!< end of time series
        bool locked: 1; //!< clock locked
        bool unused1: 1;
        bool unused2: 1;
      }; // IOFlags

      /*----------------------------------------------------------------------*/

      /*! a structure to hold quality flags
       *
       * used in Fixed Data Record Header
       * see chapter 8, page 93
       */
      struct QualityFlags {
        QualityFlags() { }
        QualityFlags(const unsigned char& p) { copy_from_pointer(this, &p); }
        bool ampsat: 1; //!< amplifier saturation detected
        bool clip: 1; //!< digitizer clipping detected
        bool spike: 1; //!< spikes detected
        bool glitch: 1; //!< glitches detected
        bool miss: 1; //!< missing/padded data present
        bool telsynch: 1; //!< telemetry synchronization error
        bool charging: 1; //!< a digital filter may be charging
        bool time: 1; //!< time tag is questionable
      }; // QualityFlags

      /*----------------------------------------------------------------------*/

      /*! a strcuture to hold a fixed data record header 
       *
       * Old SEED manual: see chapter 8, page 92.
       *
       * Recent SEED V2.4 Manual:
       * see chapter 8, page 108: "Fixed Section of Data Header (48 bytes)"
       */
      struct FixedDataRecordHeader {
        FixedDataRecordHeader() { }
        FixedDataRecordHeader(const char* p, const bool& doswap=false)
        { 
          copy_from_pointer(this, p); 
          if (doswap) { this->swap(); }
        }
        void swap();
        ControlHeader controlheader; //!< control header with sequence number
        char stat[5]; //!< station identifier code
        char loc[2];  //!< location identifier
        char chan[3]; //!< channel identifier
        char net[2];  //!< network code
        BTIME stime;  //!< record start time
        unsigned short int nsamp; //!< number of samples
        short int srate;          //!< sample rate factor
        short int srmult;         //!< sample rate multiplier
        unsigned char aflags;     //!< activity flags
        unsigned char ioflags;    //!< i/o and clock flags
        unsigned char qflags;     //!< data quality flags
        unsigned char numblock;   //!< number of blockettes that follow
        //long int tcorr;           //!< time correction
        int tcorr;                //!< time correction
        unsigned short int dbeg;  //!< beginning of data
        unsigned short int fblock; //!< first blockette
      }; // FixedDataRecordHeader

      /*----------------------------------------------------------------------*/

      /*! s structure to hold a data record blockette header
       */
      struct DataRecordBlocketteHeader {
        DataRecordBlocketteHeader() { }
        DataRecordBlocketteHeader(const char* p, const bool& swap=false)
        { 
          copy_from_pointer(this, p); 
          if (swap) { this->swap(); }
        }
        void swap();
        unsigned short int type; //!< Blockette type 
        unsigned short int next; //!< Next blockette's byte number
      }; // DataRecordBlocketteHeader

      /*----------------------------------------------------------------------*/

      /*! a structure to hold a data only blockette 
       *
       * [1000] Data Only SEED Blockette
       */
      struct DataOnlySEEDBlockette {
        DataOnlySEEDBlockette() { }
        DataOnlySEEDBlockette(const char* p, const bool& swap=false)
        { 
          copy_from_pointer(this, p); 
          if (swap) { this->swap(); }
        }
        void swap();
        DataRecordBlocketteHeader blocketteheader;
        unsigned char format;    //!< Encoding Format
        unsigned char bytesex;   //!< Word order
        unsigned char reclen;    //!< Data Record Length
        unsigned char reserved;  //!< Reserved
        unsigned int reclenbytes() const { return(2<<(reclen-1)); }
        unsigned int ireclen() const { return(this->reclen); }
        unsigned int ibytesex() const { return(this->bytesex); }
        unsigned int iformat() const { return(this->format); }
      }; // DataOnlySEEDBlockette

      /*----------------------------------------------------------------------*/

      /*! a structure to hold a data extension blockette 
       *
       * [1001] Data Extension Blockette
       */
      struct DataExtensionBlockette {
        DataExtensionBlockette() { }
        DataExtensionBlockette(const char* p, const bool& swap=false)
        { 
          copy_from_pointer(this, p); 
          if (swap) { this->swap(); }
        }
        void swap();
        DataRecordBlocketteHeader blocketteheader;
        unsigned char tquality;  //!< Timing quality
        char usec;      //!< microseconds
        unsigned char reserved;  //!< reserved
        unsigned char fcount;    //!< frame count
        int iusec() const { return(this->usec); }
        unsigned int itquality() const { return(this->tquality); }
        unsigned int ifcount() const { return(this->fcount); }
      }; // DataExtensionBlockette

      /*----------------------------------------------------------------------*/

      /*! a union to hold a fout-byte word in a Steim 1 frame
       */
      union Steim1Word {
        char byte[4];         //!< 4 1-byte differences
        short int hw[2];      //!< 2 2-byte differences (halfword)
        int fw;               //!< 1 4-byte difference (fullword)
      }; // union Steim1Word

      /*----------------------------------------------------------------------*/

      /*! a class to hold a four-byte word in a Steim 2 frame
       *
       * A Steim 2 frame knows eight different types of data encoding:
       *   - c=00 special (like Steim 1)
       *   - c=01 four 1 Byte (8 Bit) differences (like Steim 1)  
       *   - c=10 look for dnib (high order two bits) in data word
       *     - dnib=01 one 30-bit difference
       *     - dnib=10 two 15-bit differences
       *     - dnib=11 three 10-bit differences
       *   - c=11 look for dnib (high order two bits) in data word
       *     - dnib=00 five 6-bit differences
       *     - dnib=01 six 5-bit differences
       *     - dnib=10 seven 4-bit differences
       */
      class Steim2Word {
        public:
          enum ESteim2Control {
            Fspecial=0,   //!< contains non-data information, like headers
            Fbyte=1,      //!< four 8-bit differences (byte)
            Fdnib1=2,     //!< check dnib
            Fdnib2=3      //!< check dnib
          }; // enum ESteim1Control
          enum ESteim2dnib {
            Fdnib00=0,
            Fdnib01=1,
            Fdnib10=2,
            Fdnib11=3
          }; // enum ESteim2dnib
          //! store control code and data word
          Steim2Word(const ESteim2Control& c, const int& word): 
            Mctrl(c), Mword(word) { };
          //! return dnib
          int dnib() const;
          /*! return value
           *
           * \arg \c i index in word
           */
          int value(const int& i) const;
          //! return number of values in this word
          int nval() const;
        private:
          /*! return masked value
           *
           * \arg \c b number of bits
           * \arg \c p position in word
           */
          int extract(const int& b, const int& p) const;
          ESteim2Control Mctrl;
          int Mword;
      }; // class Steim2Word

      /*----------------------------------------------------------------------*/

      /*! a base class to handle a Steim frame
       *
       * We explicitely distinguish between Steim 1 and Steim 2 frames,
       * since data swapping differs for them.
       */
      class SteimFrame {
        public:
          // --- constants section ---
          //! numer of words to process
          static const int nwords=15;
          //! size of frame in bytes
          static const int blocksize=16*sizeof(int);
          // --- type definition section ---
          //! possible control codes
          enum ESteimControl {
            Fspecial=0,   //!< contains non-data information, like headers
            Fbyte=1,      //!< four 8-bit differences (byte)
            Fhw=2,        //!< two 16-bit differences (halfword)
            Ffw=3,        //!< one 32-bit difference (fullword)
            Fdnib1=2,     //!< check dnib
            Fdnib2=3      //!< check dnib
          }; // enum ESteimControl
          //! struct to hold frame data
          struct FrameData {
            //! read
            FrameData(const char* p)
            { datrw::mseed::SEED::copy_from_pointer(this, p); }
            //! control flags
            unsigned int control;     
            //! data buffer
            int word[nwords];
          }; // struct FrameData
          // --- member function section ---
          //! virtual base class need virtual destructor
          virtual ~SteimFrame() { }
          //! return control word
          unsigned int control() const { return(Mdata.control); }
          //! return control code for word \c i
          ESteimControl ctrl(const int& i) const;    
          //! return data word \c i
          int word(const int& i) const { return(Mdata.word[i]); }
          //! return current data word 
          int word() const { return(this->word(this->iword())); }
          //! read data from pointer
          void read(const char* p, const bool& swap=false)
          { 
            Mdata=FrameData(p);
            if (swap) { this->swap(); }
          }
          //! return current difference value
          virtual int diff() const =0;
          //! we are still inside the frame
          bool valid() const { return(Mvalid); }
          //! return control code for current word
          SteimFrame::ESteimControl ctrl() const
          { return(this->ctrl(this->iword())); }
          //! step to next difference value
          void next();
          //! step to first difference value
          void reset();
          //! return current word index
          int iword() const { return(Miword); }
          //! return current difference index
          int idiff() const { return(Midiff); }
        protected:
          //! swapping must be performed differently for Steim1 and Steim2
          virtual void swap() =0;
          //! set Mn for current word (compression type specific)
          virtual void setn() =0;
          //! read data to buffer
          SteimFrame(const char* p):
          Mdata(p), Mn(0), Miword(0), Midiff(0), Mvalid(true) { }
          // --- data section ---
          //! frame data (swapped)
          FrameData Mdata;
          //! number of differences in this word
          int Mn;
        private:
          //! current word in frame
          int Miword;
          //! current difference value in word
          int Midiff;
          //! we are still inside the current frame
          bool Mvalid;
      }; // class SteimFrame
      
      /*----------------------------------------------------------------------*/

      /*! a structure holding a Steim 1 frame
       */
      class Steim1Frame: public SteimFrame {
        public:
          typedef SteimFrame Tbase;
          virtual ~Steim1Frame() { }
          Steim1Frame(const char* p, const bool& swap=false):
            Tbase(p) 
          { 
            if (swap) { this->swap(); } 
            this->reset();
          }
          virtual void swap();
          virtual void setn();
          virtual int diff() const;
      }; // class Steim1Frame
      
      /*----------------------------------------------------------------------*/

      /*! a structure holding a Steim 2 frame
       */
      class Steim2Frame: public SteimFrame {
        public:
          typedef SteimFrame Tbase;
          virtual ~Steim2Frame() { }
          Steim2Frame(const char* p, const bool& swap=false):
            Tbase(p)
          { 
            if (swap) { this->swap(); } 
            this->reset();
          }
          virtual void swap();
          virtual void setn();
          virtual int diff() const;
      }; // class Steim2Frame

    } // namespace SEED

  } // namespace mseed

} // namespace datrw

#endif // DATRW_SEEDSTRUCTS_H_VERSION (includeguard)

/* ----- END OF seedstructs.h ----- */
