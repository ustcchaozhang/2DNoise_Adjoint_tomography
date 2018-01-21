/*! \file sacread.h
 * \brief decode sac files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * decode sac files (prototypes)
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
 *  - 21/12/2004   V1.0   Thomas Forbriger
 *  - 18/01/2008   V1.1   changed declaration of SAC structure
 *                        long is now int; this should provide 4 byte
 *                        integers on 32bit and 64bit systems as well
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SACREAD_H_VERSION

#define DATRW_SACREAD_H_VERSION \
  "DATRW_SACREAD_H   V1.1"

#include<istream>
#include<aff/series.h>

namespace datrw {

  /*! \brief all the stuff to read SAC binary data
   *
   * \defgroup group_sac Reading module for: SAC binary data
   */

  /*! \brief all the stuff to read SAC binary data
   *
   * \ingroup group_sac
   */
  namespace sac {

/*======================================================================*/

    /*! \brief header structure for SAC binary data
     *
     * \ingroup group_sac
     * \code
 *
 * this file comes along with the source of rdseed5.0 from IRIS DMC
 * rdseed is distributed unter the
 * GNU Lesser General Public License, Version 2.1, February 1999
 * 
 *===========================================================================* 
 * SEED reader     |               sac.h                   |     header file * 
 *===========================================================================* 
 *
        Name:           sac.h
        Purpose:        structure for header of a SAC (Seismic Analysis Code) data file
        Usage:          #include "sac.h"
        Input:          not applicable
        Output:         not applicable
        Warnings:       not applicable
        Errors:         not applicable
        Called by:      output_data
        Calls to:       none
        Algorithm:      not applicable
        Notes:          Key to comment flags describing each field:
                                Column 1:
                                        R = required by SAC
                                          = (blank) optional
                                Column 2:
                                        A = settable from a priori knowledge
                                        D = available in data
                                        F = available in or derivable from SEED fixed data header
                                        T = available in SEED header tables
                                          = (blank) not directly available from SEED data, header
                                            tables, or elsewhere
        Problems:       none known
        References:     O'Neill, D. (1987).  IRIS Interim Data Distribution Format
                (SAC ASCII), Version 1.0 (12 November 1987).  Incorporated
                Research Institutions for Seismology, 1616 North Fort Myer
                Drive, Suite 1440, Arlington, Virginia 22209.  11 pp.
                Tull, J. (1987).  SAC User's Manual, Version 10.2, October 7,
                        1987.  Lawrence Livermore National Laboratory, L-205,
                        Livermore, California 94550.  ??? pp.
        Language:       C, hopefully ANSI standard
        Author: Dennis O'Neill
        Revisions:07/15/88  Dennis O'Neill  Initial preliminary release 0.9
                11/21/88  Dennis O'Neill  Production release 1.0
                09/19/89  Dennis O'Neill  corrected length of char strings

       18/01/2008 modified long to int declaration

    \endcode
*/
    struct SACheader
    {
      public:
          float delta;          //!< RF time increment, sec    
          float depmin;         //!<    minimum amplitude      
          float depmax;         //!<    maximum amplitude      
          float scale;          //!<    amplitude scale factor 
          float odelta;         //!<    observed time inc      
          float b;              //!< RD initial value, ampl.   
          float e;              //!< RD final value, amplitude 
          float o;              //!<    event start, sec > 0   
          float a;              //!<    1st arrival time       
          float internal1;      //!<    internal use           
          float t0;             //!<    user-defined time pick 
          float t1;             //!<    user-defined time pick 
          float t2;             //!<    user-defined time pick 
          float t3;             //!<    user-defined time pick 
          float t4;             //!<    user-defined time pick 
          float t5;             //!<    user-defined time pick 
          float t6;             //!<    user-defined time pick 
          float t7;             //!<    user-defined time pick 
          float t8;             //!<    user-defined time pick 
          float t9;             //!<    user-defined time pick 
          float f;              //!<    event end, sec > 0     
          float resp0;          //!<    instrument respnse parm
          float resp1;          //!<    instrument respnse parm
          float resp2;          //!<    instrument respnse parm
          float resp3;          //!<    instrument respnse parm
          float resp4;          //!<    instrument respnse parm
          float resp5;          //!<    instrument respnse parm
          float resp6;          //!<    instrument respnse parm
          float resp7;          //!<    instrument respnse parm
          float resp8;          //!<    instrument respnse parm
          float resp9;          //!<    instrument respnse parm
          float stla;           //!<  T station latititude     
          float stlo;           //!<  T station longitude      
          float stel;           //!<  T station elevation, m   
          float stdp;           //!<  T station depth, m      
          float evla;           //!<    event latitude         
          float evlo;           //!<    event longitude        
          float evel;           //!<    event elevation        
          float evdp;           //!<    event depth            
          float unused1;        //!<    reserved for future use
          float user0;          //!<    available to user      
          float user1;          //!<    available to user      
          float user2;          //!<    available to user      
          float user3;          //!<    available to user      
          float user4;          //!<    available to user      
          float user5;          //!<    available to user      
          float user6;          //!<    available to user      
          float user7;          //!<    available to user      
          float user8;          //!<    available to user      
          float user9;          //!<    available to user      
          float dist;           //!<    stn-event distance, km 
          float az;             //!<    event-stn azimuth      
          float baz;            //!<    stn-event azimuth      
          float gcarc;          //!<    stn-event dist, degrees
          float internal2;      //!<    internal use           
          float internal3;      //!<    internal use           
          float depmen;         //!<    mean value, amplitude  
          float cmpaz;          //!<  T component azimuth     
          float cmpinc;         //!<  T component inclination 
          float unused2;        //!<    reserved for future use
          float unused3;        //!<    reserved for future use
          float unused4;        //!<    reserved for future use
          float unused5;        //!<    reserved for future use
          float unused6;        //!<    reserved for future use
          float unused7;        //!<    reserved for future use
          float unused8;        //!<    reserved for future use
          float unused9;        //!<    reserved for future use
          float unused10;       //!<    reserved for future use
          float unused11;       //!<    reserved for future use
          float unused12;       //!<    reserved for future use
          int   nzyear;         //!<  F zero time of file, yr  
          int   nzjday;         //!<  F zero time of file, day 
          int   nzhour;         //!<  F zero time of file, hr  
          int   nzmin;          //!<  F zero time of file, min 
          int   nzsec;          //!<  F zero time of file, sec 
          int   nzmsec;         //!<  F zero time of file, msec
          int   internal4;      //!<    internal use           
          int   internal5;      //!<    internal use           
          int   internal6;      //!<    internal use           
          int   npts;           //!< RF number of samples      
          int   internal7;      //!<    internal use           
          int   internal8;      //!<    internal use           
          int   unused13;       //!<    reserved for future use
          int   unused14;       //!<    reserved for future use
          int   unused15;       //!<    reserved for future use
          int   iftype;         //!< RA type of file          
          int   idep;           //!<    type of amplitude      
          int   iztype;         //!<    zero time equivalence  
          int   unused16;       //!<    reserved for future use
          int   iinst;          //!<    recording instrument   
          int   istreg;         //!<    stn geographic region  
          int   ievreg;         //!<    event geographic region
          int   ievtyp;         //!<    event type             
          int   iqual;          //!<    quality of data        
          int   isynth;         //!<    synthetic data flag    
          int   unused17;       //!<    reserved for future use
          int   unused18;       //!<    reserved for future use
          int   unused19;       //!<    reserved for future use
          int   unused20;       //!<    reserved for future use
          int   unused21;       //!<    reserved for future use
          int   unused22;       //!<    reserved for future use
          int   unused23;       //!<    reserved for future use
          int   unused24;       //!<    reserved for future use
          int   unused25;       //!<    reserved for future use
          int   unused26;       //!<    reserved for future use
          int   leven;          //!< RA data-evenly-spaced flag
          int   lpspol;         //!<    station polarity flag  
          int   lovrok;         //!<    overwrite permission   
          int   lcalda;         //!<    calc distance, azimuth 
          int   unused27;       //!<    reserved for future use
          char  kstnm[8];       //!<  F station name           
          char  kevnm[16];      //!<    event name             
          char  khole[8];       //!<    man-made event name    
          char  ko[8];          //!<    event origin time id   
          char  ka[8];          //!<    1st arrival time ident 
          char  kt0[8];         //!<    time pick 0 ident      
          char  kt1[8];         //!<    time pick 1 ident      
          char  kt2[8];         //!<    time pick 2 ident      
          char  kt3[8];         //!<    time pick 3 ident      
          char  kt4[8];         //!<    time pick 4 ident      
          char  kt5[8];         //!<    time pick 5 ident      
          char  kt6[8];         //!<    time pick 6 ident      
          char  kt7[8];         //!<    time pick 7 ident      
          char  kt8[8];         //!<    time pick 8 ident      
          char  kt9[8];         //!<    time pick 9 ident      
          char  kf[8];          //!<    end of event ident     
          char  kuser0[8];      //!<    available to user      
          char  kuser1[8];      //!<    available to user      
          char  kuser2[8];      //!<    available to user      
          char  kcmpnm[8];      //!<  F component name         
          char  knetwk[8];      //!<    network name           
          char  kdatrd[8];      //!<    date data read         
          char  kinst[8];       //!<    instrument name        
    }; // struct SACheader

/*======================================================================*/

    /*! \brief binary SAC sample type
     *
     * \ingroup group_sac
     */
    typedef float Tvalue;

    /*! ief SAC sample array type
     *
     * \ingroup group_sac
     */
    typedef aff::Series<Tvalue> Tseries;

/*======================================================================*/
// functions
      
    /*! \brief read SAC header from stream
     *
     * \ingroup group_sac
     */
    SACheader read_sac_header(std::istream& is);
      
    /*! \brief print information about file decoding
     *
     * \ingroup group_sac
     */
    void help(std::ostream& os);

    /*! \brief read samples from file
     *
     * \ingroup group_sac
     */
    Tseries read_sac_data(std::istream& is, const int& nsamples);

  } // namespace sac

} // namespace datrw

#endif // DATRW_SACREAD_H_VERSION (includeguard)

/* ----- END OF sacread.h ----- */
