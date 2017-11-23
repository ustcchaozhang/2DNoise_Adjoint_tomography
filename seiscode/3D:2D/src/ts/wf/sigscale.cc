/*! \file sigscale.cc
 * \brief apply scaling relations to input signals
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2011
 * 
 * apply scaling relations to input signals
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 11/04/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define SIGSCALE_VERSION \
  "SIGSCALE   V1.0   apply scaling relations to input signals"

#include <iostream>
#include <cmath>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <datrwxx/channeltranslation.h>

#include <fstream>
#include <string>
#include <tfxx/xcmdline.h>
#include <tfxx/stringfunc.h>
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>
#include <tfxx/misc.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <sffxx.h>
#include <aff/iterator.h>

using datrw::tsoft::Econversion;
using datrw::tsoft::ChannelDescription;
using datrw::tsoft::SFFchannelid;
using datrw::tsoft::channel;

typedef aff::Series<double> Tseries;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, debrep;
  bool overwrite;
  std::string inputformat, outputformat;
}; // struct Options

/*======================================================================*/

/*
 * the following fragment is taken from
 * DDAS3_man_technical_ch5_UIPCsoftware_2010-07-16_BFO-Excerpt.docx
 * as sent by Eric Brinton Fri, 16 Jul 2010 15:52:55 -0700
 */

namespace DDAS3 {

  const double    VL[4] = { 1.32412, 1.11732, 0.923142, 0.079767 }; 
  const double    VU[4] = { 1.69812, 1.42013, 1.139350, 0.999614 }; 
  const int       ND[4] = { 10, 11, 12, 11};
  const double    A [4][12] = 
  {{  7.556358,   -5.917261,  0.237238, -0.334636, -0.058642, -0.019929,
     -0.020715, -0.014814, -0.008789, -0.008554,  0.000000, 0.000000},
  {17.304227,   -7.894688,  0.453442,  0.002243,  0.158036, -0.193093,
    0.155717, -0.085185,  0.078550, -0.018312,  0.039255, 0.000000},
  {71.818025,  -53.799888,  1.669931,  2.314228,  1.566635,  0.723026,
    -0.149503,  0.046876, -0.388555,  0.056889,  0.116823, 0.058580},
  {287.756797, -194.144823, -3.837903, -1.318325, -0.109120, -0.393265,
    0.146911, -0.111192,  0.028877, -0.029286,  0.015619, 0.000000}}; 

  // Chebychev fit coeff for SiDiode 
#define TRANGE_12K		0
#define TRANGE_24K		1
#define TRANGE_100K		2
#define TRANGE_475K		3

  void report_sidiode_table(int range)
  {
    std::cout << " to be implemented " << std::endl;
    std::cout << " range  " << range << std::endl;
  } // void report_sidiode_table()

  ///////////////////////////////////////////////////////////////////////////////
  // convert_pt100_ohms_to_celsius
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_pt100_ohms_to_celsius(double ohms)
  {
    return( (double)(1.0/0.00385) * ((ohms/(double)100.0) - (double)1.0) ); 
  } // double  convert_pt100_ohms_to_celsius(double ohms)

  ///////////////////////////////////////////////////////////////////////////////
  // convert_pt100_ohms_to_celsius
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_pt100_ohms_to_Kelvin(double ohms)
  {
    return( 273.15 + convert_pt100_ohms_to_celsius(ohms)) ; 
  } // double  convert_pt100_ohms_to_Kelvin(double ohms)

  ///////////////////////////////////////////////////////////////////////////////
  // convert_volts_to_psi
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_volts_to_psi(double volts)
  {
    return( (volts - 6.0) * 0.250) ; 
  } // double  convert_volts_to_psi(double volts)

  ///////////////////////////////////////////////////////////////////////////////
  // convert_td_ohms_to_celsius
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_td_ohms_to_celsius(double ohms)
  {
    double a, b, c, Ro, Rt, T;

    Ro = 1854;
    Rt = ohms;

    c = Ro - Rt;
    b = 3.84 * 0.001 * Ro;
    a = 4.94 * 0.000001 * Ro;

    T = ((-b) + sqrt((b*b) - (4.0*a*c))) / (2.0*a);

    return( T ); 
  } // double  convert_td_ohms_to_celsius(double ohms)

  ///////////////////////////////////////////////////////////////////////////////
  // convert_volts_to_sidiode
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_sidiode_volts_to_kelvin(double volts)
  {
    double  x, t, v, vl, vu;        
    int     i, nd, range;

    if ( volts < 0.24963 )  
      // clip value to 410K if out of range e.g.sensor unplugged
      return 410.0 ;

    // Coefficients table ends at 1.4K
    if ( volts > 1.698120 )  
      // clip value to 1.4K if out of range //changed 09/08
      return 1.40 ;

    if      ( volts > VU[TRANGE_12K] ) range = 0 ;
    else if ( volts>VU[TRANGE_24K ] && volts<=VU[TRANGE_12K ] ) range = 0;
    else if ( volts>VU[TRANGE_100K] && volts<=VU[TRANGE_24K ] ) range = 1;
    else if ( volts>VU[TRANGE_475K] && volts<=VU[TRANGE_100K] ) range = 2;
    else    /*volts<=VU[TRANGE_475K]*/                          range = 3;

    v  = volts;
    vl = VL[range];
    vu = VU[range];
    nd = ND[range];         
    x  = ((v-vl)-(vu-v))/(vu-vl);

    if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */

    t = 0.0;
    for( i=0; i<nd; i++ )
      t = t + A[range][i]*cos(i*acos(x));

    return t;
  } // double  convert_sidiode_volts_to_kelvin(double volts)

  ///////////////////////////////////////////////////////////////////////////////
  // convert_volts_to_sidiode_with_correction
  // NEED to add correction factor for non magnetic sensor.
  ///////////////////////////////////////////////////////////////////////////////
  double  convert_sidiode_volts_to_kelvin_with_correction(double volts)
  {
    double  x, t, v, vl, vu;        
    int     i, nd, range;


    if ( volts < 0.24963 )  
      // clip value to 410K if out of range e.g.sensor unplugged
      return 410.0 ;

    // Coefficients table ends at 1.4K
    if ( volts > 1.698120 )  
      // clip value to 1.4K if out of range //changed 09/08
      return 1.40 ;

    // std::cout << "\n\nDEBUG in SIDIODE4: " << volts << std::endl;

    if      ( volts > VU[TRANGE_12K] ) range = 0 ;
    else if ( volts>VU[TRANGE_24K ] && volts<=VU[TRANGE_12K ] ) range = 0;
    else if ( volts>VU[TRANGE_100K] && volts<=VU[TRANGE_24K ] ) range = 1;
    else if ( volts>VU[TRANGE_475K] && volts<=VU[TRANGE_100K] ) range = 2;
    else    /*volts<=VU[TRANGE_475K]*/                          range = 3;

    v  = volts;
    vl = VL[range];
    vu = VU[range];
    nd = ND[range];         
    x  = ((v-vl)-(vu-v))/(vu-vl);

    if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */

    t = 0.0;
    for( i=0; i<nd; i++ )
      t = t + A[range][i]*cos(i*acos(x));

    return t;
  } // double  convert_sidiode_volts_to_kelvin_with_correction(double volts)

  /****************************************************************************/
  // convert_GEPLHe19_volts_to_percent
  // 19" (482.6mm) Active Length * 45 ohm / mm = 219.583 ohms total at 0%
  // Excitation current = 73.53 mA, V=16.14581
  // Electronics gain = 0.5, Vout at 0% = 8.0729
  // coeficient = 100 / 8.0729 = 12.3871
  /****************************************************************************/
  double  convert_GEPLHe19_volts_to_percent(double volts)
  {
    return(100.0 + (volts * -12.3871)) ; 
  } // double  convert_GEPLHe19_volts_to_percent(double volts)

  /****************************************************************************/
  /****************************************************************************/
  // convert_GEPLHe23_volts_to_percent
  // 23" (584.2mm) Active Length * 45 ohm / mm = 265.811 ohms total at 0%
  // Excitation current = 73.53 mA, V=19.5449
  // Electronics gain = 0.5, Vout at 0% = 9.77246
  // coeficient = 100 / 9.77246 = 10.2328
  /****************************************************************************/
  double  convert_GEPLHe23_volts_to_percent(double volts)
  {
    return(100.0 + (volts * -10.2328)) ; 
  } // double  convert_GEPLHe23_volts_to_percent(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_PSI_0-500
  // This routine is called when the tag PSI-500 is used in the Chan Config file
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 500 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 PSI                                                 
  // 20mA =  0.9980V = 500PSI                                                
  // PSI/V = 500PSI/0.7984 = 626.2525                                        
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_PSI_0_500(double volts)
  {
    double  dPSI;        

    if ( volts < 0.1 )  
      // clip value to -10PSI if out of range e.g.sensor faulty
      return -10.0 ;

    else if ( volts > 1.8 )  
      // clip value to 1000PSI if out of range e.g.sensor unplugged
      return 1000 ;

    dPSI = (volts - 0.1996) * 626.2525 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return dPSI;    
  } // double  convert_v_to_Pres_PSI_0_500(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_PSI_0-3000
  // This routine is called when the tag PSI-3000 is used in the Chan Config file
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 3000 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 PSI                                                 
  // 20mA =  0.9980V = 3000PSI                                                
  // PSI/V = 3000PSI/0.7984 = 3757.5150                                        
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_PSI_0_3000(double volts)
  {
    double  dPSI;        

    if ( volts < 0.1 )  
      // clip value to -400PSI if out of range e.g.sensor faulty
      return -400.0 ;

    else if ( volts > 1.8 )  
      // clip value to 6000PSI if out of range e.g.sensor unplugged
      return 6000 ;

    dPSI = (volts - 0.1996) * 3757.5150 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return dPSI;    
  } // double  convert_v_to_Pres_PSI_0_3000(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_KPa_0-3447
  // This routine is called when the tag KPa-3447 is used in the Chan Config file
  // THIS IS FOR THE 500PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 3000 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 KPa                                                 
  // 20mA =  0.9980V = 3447.37864KPa                                                
  // KPa/V = 3447.37864KPa/0.7984 = 4317.8590                           
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_KPa_0_3447(double volts)
  {
    double  KPa;        

    if ( volts < 0.1 )  
      // clip value to -450KPa if out of range e.g.sensor faulty
      return -450.0 ;

    else if ( volts > 1.8 )  
      // clip value to 7000KPa if out of range e.g.sensor unplugged
      return 7000 ;

    KPa = (volts - 0.1996) *  4317.8590 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return KPa;    
  } // double  convert_v_to_Pres_KPa_0_3447(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_KPa_0-20684
  // This routine is called when the tag KPa-20684 is used in the Chan Config file
  // THIS IS FOR THE 3000PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 3000 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 KPa                                                 
  // 20mA =  0.9980V = 20684.2718KPa                                                
  // KPa/V = 20684.2718KPa/0.7984 = 25907.154108  
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_KPa_0_20684(double volts)
  {
    double  KPa;        

    if ( volts < 0.1 )  
      // clip value to -2700KPa if out of range e.g.sensor faulty
      return -2700.0 ;

    else if ( volts > 1.8 )  
      // clip value to 42000KPa if out of range e.g.sensor unplugged
      return 42000 ;

    KPa = (volts - 0.1996) *  25907.1541 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return KPa;    
  } // double  convert_v_to_Pres_KPa_0_20684(double volts)

  // new 10/14/09 //kh
  /****************************************************************************/
  // convert_volts_to_Pres_PSI_0_2500
  // This routine is called when the tag PSI-2500 is used in the Chan Config file
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 2500 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 PSI                                                 
  // 20mA =  0.9980V = 2500PSI
  // voltage span = Vmax - Vzero = .9980V - .1996V = .7984V
  // PSI/V = 2500PSI/0.7984 = 3131.263 PSI/volt                                      
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_PSI_0_2500(double volts)
  {
    double  dPSI;        

    if ( volts < 0.1 )  
      // clip value to -400PSI if out of range e.g.sensor faulty
      return -400.0 ;

    else if ( volts > 1.8 )  
      // clip value to 6000PSI if out of range e.g.sensor unplugged
      return 6000 ;

    dPSI = (volts - 0.1996) * 3131.263 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return dPSI;    
  } // double  convert_v_to_Pres_PSI_0_2500(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_KPa_0_17236
  // This routine is called when the tag KPa-17236 is used in the Chan Config file
  // THIS IS FOR THE 2500PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 2500 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 KPa                                                 
  // 20mA =  0.9980V = 17236.8932KPa     
  // voltage span = Vmax - Vzero = .9980V - .1996V = .7984V
  // KPa/V = 17236.8932KPa/0.7984 =  21589.29509
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_KPa_0_17236(double volts)
  {
    double  KPa;        

    if ( volts < 0.1 )  
      // clip value to -2700KPa if out of range e.g.sensor faulty
      return -2700.0 ;

    else if ( volts > 1.8 )  
      // clip value to 42000KPa if out of range e.g.sensor unplugged
      return 42000 ;

    KPa = (volts - 0.1996) *  21589.295 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return KPa;    
  } // double  convert_v_to_Pres_KPa_0_17236(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_MPa_0_17236
  // This routine is called when the tag MPa-17236 is used in the Chan Config file
  // THIS IS FOR THE 2500PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 2500 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 MPa                                                 
  // 20mA =  0.9980V = 17.2368932MPa     
  // voltage span = Vmax - Vzero = .9980V - .1996V = .7984V
  // MPa/V = 17.2368932MPa/0.7984V =  21.58929509 MPa/V
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_MPa_0_17236(double volts)
  {
    double  MPa;        

    if ( volts < 0.1 )  
      // clip value to -2.700MPa if out of range e.g.sensor faulty
      return -2.7000 ;

    else if ( volts > 1.8 )  
      // clip value to 42.000MPa if out of range e.g.sensor unplugged
      return 42.000 ;

    MPa = (volts - 0.1996) *  21.589295 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return MPa;    
  } // double  convert_v_to_Pres_MPa_0_17236(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_MPa_0_3447
  // This routine is called when the tag MPa-3447 is used in the Chan Config file
  // THIS IS FOR THE 500PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 500 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 MPa                                                 
  // 20mA =  0.9980V = 3.44738MPa      
  // voltage span = Vmax - Vzero = .9980V - .1996V = .7984V
  // MPa/V = 3.44738MPa/0.7984 = 4.317859                           
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_MPa_0_3447(double volts)
  {
    double  MPa;        

    if ( volts < 0.1 )  
      // clip value to -4.50MPa if out of range e.g.sensor faulty
      return -4.500 ;

    else if ( volts > 1.8 )  
      // clip value to 7.000MPa if out of range e.g.sensor unplugged
      return 7.000 ;

    MPa = (volts - 0.1996) *  4.317859 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return MPa;    
  } // double  convert_v_to_Pres_MPa_0_3447(double volts)

  /****************************************************************************/
  // convert_volts_to_Pres_MPa_0_20684
  // This routine is called when the tag MPa-20684 is used in the Chan Config file
  // THIS IS FOR THE 3000PSI SENSOR
  ///////////////////////////////////////////////////////////////////////////////
  // Converts presssure sensor with 4 to 20 mA output and 3000 PSI range    
  // Voltage sense resistor in GEP3 remote PCB is 49.9 ohms                  
  // NOTE: Can NOT use 499.9 ohm voltage sense resistor without exceeding    
  // our sense voltage capabilities                                          
  // Excitation = 12VDC.                                                    
  // 4mA =   0.1996V = 0 MPa                                                 
  // 20mA =  0.9980V = 20.6842718MPa                                                
  // MPa/V = 20684.2718MPa/0.7984V = 25.907154108 MPa/V
  // Volt_Zero_Offset = 0.1996V                                              
  /****************************************************************************/
  double  convert_v_to_Pres_MPa_0_20684(double volts)
  {
    double  MPa;        

    if ( volts < 0.1 )  
      // clip value to -2.7MPa if out of range e.g.sensor faulty
      return -2.7000 ;

    else if ( volts > 1.8 )  
      // clip value to 42.000MPa if out of range e.g.sensor unplugged
      return 42.000 ;

    MPa = (volts - 0.1996) *  25.90715 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return MPa;    
  } // double  convert_v_to_Pres_MPa_0_20684(double volts)

  /****************************************************************************/
  // convert_volts_to_FAN16
  // This routine is called with the tag FAN16
  ///////////////////////////////////////////////////////////////////////////////
  // Converts control voltage supplied to an amplifier powering external tree fans 
  // to %fan speed.  Assumes min fan control voltage acceptable is 1.6V=16% (hence FAN16)                  
  // NOTE: The fans should NOT be turned off completely, in their "off" state they are kept
  // idiling to prevent stall and burnout.
  // To accomplish this, the Newport controller (I16D)is programmed to supply a minimum
  // 16% * 10 V = 1.6 volts at the output of out-1
  // 1.6V =   0%
  // 9.9V =  100%
  // voltage span = Vmax - Vzero = 9.9V - 1.6V = 8.3V
  // %/V = 100%/8.3V = 12.04819 %/volt                                        
  // Volt_Zero_Offset = 1.60V 
  /****************************************************************************/
  double  convert_v_to_FAN16(double volts)
  {
    double  dFanPercent;        

    if ( volts < 1.6 )  // clip value to 0%
      return 0 ;

    else if ( volts > 9.9 )  // clip value to 100%
      return 100 ;

    dFanPercent = (volts - 1.6) * 12.04819 ;

    //Should there be an error condition??    
    //if ( x <= -1.0  || x >= 1.0 ) x = 0 ;  /* error condition */
    return dFanPercent;    
  } // double  convert_v_to_FAN16(double volts)

  /*----------------------------------------------------------------------*/

  ////////////////////////////////////////////////////////////////////////////
  // convert_to_display_units
  ////////////////////////////////////////////////////////////////////////////
  double  convert_to_display_units(const Econversion& ConvCode,
                                   const double& chanval)
  {
    double retval, V, Vcor;

    switch (ConvCode)
    {
      case datrw::tsoft::SIDIODE:
      case datrw::tsoft::SIDIODE1:
        retval = convert_sidiode_volts_to_kelvin(chanval); 
        break;
      case datrw::tsoft::PT100:
        retval = convert_pt100_ohms_to_celsius(chanval); 
        break;
      case datrw::tsoft::PT100V:
        // changed from 1000 to 100 4/23/08 for rev3 pcb
        retval = convert_pt100_ohms_to_celsius(100.0 * chanval); 
        break;
      case datrw::tsoft::PT100_K: // Added 8/02 per eb
        retval = convert_pt100_ohms_to_Kelvin(chanval); 
        break;
      case datrw::tsoft::PT100V_K: // Added 8/02 per eb 
        // changed from 1000 to 100 4/23/08 for rev3 pcb
        retval = convert_pt100_ohms_to_Kelvin(100.0 * chanval); 
        break;
      case datrw::tsoft::PSI_1_1: // Added 11/05 per eb 
        retval = convert_volts_to_psi( chanval); 
        break;
      case datrw::tsoft::GEPLHe19: // Added 11/05 per eb 
        retval = convert_GEPLHe19_volts_to_percent( chanval); 
        break;
      case datrw::tsoft::GEPLHe23: // Added 8/07 per eb 
        retval = convert_GEPLHe23_volts_to_percent( chanval); 
        break;
      case datrw::tsoft::TD:
        retval = convert_td_ohms_to_celsius(chanval); 
        break;
      case datrw::tsoft::TDV:
        retval = convert_td_ohms_to_celsius(1000.0 * chanval); 
        break;
      case datrw::tsoft::MASFLO1:
        //   Mass Airflow Sensor Manufactured by Microswitch,  Model #AWM3100V
        //   Equation from  Bruno Meurers 3rd degree polynomial fit using data
        //   from manufacturers data sheet flow =
        //   -39.35+(48.390*voltage)-(11.737*voltage^2)+(2.3064*voltage^3)
        V = chanval;
        retval = ( -39.35 + (48.390 * V) - (11.737 * V * V) + (2.3064 * V * V * V) ) ;
        break;
        //else if( coeff == MASFLO1:
        //{ 
        //    retval = chanval; 
        //    break;
      case datrw::tsoft::H2OFLO1:
        retval = chanval; 
        break;
      case datrw::tsoft::SIDIODE2:
        //non-magnetic sensor raw voltage no gain from scanner
        Vcor = chanval ;
        if ( Vcor > 1.25 ) { Vcor = chanval - (0.2363 * ( chanval - 1.25 )); }
        // removed /4.98 8/02 per eb
        retval = convert_sidiode_volts_to_kelvin_with_correction(Vcor); 
        break;
      case datrw::tsoft::SIDIODE3:
        // voltage with 4.98x amp from gep aux pcb
        Vcor = chanval/4.98 ;
        retval = convert_sidiode_volts_to_kelvin(Vcor); 
        break;
      case datrw::tsoft::SIDIODE4:
        //non-magnetic sensor with 4.98x amp from gep aux pcb
        Vcor = chanval/4.98 ;
        if ( Vcor > 1.25) { Vcor = Vcor - (0.2363 * ( Vcor - 1.25 )); }
        retval = convert_sidiode_volts_to_kelvin_with_correction(Vcor); 
        break;
      case datrw::tsoft::POWERV:
        if ( chanval < 0.0) 
        { retval = 0.0 ; } /* changed 6/06 per Eric */
        else
        { retval = chanval * chanval; } /* power = volts^2 */
        break;
      case datrw::tsoft::PSI_500:
        retval = convert_v_to_Pres_PSI_0_500(chanval); 
        break;
      case datrw::tsoft::PSI_3000:
        retval = convert_v_to_Pres_PSI_0_3000(chanval); 
        break;
      case datrw::tsoft::KPA_3447:
        retval = convert_v_to_Pres_KPa_0_3447(chanval); 
        break;
      case datrw::tsoft::KPA_20684:
        retval = convert_v_to_Pres_KPa_0_20684(chanval); 
        break;
      case datrw::tsoft::PSI_2500:
        retval = convert_v_to_Pres_PSI_0_2500(chanval); 
        break;
      case datrw::tsoft::KPA_17236:
        retval = convert_v_to_Pres_KPa_0_17236(chanval); 
        break;
      case datrw::tsoft::MPA_17236:
        retval = convert_v_to_Pres_MPa_0_17236(chanval); 
        break;
      case datrw::tsoft::MPA_3447:
        retval = convert_v_to_Pres_MPa_0_3447(chanval); 
        break;
      case datrw::tsoft::MPA_20684:
        retval = convert_v_to_Pres_MPa_0_20684(chanval); 
        break;
      case datrw::tsoft::FAN16:
        retval = convert_v_to_FAN16(chanval); 
        break;
      case datrw::tsoft::C20P4:
        retval = chanval * 20.4;
        break;
      case datrw::tsoft::CNSP:
      case datrw::tsoft::CNFD:
        retval = chanval;
        break;
      default:
        std::cerr << "WARNING: \n"
          << "  unknown channel conversion code: " << ConvCode << "\n";
        retval = chanval;
    }

    return retval;
  } // double  convert_to_display_units(const Econversion& ConvCode,
    //                                  const double& chanval)

  /*----------------------------------------------------------------------*/

  ////////////////////////////////////////////////////////////////////////////
  // conversion code 
  ////////////////////////////////////////////////////////////////////////////
    std::string conversion_code(const Econversion& ConvCode)
  {
    switch (ConvCode)
    {
      case datrw::tsoft::SIDIODE:
        return "SIDIODE"; break;
      case datrw::tsoft::SIDIODE1:
        return "SIDIODE1"; break;
      case datrw::tsoft::PT100:
        return "PT100"; break;
      case datrw::tsoft::PT100V:
        return "PT100V"; break;
      case datrw::tsoft::PT100_K: 
        return "PT100_K"; break;
      case datrw::tsoft::PT100V_K:
        return "PT100V_K"; break;
      case datrw::tsoft::PSI_1_1: 
        return "PSI_1_1"; break;
      case datrw::tsoft::GEPLHe19: 
        return "GEPLHe19"; break;
      case datrw::tsoft::GEPLHe23: 
        return "GEPLHe23"; break;
      case datrw::tsoft::TD:
        return "TD"; break;
      case datrw::tsoft::TDV:
        return "TDV"; break;
      case datrw::tsoft::MASFLO1:
        return "MASFLO1"; break;
      case datrw::tsoft::H2OFLO1:
        return "H2OFLO1"; break;
      case datrw::tsoft::SIDIODE2:
        return "SIDIODE2"; break;
      case datrw::tsoft::SIDIODE3:
        return "SIDIODE3"; break;
      case datrw::tsoft::SIDIODE4:
        return "SIDIODE4"; break;
      case datrw::tsoft::POWERV:
        return "POWERV"; break;
      case datrw::tsoft::PSI_500:
        return "PSI_500"; break;
      case datrw::tsoft::PSI_3000:
        return "PSI_3000"; break;
      case datrw::tsoft::KPA_3447:
        return "KPA_3447"; break;
      case datrw::tsoft::KPA_20684:
        return "KPA_20684"; break;
      case datrw::tsoft::PSI_2500:
        return "PSI_2500"; break;
      case datrw::tsoft::KPA_17236:
        return "KPA_17236"; break;
      case datrw::tsoft::MPA_17236:
        return "MPA_17236"; break;
      case datrw::tsoft::MPA_3447:
        return "MPA_3447"; break;
      case datrw::tsoft::MPA_20684:
        return "MPA_20684"; break;
      case datrw::tsoft::FAN16:
        return "FAN16"; break;
      case datrw::tsoft::C20P4:
        return "C20P4"; break;
      case datrw::tsoft::CNSP:
        return "CNSP"; break;
      case datrw::tsoft::CNFD:
        return "not found"; break;
      default:
        return "unknown"; break;
    }

    return "unknown";
  } // std::string conversion_code(const Econversion& ConvCode)

} // namespace DDAS3

/*======================================================================*/

using std::cout;
using std::cerr;
using std::endl;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SIGSCALE_VERSION "\n"
    "usage: sigscale [-v] [-o] [-itype type] [-otype type]" "\n"
    "              outfile infile [t:T] [f:F] [infile [t:T] [f:F] ... ]" "\n"
    "   or: sigscale --help|-h" "\n"
    "   or: sigscale --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "outfile      name of output file" "\n"
    "infile       name of input file" "\n"
    "             t:T select traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "             f:F specifies an input file format differing from" "\n"
    "                 the format selected by \"-type\"" "\n"
    "\n"
    "-xhelp       print detailed information regarding file formats" "\n"
    "-v           be verbose" "\n"
    "-DEBUG       produce debug output" "\n"
    "-DEBREP      report internal scaling values for debugging" "\n"
    "-o           overwrite output" "\n"
    "-itype type  choose input file format (default: sff)" "\n"
    "-otype type  choose output file format (default: sff)" "\n"
    "\n"
    "This program is designed for auxilliary data from superconduction\n"
    "gravimeter SG056 at BFO. It uses scling relations provided by GWR\n"
    "to convert sample values to meaningful physical units."
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: overwrite mode
    {"o",arg_no,"-"},
    // 3: input file format
    {"itype",arg_yes,"sff"},
    // 4: output file format
    {"otype",arg_yes,"sff"},
    // 5: generate debug output
    {"DEBUG",arg_no,"-"},
    // 6: output file format
    {"xhelp",arg_no,"-"},
    // 7: generate debug output
    {"DEBREP",arg_no,"-"},
    {NULL}
  };

  // file specific keys
  static const char tracekey[]="t";
  static const char formatkey[]="f";

  // define commandline argument modifier keys
  static const char* cmdlinekeys[]
    ={tracekey, formatkey, 0};

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr); 
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(6))
  {
    cerr << usage_text << endl;
    cerr << endl;
    datrw::online_help(cerr); 
    exit(0);
  }
  // extract commandline options
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.inputformat=cmdline.string_arg(3);
  opt.outputformat=cmdline.string_arg(4);
  opt.debug=cmdline.optset(5);
  // 6 is --xhelp
  opt.debrep=cmdline.optset(7);

  /*======================================================================*/

  if (opt.debrep) {
    cout << SIGSCALE_VERSION << endl;  
    
    DDAS3::report_sidiode_table(TRANGE_12K);
    DDAS3::report_sidiode_table(TRANGE_24K);
    DDAS3::report_sidiode_table(TRANGE_100K);
    DDAS3::report_sidiode_table(TRANGE_475K);
    exit(2);
  }

  /*======================================================================*/

  if (opt.verbose)
  { cout << SIGSCALE_VERSION << endl; }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  if ((arguments.size()>1) && opt.verbose)
  {
    cout << "NOTICE: file specific information (SRCE line and file FREE)" <<
      endl
      <<    "        will be taken from first file only!" << endl;
  }

  /*======================================================================*/
  // start processing

  // open output file
  // ----------------
  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfile.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);

  // set flag to process header of first input file
  bool firstfile=true;
  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    if (opt.verbose) { cout << "open input file " << infile->name << endl; }
    std::string inputformat=opt.inputformat;
    if (infile->haskey(formatkey)) 
    {
      inputformat=infile->value(formatkey);
    }
    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(inputformat);
    std::ifstream ifs(infile->name.c_str(), iopenmode);
    datrw::ianystream is(ifs, inputformat);
      
    // handle file header
    // ------------------
    sff::FREE filefree;
    if (firstfile)
    {
      if (is.hasfree()) 
      { 
        sff::FREE infilefree;
        is >> infilefree;
        filefree.append("block read from first input file:");
        filefree.append(infilefree);
      }
      if (os.handlesfilefree()) { os << filefree; }
      if (is.hassrce())
      {
        sff::SRCE insrceline;
        is >> insrceline;
        if (os.handlessrce()) { os << insrceline; }
      }
    }

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));
    int itrace=0;
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        TFXX_debug(opt.debug, "main", "process trace #" << itrace );
        if (opt.verbose)
        { std::cout << "  process trace #" << itrace << ":"; }
        Tseries series;
        is >> series;

        sff::WID2 wid2;
        is >> wid2;
        TFXX_debug(opt.debug, "main", 
                   "  series and WID2 are read");
        sff::INFO info; 
        if (is.hasinfo()) 
        { 
          is >> info; 
        }

  /*----------------------------------------------------------------------*/
        // apply scaling
        ChannelDescription CD;
        SFFchannelid sid;
        sid.station=wid2.station;
        sid.channel=wid2.channel;
        sid.instrument=wid2.instype;
        sid.auxid=wid2.auxid;
        CD=channel(sid);

        std::string CCstring=DDAS3::conversion_code(CD.cc);

        aff::Iterator<Tseries> I(series);
        while (I.valid())
        {
          Tseries::Tvalue v= *I;
          *I = DDAS3::convert_to_display_units(CD.cc, v);
          ++I;
        }

  /*----------------------------------------------------------------------*/

        os << wid2;
        TFXX_debug(opt.debug, "main", 
                   "  series and WID are written");
        if (is.hasinfo()) 
        { 
          if (os.handlesinfo()) { os << info; }
        }
        if (is.hasfree() || true) 
        {
          sff::FREE tracefree;
          is >> tracefree;
          tracefree.append(SIGSCALE_VERSION);
          tracefree.append("read from file " + infile->name);
          tracefree.append("applied scaling for conversion code \"" 
                           + CCstring +"\"");
          if (os.handlestracefree()) { os << tracefree; } 
        }
        TFXX_debug(opt.debug, "main", 
                   "trace #" << itrace << " successfully processed");
        os << series;
      }
      else
      {
        TFXX_debug(opt.debug, "main", "skip trace #" << itrace );
        if (opt.verbose)
        { std::cout << "     skip trace #" << itrace << std::endl; }
        is.skipseries();
      }
    }
    
    // go to next file
    firstfile=false;
    ++infile;
  }
}

/* ----- END OF sigscale.cc ----- */
