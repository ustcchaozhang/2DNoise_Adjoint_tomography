/*! \file channeltranslation.cc
 * \brief translate TSOFT channel name (they are too long for SFF headers) (implementation)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/11/2009
 * 
 * translate TSOFT channel name (they are too long for SFF headers) (implementation)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/11/2009   V1.0   Thomas Forbriger
 *  - 02/12/2011   V1.1   added second barometer
 *  - 19/10/2012   V1.2   do not set NSP by default!
 *  - 10/01/2013   V1.3   add sensor data conversion codes for SG056
 *                        extract full channel description on request
 * 
 * ============================================================================
 */
#define DATRW_CHANNELTRANSLATION_CC_VERSION \
  "DATRW_CHANNELTRANSLATION_CC   V1.3"

#include <iomanip>
#include <datrwxx/error.h>
#include <datrwxx/util.h>
#include <datrwxx/tsoftdata.h>
#include <datrwxx/channeltranslation.h>

namespace datrw {

  namespace tsoft {

    /*! \brief global variable: channel name translation table
     * \ingroup group_tsoft
     *
     * This table is specific for SG056 at BFO.
     * Sensor data conversion codes are assigned based on GWR document
     * DDAS3_man_technical_ch5_UIPCsoftware_2010-07-16_BFO-Excerpt and
     * Auxill_2.txt as compiled by Walter Zuern (Id: Auxill_2.txt 752
     * 2013-01-09 12:07:01Z).
     */
    const Channel translationtable[]={
      {"BF:SG056:Grav-1",  "BFO:GR1:SG056:Gra1", CNSP},
      {"BF:SG056:Grav-2",  "BFO:GR2:SG056:Gra2", CNSP},
      {"BF:SG056:Baro-1",  "BFO:BA1:SG056:Bar1", CNSP},
      {"BF:SG056:Baro-2",  "BFO:BA2:SG056:Bar2", CNSP},
      // ---------------------------------------------
      {"BF:SG056:G1-Sig",  "BFO:G1S:SG056:Sig1", CNSP},
      {"BF:SG056:G1-Mode", "BFO:G1M:SG056:Mod1", CNSP},
      {"BF:SG056:GBal-1",  "BFO:G1B:SG056:GBa1", CNSP},
      {"BF:SG056:G2-Sig",  "BFO:G2S:SG056:Sig2", CNSP},
      {"BF:SG056:G2-Mode", "BFO:G2M:SG056:Mod2", CNSP},
      {"BF:SG056:GBal-2",  "BFO:G2B:SG056:GBa2", CNSP},
      // ---------------------------------------------
      {"BF:SG056:Tmpr1",   "BFO:TP1:SG056:Tmp1", CNSP},
      {"BF:SG056:RelHum1", "BFO:RH1:SG056:ReH1", CNSP},
      {"BF:SG056:Tmpr2",   "BFO:TP2:SG056:Tmp2", CNSP},
      {"BF:SG056:RelHum2", "BFO:RH2:SG056:ReH2", CNSP},
      // ---------------------------------------------
      {"BF:SG056:TX-Bal",  "BFO:TXB:SG056:TXBa", CNSP},
      {"BF:SG056:TY-Bal",  "BFO:TYB:SG056:TYBa", CNSP},
      {"BF:SG056:TX-Pwr",  "BFO:TXP:SG056:TXPw", POWERV},
      {"BF:SG056:TY-Pwr",  "BFO:TYP:SG056:TYPw", POWERV},
      // ---------------------------------------------
      {"BF:SG056:Dewr-P",  "BFO:DEP:SG056:DewP", PSI_1_1},
      {"BF:SG056:Dwr_Htr", "BFO:DHT:SG056:DHtr", C20P4},
      {"BF:SG056:NeckT-1", "BFO:NT1:SG056:NTe1", SIDIODE3},
      {"BF:SG056:NeckT-2", "BFO:NT2:SG056:NTe2", SIDIODE3},
      {"BF:SG056:HtrCrnt", "BFO:HCT:SG056:HCrt", POWERV},
      {"BF:SG056:Tmp-Bal", "BFO:TBA:SG056:TBal", CNSP},
      // ---------------------------------------------
      {"BF:SG056:P1GasCl", "BFO:P1C:SG056:P1GC", PSI_3000},
      {"BF:SG056:P2GasRg", "BFO:P2R:SG056:P2GR", PSI_500},
      {"BF:SG056:P3CmpHi", "BFO:P3H:SG056:P3CH", PSI_500},
      {"BF:SG056:P4CmpLo", "BFO:P4L:SG056:P4CL", PSI_500},
      {"BF:SG056:P5CmpBl", "BFO:P5B:SG056:P5CB", PSI_500},
      // ---------------------------------------------
      {"BF:SG056:TREEfan", "BFO:FAN:SG056:Tfan", FAN16},
      {"BF:SG056:AD-1",    "BFO:AD1:SG056:AD-1", CNSP},
      {"BF:SG056:FB_Mod",  "BFO:FBM:SG056:FBMo", CNSP},
      {"BF:SG056:Temp_77", "BFO:T77:SG056:Te77", CNSP},
      {"BF:SG056:Temp_6K", "BFO:T6K:SG056:Te6K", CNSP},
      {"BF:SG056:BelyT-3", "BFO:BT3:SG056:BTe3", SIDIODE3},
      {"BF:SG056:BodyT-4", "BFO:BT4:SG056:BTe4", SIDIODE4},
      {"BF:SG056:Temp_G1", "BFO:TG1:SG056:TeG1", TDV},
      {"BF:SG056:Temp_G2", "BFO:TG2:SG056:TeG2", TDV},
      {"BF:SG056:Temp_TX", "BFO:TTX:SG056:TeTX", TDV},
      {"BF:SG056:Temp_TY", "BFO:TTY:SG056:TeTY", TDV},
      {"BF:SG056:Temp_AX", "BFO:TAX:SG056:TeAX", TDV},
      {"BF:SG056:Temp_CH", "BFO:TCH:SG056:TeCH", TDV},
      {"BF:SG056:Temp_TE", "BFO:TTE:SG056:TeTE", TDV},
      // ---------------------------------------------
      {"BF:SG056:T1-Ext",  "BFO:T1X:SG056:T1Ex", TDV},
      {"BF:SG056:T2-Ext",  "BFO:T2X:SG056:T2Ex", TDV},
      {"BF:SG056:T3-Ext",  "BFO:T3X:SG056:T3Ex", TDV},
      // ---------------------------------------------
      {"BF:SG056:Temp-Gt", "BFO:TGT:SG056:TeGt", PT100V_K},
      // ---------------------------------------------
      {"BF:SG056:AGnd",    "BFO:GND:SG056:AGnd", CNSP},
      {"BF:SG056:LHeLvl",  "BFO:LHX:SG056:LHeX", GEPLHe23},
      {"BF:SG056:LHe-Lvl", "BFO:LH1:SG056:LHeL", GEPLHe23},
      {"BF:SG056:Comp_DC", "BFO:CDC:SG056:CoDC", CNSP},
      {"BF:SG056:CH_DC",   "BFO:CHD:SG056:CHDC", CNSP},
      // ---------------------------------------------
      {"BF:SG056:Vstd-1",  "BFO:VS1:SG056:VST1", CNSP},
      {"BF:SG056:Vstd-2",  "BFO:VS2:SG056:VST2", CNSP},
      // ---------------------------------------------
      {NULL, NULL, CNSP}
    }; // Channel translationtable[]

    /*======================================================================*/
    // functions

    TSOFTchannelid tchannelid(const Channel& ci)
    {
      std::string tci(ci.TSOFTname);
      Tvos vos=stringtovec(tci, ":"); 
      DATRW_assert(vos.size()==3, "unexpected number of fields");
      TSOFTchannelid retval;
      retval.location=vos[0];
      retval.instrument=vos[1];
      retval.datatype=vos[2];
      return(retval);
    } // TSOFTchannelid tchannelid(const Channel& ci)

    /*----------------------------------------------------------------------*/

    SFFchannelid schannelid(const Channel& ci)
    {
      std::string sci(ci.SFFname);
      Tvos vos=stringtovec(sci, ":"); 
      DATRW_assert(vos.size()==4, "unexpected number of fields");
      SFFchannelid retval;
      retval.station=vos[0];
      retval.channel=vos[1];
      retval.instrument=vos[2];
      retval.auxid=vos[3];
      return(retval);
    } // SFFchannelid schannelid(const Channel& ci)

    /*----------------------------------------------------------------------*/

    //! \brief helper function: print fixed width field
    void pfield(std::ostream& os, const std::string& s, const int& w)
    {
      os.width(w);
      os.setf(std::ios_base::left, std::ios_base::adjustfield);
      os << s.substr(0,w) << " ";
    } // void pfield(std::ostream& os, const std::string& s, const int& w)

    /*----------------------------------------------------------------------*/

    //! \brief helper function: formatted output
    void pline(std::ostream& os, 
               const std::string& s1,
               const std::string& s2,
               const std::string& s3,
               const std::string& s4,
               const std::string& s5,
               const std::string& s6,
               const std::string& s7)
    {
      pfield(os, s1, 8);
      pfield(os, s2, 10);
      pfield(os, s3, 8);
      os << " --> ";
      pfield(os, s4, 7);
      pfield(os, s5, 7);
      pfield(os, s6, 10);
      pfield(os, s7, 5);
      os << std::endl;
    } // void pline

    /*----------------------------------------------------------------------*/

    void reporttranslation(std::ostream& os)
    {
      const Channel* pc=translationtable;
      os << "Translation table for TSOFT channels:" << std::endl;
      pline(os, "TSOFT", "", "", "SFF", "", "", "");
      pline(os, "location", "instrument", "datatype",
            "station", "channel", "instrument", "auxid");
      const char* line="----------";
      pline(os, line, line, line, line, line, line, line);
      while (pc->TSOFTname != NULL)
      {
        DATRW_assert(pc->TSOFTname != NULL, "UUPS");
        TSOFTchannelid tci=tchannelid(*pc);
        SFFchannelid sci=schannelid(*pc);
        pline(os,
              tci.location,
              tci.instrument,
              tci.datatype,
              sci.station,
              sci.channel,
              sci.instrument,
              sci.auxid);
        ++pc;
      }
      os << "If none of the entries matches, we take location for station,\n"
        << "instrument for instrument, and datatype for channel and auxid."
        << std::endl;
    } // void reporttranslation(std::ostream& os)

    /*----------------------------------------------------------------------*/

    void reportdatacomments(std::ostream& os)
    {
      os << "Comments found in 1s data file (A2100101.056):" << std::endl;
      os << "15    LHe-Lvl      3     0.0    100.0      Volt         %     GEPLHe23" << std::endl;
      os << "16    AGnd         1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "17    AD-1         1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "18    TREEfan      1   -10.0     10.0      Volt     Volts      4.0" << std::endl;
      os << "19    G1-Sig       1   -10.0     10.0      Volt   nm/sec2  none" << std::endl;
      os << "20    G1-Mode      1   -10.0     10.0      Volt     Volts  none" << std::endl;
      os << "21    G2-Sig       1   -10.0     10.0      Volt   nm/sec2  none" << std::endl;
      os << "22    G2-Mode      1   -10.0     10.0      Volt     Volts  none" << std::endl;
      os << "23    Dewr-P       1    -2.0      2.0      Volt       PSI     PSI_1_1" << std::endl;
      os << "24    GBal-1       1   -10.0     10.0      Volt   nm/sec2  none" << std::endl;
      os << "25    TX-Pwr       1   -10.0     10.0      Volt    %Power     POWERV" << std::endl;
      os << "26    TX-Bal       1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "27    NeckT-1      1     1.9      8.5      Volt    Kelvin     SiDiode-3" << std::endl;
      os << "28    NeckT-2      1     1.9      8.5      Volt    Kelvin     SiDiode-3" << std::endl;
      os << "29    BelyT-3      1     1.9      8.5      Volt    Kelvin     SiDiode-3" << std::endl;
      os << "30    BodyT-4      1     1.9      8.5      Volt    Kelvin     SiDiode-4" << std::endl;
      os << "31    LHeLvl       6     1.0    100.0      Volt         %     GEPLHe23" << std::endl;
      os << "32    GBal-2       1   -10.0     10.0      Volt   nm/sec2  none" << std::endl;
      os << "33    TY-Pwr       1   -10.0     10.0      Volt    %Power     POWERV" << std::endl;
      os << "34    TY-Bal       1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "35    Tmp-Bal      1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "36    HtrCrnt      1   -10.0     10.0      Volt    %Power     POWERV" << std::endl;
      os << "37    Temp_6K      1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "38    Temp_77      1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "39    FB_Mod       1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "40    P1GasCl      1   -10.0     10.0      Volt       PSI     PSI-3000" << std::endl;
      os << "41    P2GasRg      1   -10.0     10.0      Volt       PSI     PSI-500" << std::endl;
      os << "42    P3CmpHi      1   -10.0     10.0      Volt       PSI     PSI-500" << std::endl;
      os << "43    P4CmpLo      1   -10.0     10.0      Volt       PSI     PSI-500" << std::endl;
      os << "44    P5CmpBl      1   -10.0     10.0      Volt       PSI     PSI-500" << std::endl;
      os << "55    Temp-Gt      1     0.0      1.0      Volt    Kelvin     PT100V_K" << std::endl;
      os << "56    Dwr_Htr      1   -10.0     10.0      Volt        mA     20.0" << std::endl;
      os << "57    Comp_DC      1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "58    CH_DC        1   -10.0     10.0      Volt     Volts      1.0" << std::endl;
      os << "59    Tmpr1        1     0.0      0.0   Celsius   Celsius      1.0" << std::endl;
      os << "60    Tmpr2        1     0.0      0.0   Celsius   Celsius      1.0" << std::endl;
      os << "61    RelHum1      1     0.4      1.7      Pcnt         %      1.0" << std::endl;
      os << "62    RelHum2      1     0.4      1.7      Pcnt         %      1.0" << std::endl;
    } // void reporttranslation(std::ostream& os)

    /*----------------------------------------------------------------------*/

    bool translationisunique(const bool& verbose)
    {
      bool retval=true;
      const Channel* pc1=translationtable;
      while (pc1->TSOFTname != NULL)
      {
        TSOFTchannelid tci1=tchannelid(*pc1);
        SFFchannelid sci1=schannelid(*pc1);
        const Channel* pc2=pc1;
        ++pc2;
        while (pc2->TSOFTname != NULL)
        {
          TSOFTchannelid tci2=tchannelid(*pc2);
          SFFchannelid sci2=schannelid(*pc2);
          if ((tci1.location==tci2.location)
              && (tci1.instrument==tci2.instrument)
              && (tci1.datatype==tci2.datatype))
          { 
            std::cout << "ATTENTION: "
              << "TSOFT entry appears twice in translation table:"
              << std::endl;
            std::cout << "  "
              << tci1.location << ":" 
              << tci1.instrument << ":"
              << tci1.datatype << std::endl;
            retval=false;
          }
          if ((sci1.station==sci2.station)
              && (sci1.channel==sci2.channel)
              && (sci1.instrument==sci2.instrument)
              && (sci1.auxid==sci2.auxid))
          { 
            std::cout << "ATTENTION: "
              << "SFF entry appears twice in translation table:"
              << std::endl;
            std::cout << "  "
              << sci1.station << ":" 
              << sci1.channel << ":" 
              << sci1.instrument << ":"
              << sci1.auxid << std::endl;
            retval=false;
          }
          ++pc2;
        }
        ++pc1;
      }
      return(retval);
    } // bool translationisunique(const bool& verbose)

    /*----------------------------------------------------------------------*/

    SFFchannelid translate(const TSOFTchannelid& ci)
    {
      /*
          std::cout << "translate: " 
            << ci.location << ":" 
            << ci.instrument << ":"
            << ci.datatype << ":" << std::endl;
            */
      SFFchannelid retval;
      retval.station=ci.location;
      retval.channel=ci.datatype.substr(0,3);
      retval.instrument=ci.instrument;
      retval.auxid=ci.datatype.substr(3);
      const Channel* pc=translationtable;
      bool hot=true;
      while (hot && (pc->TSOFTname != NULL))
      {
        DATRW_assert(pc->TSOFTname != NULL, 
                     "UUPS... this is a programming error!");
        TSOFTchannelid tci=tchannelid(*pc);
        if ((tci.location==ci.location)
            && (tci.instrument==ci.instrument)
            && (tci.datatype==ci.datatype))
        {
          /*
          std::cout << "found: " 
            << tci.location << ":" 
            << tci.instrument << ":"
            << tci.datatype << ":" << std::endl;
            */
          SFFchannelid sci=schannelid(*pc);
          retval.station=sci.station;
          retval.channel=sci.channel;
          retval.instrument=sci.instrument;
          retval.auxid=sci.auxid;
          hot=false;
        }
        ++pc;
      }
      return(retval);
    } // SFFchannelid translate(const TSOFTchannelid& ci)

    /*----------------------------------------------------------------------*/

    ChannelDescription channel(const SFFchannelid& insid)
    {
      SFFchannelid sid;
      sid.station=datrw::util::trimws(insid.station);
      sid.channel=datrw::util::trimws(insid.channel);
      sid.instrument=datrw::util::trimws(insid.instrument);
      sid.auxid=datrw::util::trimws(insid.auxid);

      /*
      std::cout << "translate: " 
        << sid.station << ":" 
        << sid.channel << ":"
        << sid.instrument << ":"
        << sid.auxid << std::endl;
        */

      // initialize return value with default
      ChannelDescription retval;
      TSOFTchannelid& tid=retval.tid;
      tid.location=sid.station;
      tid.datatype=sid.channel;
      tid.instrument=sid.instrument;
      retval.sid=sid;
      retval.cc=CNFD;

      const Channel* pc=translationtable;
      bool hot=true;
      while (hot && (pc->TSOFTname != NULL))
      {
        DATRW_assert(pc->TSOFTname != NULL, 
                     "UUPS... this is a programming error!");
        SFFchannelid sffid=schannelid(*pc);
        if ((sid.station==sffid.station)
            && (sid.instrument==sffid.instrument)
            && (sid.channel==sffid.channel)
            && (sid.auxid==sffid.auxid))
        {
          /*
          std::cout << "found: " 
            << tci.location << ":" 
            << tci.instrument << ":"
            << tci.datatype << ":" << std::endl;
            */
          tid=tchannelid(*pc);
          retval.cc=pc->ConversionCode;
          hot=false;
        }
        ++pc;
      }
      return(retval);
    } // ChannelDescription channel(const SFFchannelid& sci)

  } // namespace tsoft

} // namespace datrw

/* ----- END OF channeltranslation.cc ----- */
