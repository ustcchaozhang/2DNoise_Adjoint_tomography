/*! \file thiesdl1format.cc
 * \brief format definitions for ThiesDL1 files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * format definitions for ThiesDL1 files (implementation)
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
 * REVISIONS and CHANGES 
 *  - 13/09/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_THIESDL1FORMAT_CC_VERSION \
  "DATRW_THIESDL1FORMAT_CC   V1.0   "

#include <datrwxx/thiesdl1.h>

namespace datrw {

  /*! \brief all the stuff to read Thies DL1 data
   *
   * \defgroup group_thiesdl1 Reading module for: Thies DL1 data
   *
   * Thies/DL1 is the type fo pluviometer operated at BFO.
   * \sa \ref page_thiesdl1_format
   */

  /*! \brief Format properties
   * \ingroup group_thiesdl1
   * @{
   */
  const bool thiesdl1::isbinary=false;
  const char* const thiesdl1::streamID="thiesdl1";
  /**@}*/

/*======================================================================*/

  /*! \page page_thiesdl1_format Definition of the ThiesDL1 file format
   *
   * This file format is used to store data of the Thies DL1/N pluviometer at
   * BFO.
   * The files are produced by program DL1logger, which accomplishes data
   * acquisition by controlling the Thies DL1/N data logger and reading the
   * data from the logger.
   *
   * Sections in this page:
   *   - \ref sec_page_thiesdl1_formatdefinition
   *   - \ref sec_page_thiesdl1_midnight
   *   - \ref sec_page_thiesdl1_current
   *   - \ref sec_page_thiesdl1_references
   *
   * \section sec_page_thiesdl1_formatdefinition Structure of a data file
   * A typical data file stored by DL1logger is 20110911.asc:
   * \verbatim
# Logger status :
# ---------------
# Date:  12.09.11
# Time:   1:01:15
# Battery : BAT OK
# MemCycle:  Event
# ERROR: duplicate sample (index 1439): 24.000   0.0 11.09.11 24:00
# earliest date: 254 11.09.2011 00:00:00.000000
# latest date:   255 12.09.2011 00:00:00.000000
# creation date: 255 12.09.2011 01:01:19.000000
# initial line:  Data : 11.09.11  0:00
# final line:    END OF DATA BFO   1  DL1/N  V1.10a
Data : 11.09.11  0:00
16.233   0.1 11.09.11 16:14
16.250   0.2 11.09.11 16:15
16.267   0.1 11.09.11 16:16
16.283   0.1 11.09.11 16:17
16.317   0.1 11.09.11 16:19
16.617   0.1 11.09.11 16:37
16.650   0.1 11.09.11 16:39
16.667   0.1 11.09.11 16:40
16.683   0.1 11.09.11 16:41
16.867   0.1 11.09.11 16:52
17.483   0.1 11.09.11 17:29
19.417   0.1 11.09.11 19:25
19.483   0.2 11.09.11 19:29
19.500   0.1 11.09.11 19:30
19.517   0.1 11.09.11 19:31
19.566   0.1 11.09.11 19:34
19.600   0.1 11.09.11 19:36
19.633   0.1 11.09.11 19:38
19.800   0.1 11.09.11 19:48
19.850   0.1 11.09.11 19:51
19.883   0.1 11.09.11 19:53
19.983   0.1 11.09.11 19:59
20.050   0.1 11.09.11 20:03
20.067   0.1 11.09.11 20:04
20.117   0.1 11.09.11 20:07
20.150   0.1 11.09.11 20:09
20.333   0.1 11.09.11 20:20
20.600   0.1 11.09.11 20:36
20.717   0.1 11.09.11 20:43
21.533   0.1 11.09.11 21:32
24.000   0.0 11.09.11 24:00
24.000   0.0 11.09.11 24:00
END OF DATA BFO   1  DL1/N  V1.10a
\endverbatim
   *
   * It begins with a header produced by DL1logger, first reporting the
   * current status when creating the data file:
   * \verbatim
# Logger status :
# ---------------
# Date:  12.09.11
# Time:   1:01:15
# Battery : BAT OK
# MemCycle:  Event
\endverbatim
   *
   * Then reporting log messages or error conditions produced while decoding
   * the data stream:
  * \verbatim
# ERROR: duplicate sample (index 1439): 24.000   0.0 11.09.11 24:00
\endverbatim
   *
   * This is followed by a statement indicating the time span for which data
   * was requested from the data logger, the time of creation of the data file
   * and the first and final line sent by the logger:
   * \verbatim
# earliest date: 254 11.09.2011 00:00:00.000000
# latest date:   255 12.09.2011 00:00:00.000000
# creation date: 255 12.09.2011 01:01:19.000000
# initial line:  Data : 11.09.11  0:00
# final line:    END OF DATA BFO   1  DL1/N  V1.10a
\endverbatim
   *
   * All these header lines have a \c # in the first column.
   * After the header, the actual data sent by the Thies DL1/N logger is
   * dumped.
   * It starts with the initial line sent by the logger:
   * \verbatim
Data : 11.09.11  0:00
\endverbatim
   *
   * After the initial line, the logger produced one data line for each minute
   * for which precipitation was recorded.
   * Minutes for which no precipitation has occured, no lines are present. 
   * Each line consists of four fields. For example:
   * \verbatim
16.233   0.1 11.09.11 16:14
\endverbatim
   * The fields are:
   * -# the time in decimal hours (in UT)
   * -# the recorded precipitation for the reported minute in mm
   * -# the date in format DD.MM.YY
   * -# the time in format HH:MM (in UT)
   *
   * The data file is finished with the final line sent by the logger:
   * \verbatim
END OF DATA BFO   1  DL1/N  V1.10a
\endverbatim
   * This line indicates the station for which data was recorded (here: BFO)
   * and the type of instrument used (here: DL1/N).
   *
   * \section sec_page_thiesdl1_midnight Timing at end of day
   * The time reported in the data line is the time when the current
   * precipitaion count interval ended.
   * A line for 22:46 provides the amount of precipitation recorded for the
   * time interval from 22:45 to 22:46.
   * Consequently the last line in a file is for 24:00.
   * For example the file 20090714.asc ends with
   * \verbatim
...
23.917   0.2 14.07.09 23:55
23.933   0.3 14.07.09 23:56
23.950   0.7 14.07.09 23:57
23.967   0.7 14.07.09 23:58
23.983   0.6 14.07.09 23:59
24.000   0.7 14.07.09 24:00
END OF DATA BFO   1  DL1/N  V1.10a
\endverbatim
   * The next file (20090715.asc) starts with
   * \verbatim
# Logger status :
# ---------------
# Date:  16.07.09
# Time:   1:01:17
# Battery : BAT OK
# MemCycle:  Event
# earliest date: 196 15.07.2009 00:00:00.000000
# latest date:   197 16.07.2009 00:00:00.000000
# creation date: 197 16.07.2009 01:01:19.000000
# initial line:  Data : 15.07.09  0:00
# final line:    END OF DATA BFO   1  DL1/N  V1.10a
Data : 15.07.09  0:00
00.017   1.2 15.07.09 00:01
00.033   0.8 15.07.09 00:02
00.050   0.7 15.07.09 00:03
00.067   0.5 15.07.09 00:04
00.083   0.5 15.07.09 00:05
...
\endverbatim
   *
   * \section sec_page_thiesdl1_current File for the current hour
   * Files for the current hour (i.e. hours not yet completed) are given the
   * name \c active.asc. 
   * An example is
   * \verbatim
# Logger status :
# ---------------
# Date:  13.09.11
# Time:   7:55:02
# Battery : BAT OK
# MemCycle:  Event
# earliest date: 256 13.09.2011 00:00:00.000000
# latest date:   256 13.09.2011 07:57:02.000000
# creation date: 256 13.09.2011 07:55:05.000000
# initial line:  Data : 13.09.11  0:00
# final line:    END OF DATA BFO   1  DL1/N  V1.10a
Data : 13.09.11  0:00
03.383   0.1 13.09.11 03:23
04.400   0.1 13.09.11 04:24
END OF DATA BFO   1  DL1/N  V1.10a
\endverbatim
   * The time span for which this file is valid is only apparent from the file
   * header.
   *
   * \section sec_page_thiesdl1_references References
   * \sa Product page of the Thies company:
   *   http://www.thiesclima.com/Datalogger%20DL1%20N.html
   * \sa Thies DL1 data acquisition program:
   *   https://git.scc.kit.edu/Seitosh/Seitosh/tree/master/src/conv/ThiesDL1
   * \sa \ref group_thiesdl1
   */

} // namespace datrw

/* ----- END OF thiesdl1format.cc ----- */
