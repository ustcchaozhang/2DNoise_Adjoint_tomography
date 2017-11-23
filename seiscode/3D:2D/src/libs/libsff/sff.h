/*! \file sff.h
 * \brief provide C prototypes for C-code linking against libsff (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 14/11/2010
 * 
 * provide C prototypes for C-code linking against libsff (prototypes)
 *
 * ----
 * This file is part of libsff.
 *
 * libsff is free software; you can redistribute it and/or modify
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
 * This file was created by
 *
 * f2c -u -f -P stuff.f
 *
 * It contains prototypes for functions in stuff.f as are needed when linking
 * C-code against libsff. Appropriate Fortran typedefs (integer, real, etc)
 * must be set before including sff.h
 *
 * The return values of sff_getdt and sff_libversion are declared doublereal,
 * which is the f77 default.
 * 
 * REVISIONS and CHANGES 
 *  - 14/11/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SFF_H_VERSION

#define TF_SFF_H_VERSION \
  "TF_SFF_H   V1.0"
#define TF_SFF_H_CVSID \
  "$Id$"

extern doublereal sff_libversion__(void);
extern int sff_f2i__(integer *idata, real *fdata, integer *nsamples, real *ampfac);
extern int sff_i2f__(integer *idata, real *fdata, integer *nsamples, real *ampfac);
extern int sff_new__(integer *lu, char *filename, integer *ierr, ftnlen filename_len);
extern int sff_wopen__(integer *lu, char *filename, integer *ierr, ftnlen filename_len);
extern int sff_wopenf__(integer *lu, char *filename, char *lines, integer *nline, integer *ierr, ftnlen filename_len, ftnlen lines_len);
extern int sff_wopens__(integer *lu, char *filename, char *type__, char *cs, real *c1, real *c2, real *c3, char *date, char *time, integer *ierr, ftnlen filename_len, ftnlen type_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_wopenfs__(integer *lu, char *filename, char *lines, integer *nline, char *type__, char *cs, real *c1, real *c2, real *c3, char *date, char *time, integer *ierr, ftnlen filename_len, ftnlen lines_len, ftnlen type_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_ropen__(integer *lu, char *filename, real *version, char *timestamp, char *code, integer *ierr, ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len);
extern int sff_ropenf__(integer *lu, char *filename, real *version, char *timestamp, char *code, integer *nline, char *lines, integer *lenmax, integer *lindim, integer *ierr, ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len, ftnlen lines_len);
extern int sff_ropens__(integer *lu, char *filename, real *version, char *timestamp, char *code, char *type__, char *cs, real *c1, real *c2, real *c3, char *date, char *time, integer *ierr, ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len, ftnlen type_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_ropenfs__(integer *lu, char *filename, real *version, char *timestamp, char *code, integer *nline, char *lines, integer *lenmax, integer *lindim, char *type__, char *cs, real *c1, real *c2, real *c3, char *date, char *time, integer *ierr, ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len, ftnlen lines_len, ftnlen type_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_wtrace__(integer *lu, char *wid2line, integer *nsamp, real *fdata, integer *idata, logical *last, integer *ierr, ftnlen wid2line_len);
extern int sff_wtracef__(integer *lu, char *wid2line, integer *nsamp, real *fdata, integer *idata, logical *last, integer *nline, char *lines, integer *ierr, ftnlen wid2line_len, ftnlen lines_len);
extern int sff_wtracei__(integer *lu, char *wid2line, integer *nsamp, real *fdata, integer *idata, logical *last, char *cs, real *c1, real *c2, real *c3, integer *nstack, integer *ierr, ftnlen wid2line_len, ftnlen cs_len);
extern int sff_wtracefi__(integer *lu, char *wid2line, integer *nsamp, real *fdata, integer *idata, logical *last, integer *nline, char *lines, char *cs, real *c1, real *c2, real *c3, integer *nstack, integer *ierr, ftnlen wid2line_len, ftnlen lines_len, ftnlen cs_len);
extern int sff_rtrace__(integer *lu, real *tanf, real *dt, char *wid2line, integer *nsamp, real *fdata, integer *idata, char *code, logical *last, integer *ierr, ftnlen wid2line_len, ftnlen code_len);
extern int sff_rtracef__(integer *lu, real *tanf, real *dt, char *wid2line, integer *nsamp, real *fdata, integer *idata, char *code, logical *last, integer *nline, char *lines, integer *lindim, integer *lenmax, integer *ierr, ftnlen wid2line_len, ftnlen code_len, ftnlen lines_len);
extern int sff_rtracei__(integer *lu, real *tanf, real *dt, char *wid2line, integer *nsamp, real *fdata, integer *idata, char *code, logical *last, char *cs, real *c1, real *c2, real *c3, integer *nstack, integer *ierr, ftnlen wid2line_len, ftnlen code_len, ftnlen cs_len);
extern int sff_rtracefi__(integer *lu, real *tanf, real *dt, char *wid2line, integer *nsamp, real *fdata, integer *idata, char *code, logical *last, integer *nline, char *lines, integer *lindim, integer *lenmax, char *cs, real *c1, real *c2, real *c3, integer *nstack, integer *ierr, ftnlen wid2line_len, ftnlen code_len, ftnlen lines_len, ftnlen cs_len);
extern int sff_modwid2samprat__(char *wid2line, real *samprat, ftnlen wid2line_len);
extern int sff_modwid2samps__(char *wid2line, integer *samps, ftnlen wid2line_len);
extern int sff_modwid2date__(char *wid2line, integer *year, integer *month, integer *day, ftnlen wid2line_len);
extern int sff_modwid2time__(char *wid2line, integer *hour, integer *minute, real *second, ftnlen wid2line_len);
extern int sff_modwid2shift__(char *wid2line, real *tmin, real *tsec, ftnlen wid2line_len);
extern logical sff_timeisleapyear__(integer *year);
extern int sff_timesetdoy__(integer *doy, integer *year, integer *month, integer *day);
extern integer sff_timegetdoy__(integer *year, integer *month, integer *day);
extern int sff_timesplit__(real *tsec, integer *day, integer *hour, integer *minute, real *second);
extern int sff_timeadd__(integer *year1, integer *doy1, integer *hour1, integer *minute1, real *second1, integer *year2, integer *doy2, integer *hour2, integer *minute2, real *second2, integer *year, integer *doy, integer *hour, integer *minute, real *second);
extern int sff_quickwrite__(integer *lu, char *wid2line, integer *nsamp, integer *idata, real *ampfac, ftnlen wid2line_len);
extern int sff_quickread__(integer *lu, char *wid2line, integer *nsamp, real *tanf, real *dt, integer *idata, real *ampfac, integer *ierr, ftnlen wid2line_len);
extern int sff_wstatus__(integer *lu, char *code, ftnlen code_len);
extern int sff_rstatus__(integer *lu, real *version, char *timestamp, char *code, integer *ierr, ftnlen timestamp_len, ftnlen code_len);
extern int sff_wfree__(integer *lu, integer *nline, char *lines, ftnlen lines_len);
extern int sff_rfree__(integer *lu, integer *nline, char *lines, integer *lenmax, integer *lindim, integer *ierr, ftnlen lines_len);
extern int sff_skipfree__(integer *lu, integer *ierr);
extern int sff_wsource__(integer *lu, char *typh, char *cs, real *c1, real *c2, real *c3, char *date, char *time, ftnlen typh_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_rsource__(integer *lu, char *typ, char *cs, real *c1, real *c2, real *c3, char *date, char *time, integer *ierr, ftnlen typ_len, ftnlen cs_len, ftnlen date_len, ftnlen time_len);
extern int sff_winfo__(integer *lu, char *cs, real *c1, real *c2, real *c3, integer *nstack, ftnlen cs_len);
extern int sff_rinfo__(integer *lu, char *cs, real *c1, real *c2, real *c3, integer *nstack, integer *ierr, ftnlen cs_len);
extern int sff_checksum__(integer *nsamp, integer *idata, integer *nchecksum);
extern int sff_rwdata__(integer *lu, char *wid2line, char *code, real *ampfac, integer *nsamp, integer *idata, logical *rwflag, integer *ierr, ftnlen wid2line_len, ftnlen code_len);
extern int sff_wdata__(integer *lu, char *wid2line, integer *nsamp, integer *idata, real *ampfac, char *code, ftnlen wid2line_len, ftnlen code_len);
extern int sff_rwid2__(integer *lu, char *wid2line, integer *nsamp, real *tanf, real *dt, integer *nchar, real *ampfac, char *code, integer *ierr, ftnlen wid2line_len, ftnlen code_len);
extern int sff_rdata__(integer *lu, char *wid2line, integer *nsamp, real *tanf, real *dt, integer *idata, real *ampfac, char *code, integer *ierr, ftnlen wid2line_len, ftnlen code_len);
extern int sff_rdata2__(integer *lu, integer *nsamp, integer *idata, char *cbuf, integer *nchar, integer *ierr, ftnlen cbuf_len);
extern int sff_skipdata__(integer *lu, char *code, logical *last, integer *ierr, ftnlen code_len);
extern int sff_prepwid2__(integer *nsamp, real *samprat, char *station, integer *year, integer *month, integer *day, integer *hour, integer *minute, char *comp, char *auxid, char *instyp, real *second, real *calib, real *calper, real *hang, real *vang, char *wid2line, integer *ierr, ftnlen station_len, ftnlen comp_len, ftnlen auxid_len, ftnlen instyp_len, ftnlen wid2line_len);
extern int sff_getdate__(char *wid2line, char *date, ftnlen wid2line_len, ftnlen date_len);
extern int sff_gettime__(char *wid2line, char *time, ftnlen wid2line_len, ftnlen time_len);
extern int sff_getstation__(char *wid2line, char *sta, ftnlen wid2line_len, ftnlen sta_len);
extern int sff_getchannel__(char *wid2line, char *channel, ftnlen wid2line_len, ftnlen channel_len);
extern integer sff_getn__(char *wid2line, ftnlen wid2line_len);
extern doublereal sff_getdt__(char *wid2line, ftnlen wid2line_len);
extern int sff_trimlen__(char *string, integer *ntrim, ftnlen string_len);
/*:ref: idate_ 14 1 4 */
/*:ref: itime_ 14 1 4 */
/*:ref: dcomp6_ 14 6 4 13 4 4 4 124 */
/*:ref: remdif1_ 14 2 4 4 */
/*:ref: dif1_ 14 2 4 4 */
/*:ref: cmprs6_ 14 6 4 4 4 13 4 124 */

#endif // TF_SFF_H_VERSION (includeguard)
/* ----- END OF sff.h ----- */
