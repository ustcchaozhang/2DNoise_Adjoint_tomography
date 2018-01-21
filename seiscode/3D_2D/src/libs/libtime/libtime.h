/* this is <libtime.h>
 * ----------------------------------------------------------------------------
 *
 * 12/08/97 by Thomas Forbriger (IfG Stuttgart)
 *
 * some definitions and prototypes for the C libtime interface
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *    12/08/97   V1.0   Thomas Forbriger
 *    06/08/00   V2.0   do not include f2c - rather copy relevant definitions
 *                      added C linkage convention for C++ 
 *                      use namespace time_kernel
 *    14/12/07   V2.1   default integer type is different for g77 on 64bit
 *                      machines; distinguish explicitely
 *                      discarded farray element in union time_Tu
 *    17/12/07   V2.2   introduced typedef timeint
 *    13/11/10   V2.3   abort, if f2c.h is read for 64bit CPU compilation
 *
 * ============================================================================
 */

#ifndef _TF_LIBTIME_H
#define _TF_LIBTIME_H

/* #include <f2c.h> */
/* #include <stdio.h> */

#ifdef __cplusplus
namespace time_kernel {
extern "C" {
#endif

/*
 * all f2c stuff that is needed here
 * =================================
 */

#ifndef F2C_INCLUDE
/* FORTRAN (f2c) types needed by the wrapper functions */

/* g77 on 64bit system apparently doesn't use long int */
#ifdef __x86_64
typedef int integer;
typedef double doublereal;
typedef int logical;
typedef int ftnlen;
#else
typedef long int integer;
typedef double doublereal;
typedef long int logical;
typedef long int ftnlen;
#endif
#else
#warning "f2c.h is read from somewhere else!"
#ifdef __x86_64
#error "long int integer type in f2c.h does not work for x86_64 compilation!"
#endif
#endif

/* type of integer that we use externally */
typedef integer timeint;

/*
 * a few tf-macros needed by the C specific functions
 * ==================================================
 */

/* return value of time_read on success */
#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif

/* return value of time_read on failure */
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

/* error handling macro used within time_read */
#ifndef RETURNERROR
#define RETURNERROR( EXPR , SUB, STR, CODE )\
  if ( EXPR ) { fprintf(stderr, "ERROR (%s):\n   %s\n", SUB, STR );\
      return(CODE); }
#endif

/*S*/
/*
 * some macro constants
 * ====================
 */

/* value returned by time_isleapyear in case year IS a leap-year */
#define TIME_ISLEAP (1)
/* value returned by time_isleapyear in case year IS NOT a leap-year */
#define TIME_ISNOTLEAP (0)

/* length of static string buffer in time_sprint */
#define TIME_SLEN (35)

/* 
 * time data structure
 * ===================
 */

/* standard structure to hold date record */
typedef struct {
  integer year;    /* year  (=0 for relative times)                */
  integer doy;     /* day within yaer (may be 0 for relative times */
  integer hour;    /* hour within day                              */
  integer minute;  /* minute within hour                           */
  integer second;  /* second within minute                         */
  integer milsec;  /* millisecond within second                    */
  integer micsec;  /* microsecond within millisecond               */
} time_Ts;

/*E*/

/* this unios is used to convert date records between different
 * representations and language specific functions */
typedef union {
  integer array[7];  /* linear access in C */
  time_Ts s;         /* the structure that is fed in */
} time_Tu;

/*S*/
/*
 * wrapper function prototypes
 * ===========================
 */

/* void time_add(time_Ts date1, time_Ts date2, time_Ts *date3)
 *
 * date1:   input: any date record
 * date2:   input: date record (may be absolute if date1 is relative)
 * date3:   output: sum of date1 and date2
 */
void time_add(time_Ts, time_Ts, time_Ts *);

/* void time_clear(time_Ts *date)
 *
 * date:    input: any date record
 *          output: zero relative time
 */
void time_clear(time_Ts *);

/* long int time_compare(time_Ts date1, time_Ts date2)
 *
 * date1:   input: any date record
 * date2:   input: any date record (must be relative is date1 is relative,
 *                 must be absolute if date1 is absolute)
 * returns: 1  if date1 >  date2
 *          0  if date1 == date2
 *          -1 if date1 <  date2
 *          -2 when mixing relative and absolute date records
 */
integer time_compare(time_Ts, time_Ts);

/* void time_copy(time_Ts date1, time_Ts *date2)
 *
 * date1:   input: any date record
 * date2:   output: copy of date1
 */
void time_copy(time_Ts, time_Ts *);

/* void time_div(time_Ts date1, time_Ts *date2, long int n, long int *rest)
 *
 * date1:   input: any relative time
 * date2:   output: n-th fraction of date1
 * n:       input: divisor for date1
 * rest:    output: rest of division in mircoseconds
 *                  always: date1 >= (n*date2)
 */
void time_div(time_Ts, time_Ts *, timeint, timeint *);

/* void time_finish(time_Ts *date)
 *
 * date:    input: any date record
 *          output: fully qualified and regularized date record
 */
void time_finish(time_Ts *);

/* void time_fullyear(long int *year)
 *
 * year:    ainput: ny year value (may be a 2-digit abbreviation)
 *          output: a full qualified year value 
 */
void time_fullyear(timeint *);

/* void time_getdate(long int *day, long int *month, time_Ts date)
 *
 * day:     output: day within month index of date
 * month:   output: month wihtin year index of date
 * date:    input: any absolute date record
 */
void time_getdate(timeint*, timeint*, time_Ts);

/* long int time_isleapyear(long int year)
 *
 * year:    input: full qualified year value to be checked
 * returns: TIME_ISLEAP       if argument is a leap-year
 *          TIME_ISNOLEAP     if argument is not a leap-year
 */
integer time_isleapyear(timeint);

/* double time_libversion
 *
 * returns: version number of library kernel
 */
double time_libversion();

/* void time_mul(time_Ts date1, time_Ts *date2, long int n)
 *
 * date1:   input: any relative date record
 * date2:   output: n times date1
 * n:       input: factor to multiply date1 with
 */
void time_mul(time_Ts, time_Ts *, timeint);

/* void time_nfit(time_Ts date1, time_Ts date2, long int *n, time_Ts *full)
 *
 * date1:   input: any relative time record
 * date2:   input: any relative time record
 * n:       output: number os date2 intervals the fit best into date1
 *                  so that abs((n*date2)-date1) <= date2/2
 * full:    output: full time span defined by n and date2 (full=n*date2)
 */
void time_nfit(time_Ts, time_Ts, timeint *, time_Ts *);

/* void time_norm(time_Ts *date)
 *
 * date:    input: any date record
 *          output: regularized date record
 */
void time_norm(time_Ts *);

/* void time_setdoy(long int day, long int month, time_Ts *date)
 *
 * day:     input: day index within month
 * month:   input: month index within year
 * date:    input: any date record with year set
 *          output: has correct doy set from day and month 
 */
void time_setdoy(timeint, timeint, time_Ts *);

/* void time_sub(time_Ts date1, time_Ts date2, time_Ts date3)
 *
 * date1:   input: any date record
 * date2:   input: any date record
 * date3:   output: absolute (positive) difference between date1 and date2
 */
void time_sub(time_Ts, time_Ts, time_Ts *);

/*
 * prototypes of pure C functions
 * ==============================
 */

/* int time_read(time_Ts *date, const char *string)
 *
 * string:  input: character representation of a time with the fields in the
 *                 following order:
 *                    year month day hour minute seconds
 *                 - the fields must be separated by non-numeric characters
 *                 - all fields except the field 'seconds' must be integer
 *                 - you may omit any number of trailing fields
 *                 - year AND month must be zero to specify a relative time
 * date:    output: full qualified and regularized date record specified by
 *                  string
 * returns: EXIT_SUCCESS on success
 *          EXIT_FAILURE on FAILURE
 *
 * NOTICE: time_read is not the direct inverse operation of time_sprint
 */
int time_read(time_Ts *, const char *);

/* char *time_sprint(time_Ts date)
 *
 * date:    input: any date record
 * returns: a pointer to a static character string of at most TIME_SLEN
 *          characters length containing the ASCII text representation
 *          of date (NOTICE: the next call to time_sprint will overwrite
 *          the static character string)
 */
char *time_sprint(time_Ts);

/*E*/

#ifdef __cplusplus
}}
#endif

#endif /* _TF_LIBTIME_H */

/* ----- END OF libtime.h ----- */
