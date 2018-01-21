/* this is <sffutil.h>
 *
 * Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
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
 * 23/04/97   Thomas Forbriger (IfG Stuttgart)
 * 30/09/97   adding a new subroutine for easy creation of free blocks
 * 09/10/97   changing from timeutil to libtime
 * 16/11/99   added float to int converter and sff_Tdata
 *            prototype for sff_WInfo was missing
 * 11/10/00   reading f2c first is mandatory with new libtime version
 * 14/11/10   appropriate Fortran variable types are provided by libtime.h
 *            provide prototypes for function in sffutilf.f
 *            provide typedef for Fortran type real
 *
 */

#ifndef _TF_SFFUTIL_H
#define _TF_SFFUTIL_H

/* libtime.h also provides Fortran variable types like integer, real, etc */
#include <libtime.h>
/* a typedef for real is not provided by libtime.h */
typedef float real;

/*======================================================================*/
/* prototypes for Fortran interface functions in sffutilf.f             */

extern int sff_filewopen__(integer *lu, char *filename, integer *ierr, ftnlen
                           filename_len);
extern int sff_fileclose__(integer *lu, integer *ierr);
extern int sff_writeline__(integer *funit, char *text, ftnlen text_len);

/*======================================================================*/

/* 
 * define macros
 * =============
 */

/* length of FREE block line */
#define SFF_LENFREE 80
/* length of WID2 line */
#define SFF_LENWID2 132

/* errors */
#define SFF_NOMEM 2
#define SFF_OK 0

/* maximum number of samples that could be handled by fixed fortran library */
#define SFF_MAXSAMPLES 100000

/* integer amplitude limit */
#define SFF_AMPLIMIT 0x800000
/*
 * define data structures
 * ======================
 */
typedef char sff_Tfreeline[SFF_LENFREE+1];

typedef struct {
  char wid2[SFF_LENWID2+1];
  time_Ts   date;            /* time and date of first sample */
  char      station[6];      /* station ID                    */
  char      channel[4];      /* channel ID                    */
  char      auxid[5];        /* auxiliary identification code */
  long int  nsamp;           /* number of samples             */
  double    interval;        /* sampling interval in seconds  */
  double    calib;           /* calibration factor            */
  double    calper;          /* calibration period            */
  char      instype[7];      /* instrument type               */
  double    hang;            /* horizontal orientation of sensor */
  double    vang;            /* vertical orientation of sensor */
} sff_Twid2line;

typedef struct {
  char      type[20];        /* type of source                 */
  char      cs;              /* coordinate system              */
  double    c1,c2,c3;        /* coordinates                    */
  time_Ts   date;            /* date of source                 */
} sff_Tsource;

typedef struct {
  void          *next;       /* next element in chain         */
  sff_Tfreeline text;        /* text of free block line       */
} sff_Tchainedfree;

typedef struct {
  sff_Tchainedfree *first;
  int n;
  int max;
  char **line;
} sff_Tfreeblock;

typedef struct {
  char      cs;              /* coordinate system              */
  double    c1,c2,c3;        /* receiver coordinates           */
  int       stacks;          /* number of stacks               */
} sff_Tinfo;

typedef union {
  float *f;
  integer *i;
  int *ci;
} sff_Tdata;

/*
 * prototypes
 * ==========
 */

/* convert strings */
void sff_cstring_fortran(char *, long int);

/* delete file */
void sff_New(char *);

/* open file */
void sff_WOpen(integer, char *);

/* write status */
void sff_WStatus(integer, char *);

/* write FREE block */
void sff_WFree(integer, sff_Tfreeblock);

/* write SRCE line */
void sff_WSource(integer, sff_Tsource);

/* write INFO line */
void sff_WInfo(integer lu, sff_Tinfo); 

/* create WID2 */
void sff_PrepWid2(sff_Twid2line *);

/* write data */
void sff_WData(integer, sff_Twid2line, integer *, double, char *);

/* close file */
void sff_Close(integer);

/* write to chained FREE block */
void sff_wcfree(sff_Tfreeblock *, char *);

/* roll out chained FREE block to array */
int sff_rollout(sff_Tfreeblock *);

/* free memory allocated by FREE block */
void sff_freecfree(sff_Tfreeblock);

/* convert float data to int data */
void sff_f2i(sff_Tdata, int, double *);

#endif /* _TF_SFFUTIL_H */

/***** END OF sffutil.h *****/
