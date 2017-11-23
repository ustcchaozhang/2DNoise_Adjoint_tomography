/* this is <sffutil.c>
 *
 * for manipulating sff-files in c
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
 * NOTICE: These functions rely on linking conventions of the Fortran
 * compiler. They are designed to be used together with f2c such that all
 * source code finally may be compiled and linked by the C compiler. Beware
 * that Fortran I/O differs from C I/O. gfortran for the compilation of the
 * main program must be called appropriately (-ff2c).
 * 
 * If C++ is an alternative for you, you should consider libsffxx which
 * consistently is written in C++ and does not require linking against
 * Fortran.
 *
 * Revisions and Changes:
 *   30/04/97   corrected subroutine write_SRCE to write only
 *              least significant two digits of year
 *   30/09/97   adding a new subroutine for easy creation of free blocks
 *   09/10/97   changed from timeutil to libtime
 *   27/10/97   - changed sff_freecfree to set NULL pointer after free
 *              - increased most character array by one character
 *   16/11/99   added float -> int conversion
 *   06/12/07   some corrections to decalarations to satisfy my compiler
 *   18/09/2010 compiler issues warning since there exits not true prototypes
 *              for libsff.a routines
 *   14/11/2010 appropriate Fortran variable types are provided by libtime.h
 *              include sff.h
 *   15/11/2010 do no longer use tfmacros.h
 *   16/02/2012 resolved type issues with timeint and long int parameters
 */

/* libtime.h also provides Fortran variable types like integer, real, etc */
#include <libtime.h>
#include <sffutil.h>
#include <sff.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define SFFU_EXIT_FAILURE 1
#define SFFU_CHECKERROR( EXPR , SUB, STR )\
    if ( EXPR ) { fprintf(stderr, "ERROR (%s):\n   %s\n", SUB, STR );\
        exit(SFFU_EXIT_FAILURE); }

/* 
 * convert C-type string to fortran
 * ================================
 */
void sff_cstring_fortran(string, n)
char *string;
long int n;
{
  int i, FoundEnd;
  FoundEnd=0;
  for (i=0; i<n; i++) {
    if (FoundEnd==0) {
      if (string[i]=='\0') {
        FoundEnd=1;
        string[i]=' ';
      }
    } else {
      string[i]=' ';
    }
  }
} /* sff_cstring_fortran */

/*
 * delete sff file 
 * ================
 */
void sff_New(filename)
char *filename;
{
/* Fortran */
  static integer ierr, lu;
  static char name[201];
  extern int sff_new__();

  strncpy(name, filename, 200);
  lu=20;
  sff_new__(&lu, name, &ierr, 200L);

  SFFU_CHECKERROR((ierr!=0), "sff_New", "deleting file")
} /* sff_New */

/*
 * open sff file 
 * =============
 */
void sff_WOpen(lu, filename)
integer lu;
char *filename;
{
/* Fortran */
  static integer ierr;
  static char name[201];
  extern int sff_filewopen__();

  strncpy(name, filename, 200);
  lu=20;
  sff_filewopen__(&lu, name, &ierr, 200L);

  SFFU_CHECKERROR((ierr!=0), "sff_WOpen", "opening file")
} /* sff_WOpen */

/*
 * write status line
 * =================
 */
void sff_WStatus(lu, code)
integer lu;
char *code;
{
/* Fortran */
  static char fcode[11];
  extern int sff_wstatus__();

  strncpy(fcode, code, 10);
  sff_cstring_fortran(fcode, 10);

  sff_wstatus__(&lu, fcode, 10L);
} /* sff_WStatus */

/*
 * write FREE block
 * ================
 */
void sff_WFree(lu, freeblock)
integer lu;
sff_Tfreeblock freeblock;
{
  int l;
/* Fortran */
  static char line[SFF_LENFREE+1];
  extern int sff_writeline__();

  strcpy(line, "FREE");
  sff_cstring_fortran(line, SFF_LENFREE); 
  sff_writeline__(&lu, line, SFF_LENFREE);

  for (l=0; l<freeblock.n; l++) {
    strncpy(line, freeblock.line[l], SFF_LENFREE);
    sff_cstring_fortran(line, SFF_LENFREE);
    sff_writeline__(&lu, line, SFF_LENFREE);
  }

  strcpy(line, "FREE");
  sff_cstring_fortran(line, SFF_LENFREE); 
  sff_writeline__(&lu, line, SFF_LENFREE);
} /* sff_WFree */

/*
 * write INFO line 
 * ===============
 */
void sff_WInfo(lu, info)
integer lu;
sff_Tinfo info;
{
/* Fortran */
  static char cs[1];
  static real c1, c2, c3;
  static integer stacks;
  extern int sff_winfo__();

  c1=(real)info.c1;
  c2=(real)info.c2;
  c3=(real)info.c3;
  cs[0]=(real)info.cs;
  stacks=(integer)info.stacks;
  sff_winfo__(&lu, cs, &c1, &c2, &c3, &stacks, 1L); 
} /* sff_WInfo */

/*
 * write SRCE line 
 * ===============
 */
void sff_WSource(lu, source)
integer lu;
sff_Tsource source;
{
  timeint year, month, day, milsec;
/* Fortran */
  static char typh[21];
  static char cs[1];
  static real c1, c2, c3;
  static char date[7], time[11];
  extern int sff_wsource__();

  c1=source.c1;
  c2=source.c2;
  c3=source.c3;
  cs[0]=source.cs;
  year=source.date.year;
  year=year-(100*((long int)(year/100)));
  strncpy(typh, source.type, 20);
  time_getdate(&day, &month, source.date);
  sprintf(date, "%2.2ld%2.2ld%2.2ld", (long int)(year), 
          (long int)(month), (long int)(day));
  milsec=source.date.milsec;
  if (source.date.micsec>=500) milsec=milsec+1; 
  sprintf(time, "%2.2ld%2.2ld%6.3f", 
    (long int)(source.date.hour), (long int)(source.date.minute), 
    source.date.second+1.e-3*milsec);

  sff_cstring_fortran(date, 6);
  sff_cstring_fortran(time, 10);
  sff_cstring_fortran(typh, 20);

  sff_wsource__(&lu, typh, cs, &c1, &c2, &c3, date, time, 20L, 1L, 6L, 10L); 
} /* sff_WSource */

/*
 * create WID2 line string
 * =======================
 */
void sff_PrepWid2(wid2)
sff_Twid2line *wid2;
{
  char extra[21];
  timeint ismonth, isday;
/* Fortran */
  static char station[11], comp[11], auxid[11], instyp[11];
  static integer nsamp, year, month, day, hour, minute, ierr;
  static real samprat, second, calib, calper, hang, vang;
  extern int sff_prepwid2__();

  time_getdate(&isday, &ismonth, wid2->date);
  nsamp   =wid2->nsamp;
  samprat =1./wid2->interval;
  year    =wid2->date.year;
  month   =ismonth;
  day     =isday;
  minute  =wid2->date.minute;
  hour    =wid2->date.hour;
  second  =wid2->date.second+1.e-3*wid2->date.milsec+1.e-6*wid2->date.micsec;
  hang    =wid2->hang;
  vang    =wid2->vang;
  calib   =wid2->calib;
  calper  =wid2->calper;
  strncpy(station, wid2->station, 10);
  strncpy(comp, wid2->channel, 10);
  strncpy(instyp, wid2->instype, 10);
  strncpy(auxid, wid2->auxid, 10);

  sff_cstring_fortran(station, 10);
  sff_cstring_fortran(comp, 10);
  sff_cstring_fortran(instyp, 10);
  sff_cstring_fortran(auxid, 10);
  
  sff_prepwid2__(&nsamp, &samprat, station, &year, &month, &day, &hour, 
    &minute, comp, auxid, instyp, &second, &calib, &calper, &hang, 
    &vang, wid2->wid2, &ierr, 10L, 10L, 10L, 10L, 132L);

  /* now check values exceeding GSE2.0 specification */
  if (samprat>=10000.) {
    fprintf(stderr, 
      "WARNING (%s):\n  leaving GSE2.0 format specification for samprat %f\n",
      "sff_PrepWid2", samprat);
    sprintf(extra, "%11.5e", samprat);
    sff_cstring_fortran(extra, 20);
    strncpy(wid2->wid2+57, extra, 11);
  }
} /* sff_PrepWid2 */

/*
 * write sff trace
 * ===============
 */
void sff_WData(lu, wid2line, idata, ampfac, code)
integer lu;
sff_Twid2line wid2line;
integer *idata;
double ampfac;
char *code;
{
/* Fortran */
  static integer nsamp;
  extern int sff_wdata__();
  static real fampfac;
  static char fcode[11];

  fampfac=(real)ampfac;
  nsamp=(integer)wid2line.nsamp;
  strncpy(fcode, code, 10);
  sff_cstring_fortran(fcode, 10);

  sff_PrepWid2(&wid2line);
  sff_wdata__(&lu, wid2line.wid2, &nsamp, idata, &fampfac, fcode, 132L, 10L);
} /* sff_WData */

/* 
 * close sff file 
 * ==============
 */
void sff_Close(lu)
integer lu;
{
/* Fortran */
  integer ierr;
  extern int sff_fileclose__();
  sff_fileclose__(&lu, &ierr);
  SFFU_CHECKERROR((ierr!=0), "sff_Close", "closing file")
} /* sff_Close */

/*
 * write a single line in a chained free block 
 * ===========================================
 */
void sff_wcfree(freeblock, newtext)
sff_Tfreeblock *freeblock;
char *newtext;
{
  sff_Tchainedfree *newline;
  sff_Tchainedfree *this;

  /* get memory for new chainline */
  newline=(sff_Tchainedfree *)malloc(sizeof(sff_Tchainedfree));
  SFFU_CHECKERROR((newline==NULL), "sff_wcfree", \
    "could not allocate memory")

  newline->next=NULL;
  strncpy(newline->text, newtext, SFF_LENFREE);
  newline->text[SFF_LENFREE]='\0';

  /* is the chain initialized? */
  if (freeblock->first==NULL) {
    freeblock->first=newline;
  } else {
    this=freeblock->first;
    while (this->next!=NULL) {
      this=(sff_Tchainedfree *)this->next;
    }
    this->next=(void *)newline;
  }
} /* sff_wcfree */

/*
 * roll out chained FREE block
 * ===========================
 */
int sff_rollout(freeblock)
sff_Tfreeblock *freeblock;
{
  int n;
  sff_Tchainedfree *this;

  this=freeblock->first;
  n=0;
  while (this!=NULL) {
    this=(sff_Tchainedfree *)this->next;
    n++;
  }

  freeblock->line=(char **)malloc(sizeof(char *)*n);
  RETURNERROR((freeblock->line==NULL), "sff_rollout", \
    "could not allocate memory for blockfree", SFF_NOMEM)

  freeblock->n=n;
  freeblock->max=n;

  this=freeblock->first;
  n=0;
  while (this!=NULL) {
    freeblock->line[n]=this->text;
    this=(sff_Tchainedfree *)this->next;
    n++;
  }

  return(SFF_OK);
} /* sff_rollout */


/* 
 * free memory of chained FREE block
 * =================================
 */
void sff_freecfree(freeblock)
sff_Tfreeblock freeblock;
{
  sff_Tchainedfree *this;
  sff_Tchainedfree *next;

  this=freeblock.first;
  while (this!=NULL) {
    next=(sff_Tchainedfree *)this->next;
    free(this);
    this=next;
  }
  freeblock.first=NULL;
} /* sff_freecfree */

/*
 * convert float data to int data
 * ==============================
 */
void sff_f2i(data, n, ampfac)
sff_Tdata data;
int n;
double *ampfac;
{
  /*
   * data:   is used as pointer of type (float *) on input
   *                        and of type (int *) on output
   * n:      gives the number of samples to convert
   * ampfac: scaling factor for integer data (on output)
   *         multiply integer data with this factor to get
   *         original (float) values
   */

  int i;
  double maxval;

  maxval=0;
  for (i=0;i<n;i++) {
    maxval=maxval < abs(data.f[i]) ? maxval : abs(data.f[i]);
  }

  if (maxval == 0.) { maxval=SFF_AMPLIMIT; }

  *ampfac=maxval/SFF_AMPLIMIT;

  if (*ampfac == 0.) { *ampfac=1.; }

  for (i=0;i<n;i++) {
    data.i[i] = (int)(data.f[i]/(*ampfac)+0.5);
  }
}


/***** END OF sffutil.c *****/
