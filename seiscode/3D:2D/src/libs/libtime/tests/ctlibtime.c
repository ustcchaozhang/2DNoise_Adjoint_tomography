/* this is <ctlibtime.c>
 * ----------------------------------------------------------------------------
 *
 * 12/08/97 by Thomas Forbriger (IfG Stuttgart)
 *
 * C version of libtime test program
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
 *    13/12/07   V1.1   started migration to 64bit
 *    17/12/07   V1.2   migrated code works
 *
 * ============================================================================
 */

#include <libtime.h>
#include <stdio.h>
#include <string.h>

void subaddtest(date1,date2)
time_Ts date1;
time_Ts date2;
{
      time_Ts date3, date4;
      char string[TIME_SLEN];
      strcpy(string,time_sprint(date1));
      printf("***           date1: %s\n",string);
      strcpy(string,time_sprint(date2));
      printf("              date2: %s\n",string);
      time_sub(date1, date2, &date3);
      strcpy(string,time_sprint(date3));
      printf("  date1-date2=date3: %s\n",string);
      time_add(date3, date2, &date4);
      strcpy(string,time_sprint(date4));
      printf("  date3+date2=date4: %s\n",string);
} /* subaddtest */

void head(routine)
char *routine;
{
      printf("\nTEST %s\n\n",routine);
} /* head */
      
void subhead(routine)
char *routine;
{
      printf("\n**** %s\n\n",routine);
} /* subhead */
      
void arout(Date)
time_Ts Date;
{
      int i;
      time_Tu *u;
      u=(time_Tu *)&Date;
      printf("date-array ");
      for (i=0; i<7; i++) { printf("%6d ",u->array[i]); }
      printf("\n");
} /* arout */

void cmptest(date1, date2)
time_Ts date1;
time_Ts date2;
{
      char string1[TIME_SLEN];
      char string2[TIME_SLEN];
      strcpy(string1, time_sprint(date1));
      strcpy(string2, time_sprint(date2));
      printf("%s  ??  %s  :  %d\n",
        string1,string2,time_compare(date1, date2));
      printf("%s  ??  %s  :  %d\n",
        string2,string1,time_compare(date2, date1));
} /* cmptest */

void divmultest(date1, n, date2)
time_Ts date1;
long int n;
time_Ts date2;
{
      timeint nfit, rest;
      time_Ts date3, date4, full;
      char string1[TIME_SLEN];
      char string2[TIME_SLEN];
      char string3[TIME_SLEN];
      char string4[TIME_SLEN];
      char stringf[TIME_SLEN];
      printf("****\n");
      strcpy(string1,time_sprint(date1));
      strcpy(string2,time_sprint(date2));
      printf("date1 is %s\n",string1);
      printf("date2 is %s\n",string2);
      time_mul(date1, &date3, n);
      strcpy(string3,time_sprint(date3));
      printf("%s * %d -> %s\n",string1, n, string3);
      time_nfit(date3, date1, &nfit, &full);
      strcpy(stringf,time_sprint(full));
      printf("fitting: %d to %s leads to %s\n",nfit,string3,stringf);
      time_div(date3, &date4, n, &rest);
      strcpy(string4,time_sprint(date4));
      printf("%s / %d -> %s rest %d\n",string3,n,string4,rest);
      time_add(date3, date2, &date4);
      time_div(date4, &date3, n, &rest);
      strcpy(string4,time_sprint(date4));
      strcpy(string3,time_sprint(date3));
      printf("%s / %d -> %s rest %d\n",string4,n,string3,rest);
      time_nfit(date4, date1, &nfit, &full);
      strcpy(stringf,time_sprint(full));
      printf("fitting: %d to %s leads to %s\n",nfit,string4,stringf);
      time_add(date4, date1, &date3);
      strcpy(string3,time_sprint(date3));
      time_nfit(date3, date1, &nfit, &full);
      strcpy(stringf,time_sprint(full));
      printf("fitting: %d to %s leads to %s\n",nfit,string3,stringf);
} /* divmultest */

/*
 * Main program
 */

int main()
{
      time_Ts date1, date2;
      timeint day, month, year;

      date1.year=97;
      date1.doy=150;
      date1.hour=12;
      date1.minute=10;
      date1.second=9;
      date1.milsec=123;
      date1.micsec=456;

      head("time_libversion");
      printf("libversion: %f\n",time_libversion());

      head("time_isleapyear");
      printf("leap 2000 %d\n",time_isleapyear(2000));
      printf("leap 1996 %d\n",time_isleapyear(1996));
      printf("leap 1997 %d\n",time_isleapyear(1997));
      printf("leap   92 %d\n",time_isleapyear(  92));
      printf("leap    0 %d\n",time_isleapyear(   0));
      printf("leap 1900 %d\n",time_isleapyear(1900));

      head("time_fullyear");
      year=0;
      time_fullyear(&year);
      printf("year    0: %d\n",year);
      year=15;
      time_fullyear(&year);
      printf("year   15: %d\n",year);
      year=97;
      time_fullyear(&year);
      printf("year   97: %d\n",year);
      year=70;
      time_fullyear(&year);
      printf("year   70: %d\n",year);
      year=100;
      time_fullyear(&year);
      printf("year  100: %d\n",year);
      year=69;
      time_fullyear(&year);
      printf("year   69: %d\n",year);
      year=99 ;
      time_fullyear(&year);
      printf("year   99: %d\n",year);
      year=1831;
      time_fullyear(&year);
      printf("year 1831: %d\n",year);
      year=2061;
      time_fullyear(&year);
      printf("year 2061: %d\n",year);
      time_fullyear(&date1.year);
      printf("full year: %s\n", time_sprint(date1));
      date1.year=50;
      time_fullyear(&date1.year);
      printf("full year 50: %s\n", time_sprint(date1));

      head("time_sprint (time_getdate is implicit)");
      date1.year=97;
      date1.doy=150;
      date1.hour=12;
      date1.minute=10;
      date1.second=9;
      date1.milsec=123;
      date1.micsec=456;
      arout(date1);
      printf("date: %s\n", time_sprint(date1));
      date1.year=96;
      printf("other year: %s\n", time_sprint(date1));
      date1.doy=366;
      printf("last doy (set by doy): %s\n", time_sprint(date1));
      date1.year=0;
      date1.doy=12345;
      printf("relative time value: %s\n", time_sprint(date1));

      head("time_setdoy");
      date1.year=97;
      date1.doy=150;
      date1.hour=12;
      date1.minute=10;
      date1.second=9;
      date1.milsec=123;
      date1.micsec=456;
      time_setdoy(1, 1, &date1);
      printf("jan first: %s\n", time_sprint(date1));
      time_setdoy(31, 12, &date1);
      printf("dec last: %s\n", time_sprint(date1));

      head("time_clear");
      time_clear(&date1);
      arout(date1);
      printf("after clear: %s\n", time_sprint(date1));

      head("time_norm");
      date1.year=96;
      date1.doy=1;
      date1.hour=24;
      date1.minute=60;
      date1.second=60;
      date1.milsec=1000;
      date1.micsec=1000;
      arout(date1);
      time_norm(&date1);
      arout(date1);
      printf("after norm: %s\n", time_sprint(date1));
      date1.year=96;
      date1.doy=1;
      date1.hour=0;
      date1.minute=0;
      date1.second=0;
      date1.milsec=0;
      date1.micsec=-1;
      arout(date1);
      time_norm(&date1);
      arout(date1);
      printf("after norm: %s\n", time_sprint(date1));
      date1.year=0;
      date1.doy=5003;
      date1.hour=24;
      date1.minute=60;
      date1.second=60;
      date1.milsec=1000;
      date1.micsec=1000;
      arout(date1);
      time_norm(&date1);
      arout(date1);
      printf("after norm: %s\n", time_sprint(date1));
      date1.year=0;
      date1.doy=123;
      date1.hour=0;
      date1.minute=0;
      date1.second=0;
      date1.milsec=0;
      date1.micsec=-1;
      arout(date1);
      time_norm(&date1);
      arout(date1);
      printf("after norm: %s\n", time_sprint(date1));

      head("time_getdate after time_clear");
      time_clear(&date1);
      arout(date1);
      time_getdate(&day, &month, date1);
      printf("date after time_clear %d %d\n",day,month);

      head("time_add and time_sub");
      date1.year=1997;
      date1.doy=150;
      date1.hour=12;
      date1.minute=34;
      date1.second=56;
      date1.milsec=123;
      date1.micsec=456;

      subhead("absolute & absolute");
      time_copy(date1, &date2);
      date2.year=1999;
      subaddtest(date1, date2);
      subaddtest(date2, date1);
      time_copy(date1, &date2);
      date2.hour=11;
      date2.minute=33;
      date2.second=55;
      date2.milsec=122;
      date2.micsec=455;
      subaddtest(date2, date1);
      date2.micsec=127;
      subaddtest(date2, date1);

      subhead("absolute & relative");
      time_clear(&date2);
      date2.doy=12;
      date2.hour=40;
      date2.minute=40;
      date2.second=20;
      date2.milsec=20;
      date2.micsec=20;
      subaddtest(date2, date1);
      subaddtest(date1, date2);
      
      subhead("relative & relative");
      date1.year=0;
      date1.doy=1;
      date1.hour=11;
      date1.minute=12;
      date1.second=13;
      date1.milsec=14;
      date1.micsec=15;
      subaddtest(date2, date1);
      subaddtest(date1, date2);

      head("time_copy");
      date1.year=1997;
      date1.doy=150;
      date1.hour=12;
      date1.minute=34;
      date1.second=56;
      date1.milsec=123;
      date1.micsec=456;
      time_clear(&date2);
      printf("date1: %s\n", time_sprint(date1));
      printf("date2 after clear: %s\n", time_sprint(date2));
      time_copy(date1, &date2);
      printf("date2 after copy: %s\n", time_sprint(date2));

      head("time_finish");
      date1.year=57;
      date1.doy=450;
      date1.hour=12;
      date1.minute=0;
      date1.second=4781;
      date1.milsec=0;
      date1.micsec=45456;
      arout(date1);
      time_finish(&date1);
      arout(date1);

      head("time_compare");
      date1.year=1997;
      date1.doy=150;
      date1.hour=12;
      date1.minute=34;
      date1.second=56;
      date1.milsec=123;
      date1.micsec=456;
      time_copy(date1, &date2);
      cmptest(date1, date2);
      date1.doy=146;
      cmptest(date1, date2);
      date1.year=0;
      date1.doy=5667;
      time_copy(date1, &date2);
      date2.minute=12;
      date2.second=45;
      cmptest(date1, date2);

      head("time_mul and time_div and time_nfit");
      date1.year=0;
      date1.doy=0;
      date1.hour=0;
      date1.minute=0;
      date1.second=1;
      date1.milsec=2;
      date1.micsec=3;
      time_copy(date1, &date2);
      date1.second=0;
      date1.milsec=1;
      date1.micsec=500;
      divmultest(date1, 5, date2);
      divmultest(date1, 8567, date2);
      divmultest(date1, 2658567, date2);
} /* main */

 
/* ----- END OF ctlibtime.c ----- */
