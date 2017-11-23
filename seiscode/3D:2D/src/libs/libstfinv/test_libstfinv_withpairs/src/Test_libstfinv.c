/*------------------------------------------------------------------------
 *  Test_libstfinv.c
 *
 *  Copyright (c) 2011 by Lisa Rehor (KIT Karlsruhe) and Martin Schaefer (KIT Karlsruhe)            
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
 *  ---------------------------------------------------------------------*/


#include "fd.h"
#include "segy.h" 
#include "globvar.h"
#include "stfinv/stfinv.h"




int main(int argc, char **argv){ 

/* declaration of variables */
int ns, ns_test, ns_add, tracl1, j, k;
float DT, DT_test, DT_add;
float **sectionvy_obs=NULL, **sectionvy_syn=NULL, **sectionvy_conv=NULL;
float *source_time_function=NULL,*add=NULL,*add_conv=NULL;
float dump;
FILE *fpdata;
segy *tr_obs, *tr_syn, *tr_add;

char *fileinp;
FILE *FP;

/* --------------- read parameters from parameter-file (stdin) --------------- */
fileinp=argv[1];
FP=fopen(fileinp,"r");
read_parameters(FP);


printf("\n\n----------- Test for libstfinv -----------\n\n");
printf("Parameters read from input file:\n");
printf("FILE_OBS\t: %s\n",FILE_OBS);
printf("FILE_SYN\t: %s\n",FILE_SYN);
printf("ntr\t\t: %i\n",ntr);
printf("FILE_CONV\t: %s\n",FILE_CONV);
printf("FILE_STF\t: %s\n",FILE_STF);
printf("stfinv_par\t: %s\n",stfinv_par);
printf("FILE_ADD\t: %s\n",FILE_ADD);
printf("FILE_ADD_CONV\t: %s\n",FILE_ADD_CONV);

/* --------------- allocation of memory for structs with traces -------------- */
tr_obs=(segy *)malloc(ntr*sizeof(segy));
tr_syn=(segy *)malloc(ntr*sizeof(segy));
tr_add=(segy *)malloc(1*sizeof(segy));

/* --------------- reading in the observed data from an su file --------------- */
fpdata = fopen(FILE_OBS,"r");

fread(&tr_obs[0],240,1,fpdata);	/* reading header of first trace */
ns=(int)tr_obs[0].ns;
DT=(float)tr_obs[0].dt*1e-06;
printf("number of samples: %d\n",ns);
printf("sampling interval in s: %f\n",DT);

fread(&tr_obs[0].data,4,ns,fpdata);	/* reading data of first trace */

for(tracl1=1;tracl1<ntr;tracl1++){        
	fread(&tr_obs[tracl1],(240+ns*4),1,fpdata);
}
fclose(fpdata);



/* --------------- reading in the forward modelled data from an su file --------------- */
fpdata = fopen(FILE_SYN,"r");

fread(&tr_syn[0],240,1,fpdata);	/* reading header of first trace */
ns_test=(int)tr_syn[0].ns;
DT_test=(float)tr_syn[0].dt*1e-06;
if (ns!=ns_test){
	printf("Different number of samples.");
	exit(1);}
if (DT!=DT_test){
	printf("Different sampling intervals.");
	exit(1);}
	
fread(&tr_syn[0].data,4,ns,fpdata);	/* reading data of first trace */

for(tracl1=1;tracl1<ntr;tracl1++){        
	fread(&tr_syn[tracl1],(240+ns*4),1,fpdata);
}
fclose(fpdata);


/* --------------- reading in the additional time series from an su file --------------- */
fpdata = fopen(FILE_ADD,"r");

fread(&tr_add[0],240,1,fpdata);	/* reading header of first trace */
ns_add=(int)tr_add[0].ns;
DT_add=(float)tr_add[0].dt*1e-06;
if (ns!=ns_add){
	printf("Different number of samples.");
	exit(1);}
if (DT!=DT_add){
	printf("Different sampling intervals.");
	exit(1);}
	
fread(&tr_add[0].data,4,ns,fpdata);	/* reading data of first trace */

for(tracl1=1;tracl1<ntr;tracl1++){        
	fread(&tr_add[tracl1],(240+ns*4),1,fpdata);
}
fclose(fpdata);



/* --------------- allocation of memory and storing of seismograms in matrices --------------- */
/* allocation of matrices where data are stored similar to 
the data storage in DENISE */

sectionvy_conv=(float**)malloc(ntr*sizeof(float*));
for (k=0;k<ntr;k++)  sectionvy_conv[k]= (float*)malloc(ns*sizeof(float));
for (k=0;k<ntr;k++){
	for (j=0;j<ns;j++){
		sectionvy_conv[k][j]=0.0;
	}
}


sectionvy_obs=(float**)malloc(ntr*sizeof(float*));
for (k=0;k<ntr;k++)  sectionvy_obs[k]= (float*)malloc(ns*sizeof(float));
for (k=0;k<ntr;k++){
	for (j=0;j<ns;j++){
		sectionvy_obs[k][j]=0.0;
	}
}


sectionvy_syn=(float**)malloc(ntr*sizeof(float*));
for (k=0;k<ntr;k++)  sectionvy_syn[k]= (float*)malloc(ns*sizeof(float));
for (k=0;k<ntr;k++){
	for (j=0;j<ns;j++){
		sectionvy_syn[k][j]=0.0;
	}
}


source_time_function=(float*)malloc((ns*sizeof(float)));
for (k=0;k<ns;k++) source_time_function[k]=0.0;


add=(float*)malloc((ns*sizeof(float)));
for (k=0;k<ns;k++) add[k]=0.0;

add_conv=(float*)malloc((ns*sizeof(float)));
for (k=0;k<ns;k++) add_conv[k]=0.0;


/* storing the traces itself in a matrix */
for(tracl1=0;tracl1<ntr;tracl1++){
	for(j=0;j<ns;j++){
		dump=tr_obs[tracl1].data[j];
		sectionvy_obs[tracl1][j]=dump;
	}
}


/* storing the traces itself in a matrix */
for(tracl1=0;tracl1<ntr;tracl1++){
	for(j=0;j<ns;j++){
		dump=tr_syn[tracl1].data[j];
		sectionvy_syn[tracl1][j]=dump;
	}
}	

/* storing the traces itself in a vector */
for(j=0;j<ns;j++){
	dump=tr_add[0].data[j];
	add[j]=dump;	
}	


/* --------------- declaration of variables for libstfinv --------------- */
unsigned int nrec, nsamp, npairs;
float dt;

nrec=(unsigned int)ntr;
nsamp=(unsigned int)ns;
dt=DT;
npairs=1;

struct CTriples data;
data.n=nrec;
data.triples=(struct CWaveformTriple *)malloc(nrec*sizeof(struct CWaveformTriple));
if (data.triples == NULL) {abort();}

unsigned int i;
for (i=0;i<nrec;i++){
	data.triples[i].data=&sectionvy_obs[i][0];
	data.triples[i].synthetics=&sectionvy_syn[i][0];
	data.triples[i].convolvedsynthetics=&sectionvy_conv[i][0];
	
	data.triples[i].header.sx=tr_syn[i].sx;
	data.triples[i].header.sy=0.0;
	data.triples[i].header.sz=tr_syn[i].sdepth;
	data.triples[i].header.rx=tr_syn[i].gx;
	data.triples[i].header.ry=0.0;
	data.triples[i].header.rz=tr_syn[i].gelev;
	data.triples[i].header.sampling.n=nsamp;
	data.triples[i].header.sampling.dt=dt;
}



struct CWaveform stf;
stf.series = &source_time_function[0];
stf.sampling.n=nsamp;
stf.sampling.dt=dt;

struct CPairs pairs;
pairs.n=npairs;
pairs.pairs=(struct CWaveformPair *)malloc(npairs*sizeof(struct CWaveformPair));
if (pairs.pairs == NULL) { abort(); }

for (i=0; i<npairs; ++i){
	  
	pairs.pairs[i].synthetics=&add[0];
	pairs.pairs[i].convolvedsynthetics=&add_conv[0];
	
	pairs.pairs[i].sampling.n=nsamp;
	pairs.pairs[i].sampling.dt=dt;
}

initstfinvenginewithpairs(data, stf, pairs, stfinv_par);

runstfinvengine();


/* --------------- writing out the seismograms --------------- */
fpdata=fopen(FILE_CONV,"w");
for (tracl1=0;tracl1<ntr;tracl1++){
	fwrite(&tr_syn[tracl1],240,1,fpdata);
	for (i=0;i<ns;i++){
		fwrite(&sectionvy_conv[tracl1][i],4,1,fpdata);}
}
fclose(fpdata);	


/* --------------- writing out the source time function --------------- */
fpdata=fopen(FILE_STF,"w");
fwrite(&tr_syn[0],240,1,fpdata);
for (i=0;i<ns;i++){
	fwrite(&source_time_function[i],4,1,fpdata);}
fclose(fpdata);	

/* --------------- writing out the convolved additional time series --------------- */
fpdata=fopen(FILE_ADD_CONV,"w");
fwrite(&tr_add[0],240,1,fpdata);
for (i=0;i<ns;i++){
	fwrite(&add_conv[i],4,1,fpdata);}
fclose(fpdata);


/* --------------- deallocation of memory --------------- */
freestfinvengine();
free(data.triples);
free(tr_obs);
free(tr_syn);
free(tr_add);


for (i=0;i<ntr;i++){
	free(sectionvy_conv[i]);}
free(sectionvy_conv);

for (i=0;i<ntr;i++){
	free(sectionvy_obs[i]);}
free(sectionvy_obs);

for (i=0;i<ntr;i++){
	free(sectionvy_syn[i]);}
free(sectionvy_syn);


free(source_time_function);
free(add);
free(add_conv);



return 0;
}
