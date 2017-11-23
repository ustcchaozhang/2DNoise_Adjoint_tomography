/*! \file cfftwtest.c
 * \brief a small test program for fftw (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 * \author Thomas Forbriger
 * \date 12/09/2007
 * 
 * a small test program for fftw (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 12/09/2007   V1.0   Thomas Forbriger
 *
 *  Note:
 *  The library code is migrated to FFTW3. This program however was just meant
 *  to be a testing sandbox to develop the first library code. It is not used
 *  in production nor is it used to test the library. This code will not be
 *  migrated and becomes obsolete.
 * 
 * ============================================================================
 */
#define TF_CFFTWTEST_C_VERSION \
  "TF_CFFTWTEST_C   V1.0   "

#include<drfftw.h>
#include<stdio.h>
#include<math.h>

void fill(fftw_real* a, int n, int m)
{
  int k;
  double f1, f2;
  f1=3.141592653589/n;
  f2=f1*m*2;
  /* fill input array */
  for (k=0; k<n; ++k)
  {
    a[k]=sin(k*f1)*sin(k*f1)*sin(k*f2);
  }
}

/*
 * the code provided in the tutorial for rfftw_one produces segmentation
 * faults if used together with heap memory
 *
 * solution: always use drfftw.h together with double precision transforms!
 */

void process(int n, int m)
{
  // create arrays 
  fftw_real* in=malloc(n*sizeof(fftw_real));
  fftw_real* out=malloc(n*sizeof(fftw_real));
  fftw_real* power_spectrum=malloc((n/2+1)*sizeof(fftw_real));
  if (in == NULL) { printf("could not alloc in\n"); abort(); }
  if (out == NULL) { printf("could not alloc out\n"); abort(); }
  if (power_spectrum == NULL) 
  { printf("could not alloc power_spectrum\n"); abort(); }
  rfftw_plan p;
  int k;
  fill(in, n, m);
  p = rfftw_create_plan(n, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE);
  rfftw_one(p, in, out);
  power_spectrum[0] = out[0]*out[0];  // DC component 
  for (k = 1; k < (n+1)/2; ++k)  // (k < N/2 rounded up) 
       power_spectrum[k] = out[k]*out[k] + out[n-k]*out[n-k];
  if (n % 2 == 0) // N is even 
       power_spectrum[n/2] = out[n/2]*out[n/2];  // Nyquist freq. 
  rfftw_destroy_plan(p);
  printf("\nPower spectrum of %d point signal with %d periods\n", n, m);
  printf("\ni.e. signal frequency %f\n", (double)m/n);
  for (k=0; k<((n+1)/2); ++k)
  {
    printf("  %3.3d, %5f: %10e\n",k, (double)k/n, power_spectrum[k]);
  }
  free(in);
  free(out);
  free(power_spectrum);
}


#define N 2048
int main()
{
  printf("%s\n", TF_CFFTWTEST_C_VERSION);

/*
 * the code provided in the tutorial for rfftw_one produces segmentation
 * faults if used together with stack memory for N=2000 or larger
 *
 * segfaults can be prevented if in and out array dimensions are set to the
 * double size that is required
 *
 * solution: always use drfftw.h together with double precision transforms!
 */

  fftw_real in[N], out[N], power_spectrum[N/2+1];
  rfftw_plan p;
  int k;
  int m;
  m=5;
  fill(in, N, m);
  p = rfftw_create_plan(N, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE);
  rfftw_one(p, in, out);
  power_spectrum[0] = out[0]*out[0];  // DC component 
  for (k = 1; k < (N+1)/2; ++k)  // (k < N/2 rounded up) 
       power_spectrum[k] = out[k]*out[k] + out[N-k]*out[N-k];
  if (N % 2 == 0) // N is even 
       power_spectrum[N/2] = out[N/2]*out[N/2];  // Nyquist freq. 
  rfftw_destroy_plan(p);
  printf("\nPower spectrum of %d point signal with %d periods\n", N, m);
  printf("\ni.e. signal frequency %f\n", (double)m/N);
  for (k=0; k<((N+1)/2); ++k)
  {
    printf("  %3.3d, %5f: %10e\n",k, (double)k/N, power_spectrum[k]);
  }
  
  fill(in, N, m);
  p = rfftw_create_plan(N, FFTW_REAL_TO_COMPLEX, FFTW_ESTIMATE);
  rfftw_one(p, in, out);
  power_spectrum[0] = out[0]*out[0];  // DC component 
  for (k = 1; k < (N+1)/2; ++k)  // (k < N/2 rounded up) 
       power_spectrum[k] = out[k]*out[k] + out[N-k]*out[N-k];
  if (N % 2 == 0) // N is even 
       power_spectrum[N/2] = out[N/2]*out[N/2];  // Nyquist freq. 
  rfftw_destroy_plan(p);
  printf("\nPower spectrum of %d point signal with %d periods\n", N, m);
  printf("\ni.e. signal frequency %f\n", (double)m/N);
  for (k=0; k<((N+1)/2); ++k)
  {
    printf("  %3.3d, %5f: %10e\n",k, (double)k/N, power_spectrum[k]);
  }
  
  process(200, 20);
  process(50000, 2000);
}

/* ----- END OF cfftwtest.c ----- */
