c this is <glq_data.inc>
c------------------------------------------------------------------------------
cS
c
c 28/11/97 by Thomas Forbriger (IfG Stuttgart)
c
c all we need to store and define datasets
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    28/01/98   V1.1   promoted travel time data to double precision
c    25/03/98   V1.2   added array to keep transfer function calculated
c                      by dat_fcamp
c    20/08/98   V1.3   new parameter data_maxamp
c    04/03/99   V1.4   allow splitting of travel time calculation
c
c==============================================================================
c
c number of data samples in use
      integer data_nslo, data_nfre, data_ntts
c
c greens matrix and travel time samples
c slowness is measured in s/km
c frequency will be given in Hz
      real read_dat_slo(glqd_mslo)
      real read_dat_om(glqd_mfre)
      double precision dat_slo(glqd_mslo)
      double precision dat_fre(glqd_mfre)
c angular frequency
      double precision dat_om(glqd_mfre)
      double complex green(glqd_mslo, glqd_mfre, glqd_mdat)
c just for reading purspose
      complex readgreen(glqd_mslo, glqd_mfre)
c offsets are measured in km
c travel times are measured in seconds
      double precision travx(glqd_mtts)
      double precision travt(glqd_mtts, glqd_mdat)
c 
c index where to split travel time sections
      integer data_ttsplit
c 
c weight to greens matrix an travel time samples
c r*weight are the read values (or preset values)
c *weight are the values to be calculated with (contains mormalizing factor)
c
c all read weights are meant to be not squared values
c all runtime weights are meant to be squared values
      real rgweight(glqd_mslo, glqd_mfre)
      real rtweight(glqd_mtts)
      real gweight(glqd_mslo, glqd_mfre)
      real tweight(glqd_mtts)
c 
c the transfer function calculated by dat_fcamp will be stored here
      complex dat_response(glqd_mfre)
c 
c number of anonymous data points used
      integer dat_n
c 
c maximum amplitude of input green data
      double precision data_maxamp
c
      common /glq_data/green, readgreen, dat_response,
     &                 travx, travt, 
     &                 gweight, tweight, rgweight, rtweight,
     &                 dat_slo, dat_fre, dat_om,
     &                 data_nslo, data_nfre, data_ntts,
     &                 dat_n, read_dat_slo, read_dat_om, data_maxamp,
     &                 data_ttsplit
c
cE
c ----- END OF glq_data.inc -----
