c this is <glq_model.inc>
c------------------------------------------------------------------------------
cS
c
c 28/11/97 by Thomas Forbriger (IfG Stuttgart)
c
c this file contains all the stuff to define and store models
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    29/01/98   V1.1   remember from which model the discrete one comes
c    13/01/99   V1.2   there are major changes in the meaning of model
c                      parameters - see detailed description below
c    04/03/99   V1.3   glqm_lasas for asphalt handling
c
c----------------------------------------------------------------------
c 
c 13/01/1999 - changes in model definition:
c   - all polynomials refer now to the middle of the section (rather than
c     the top of the section):
c     p(z)=p1+p2(z-zr)+p3*(z-zr)**2 where zr is now middle of section
c     routines to be changed are:
c       mod_chop        done
c       mod_write       done
c       mod_follow      done
c       mod_read        done
c       pgs_par         done
c       dat_dctt        done
c   - the section depths now mean bottom of section rather than top of
c     section - for this reason the chop_hs (top of halfspace) parameter
c     may now be omitted.
c     routines to be changed are:
c       dat_dctt        done
c       mod_chop        done
c       mod_clear       is not affected
c       mod_copy        is not affected
c       mod_follow      done
c       mod_isec        done (is just used by splimo.f)
c       mod_parcor      done
c       mod_pcheck      is not affected
c       mod_read        done
c       mod_write       done
c       par_sano        done
c       pgs_par         done
c     routines to be changed because of chop_hs:
c       par_showmod     done
c       par_read        done
c       par_set         done
c       par_chop        done (argument list changed! - is just used by mop.f
c                             and pmotra.f)
c     chop_hs will be canceled in glq_para.inc
c   - further we have to change to meaning of glqm_follow:
c     glqm_follow means now that the mean value (polynomial 
c     coefficient 1) will now follow the mean value of the overlying
c     layer.
c     this has implications on: 
c       dat_dctt.f            done
c       mod_chop.f            done
c       mod_clear.f           is not affected
c       mod_follow.f          done
c       mod_read.f            done
c       mod_showparcor.f      done
c       mod_write.f           is not affected by follow flag
c       par_pardisp.f         done
c       par_wwrite.f          is not affected
c       pgs_par.f             done
c       mod_parcor.f          done
c       mod_weight.f          done
c       mod_identify.f        done
c       par_parcor.f          done
c       par_wcheck.f          done
c       par_wenc.f            done
c       par_wdec.f            done
c       par_wread.f           done
c       res_part.f            done
c       par_sano.f            done
c
c----------------------------------------------------------------------
c 
c number of model sections in use
      integer glqm_nsec
c
c number of polynomial coefficients in use per section and parameter
      integer glqm_npol(glqm_msec, glqm_mpar)
c 
c switch to make a section follow its lower neighbour 
      logical glqm_follow(glqm_msec, glqm_mpar)
c
c named model (and section depth)
c section depth will be given in meters (not km!)
      double precision model(glqm_mpol, glqm_msec, glqm_mpar, glqm_mmod)
      double precision mdepth(glqm_msec, glqm_mmod)
c 
c define named parameters to be estimated (depth and others)
      logical destim(glqm_msec)
      logical mestim(glqm_msec, glqm_mpar)
c 
c number of anonymous model parameters in use
      integer mod_n
c 
c anonymous model
      double precision mdelta(glqm_mano)
c 
c weight to anonymous model (are search ranges now!)
      double precision mweight(glqm_mano)
c 
c for travel time synthetics we need to hold the discrete model version
      integer glqm_nlay
c discrete depth are given in km
c depth gives top of layer
c first layer is top halfspace with depth set to zero
c second index is first layer with top depth zero
      double precision dmodel(glqm_mlay, glqm_cpar)
c parent of discrete model
      integer glqm_parent
c
c index of last layer in asphalt section
      integer glqm_lasas
c 
c common block
      common /glq_model/mdelta, mweight, model, mdepth, dmodel,
     &                  destim, mestim, glqm_npol, glqm_follow,
     &                  mod_n, glqm_nsec, glqm_nlay, glqm_parent,
     &                  glqm_lasas
c
cE
c ----- END OF glq_model.inc -----
