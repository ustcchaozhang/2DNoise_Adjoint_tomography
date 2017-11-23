c this is <glq_verbose.inc>
c------------------------------------------------------------------------------
c
c 22/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c define verbosity levels for subroutines
c
c REVISIONS and CHANGES
c    22/12/97   V1.0   Thomas Forbriger
c    02/12/98   V1.1   introduced debug mode
c    24/01/99   V1.2   option to report parameter changes verb_changes
c
c==============================================================================
c
c what subroutines do
      logical verb_subaction
c results of subroutines
      logical verb_subresult
c input to subroutines
      logical verb_subinput
c strategy information within subroutines
      logical verb_substrategy
c higher level strategy information
      logical verb_medstrategy
c top level strategy information
      logical verb_topstrategy
c warnings anywhere a warning condition is noticed
      logical verb_allwarn
c should intermediate results in extra graphics window
      logical verb_graph
c file in out
      logical verb_io
c report changes of model parameters
      logical verb_changes
c model refrence ouput
      logical verb_model
c debug
      logical verb_debug
c 
      common /glq_verbose/verb_subaction, verb_subresult, verb_subinput,
     &                    verb_substrategy, verb_medstrategy,
     &                    verb_topstrategy, verb_allwarn, verb_io,
     &                    verb_graph, verb_debug, verb_changes,
     &                    verb_model
c
c ----- END OF glq_verbose.inc -----
