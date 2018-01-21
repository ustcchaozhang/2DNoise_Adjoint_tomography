#!/bin/bash
# this is <systemtype.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
# 
# report the type of system and installed compilers
# 
# REVISIONS and CHANGES 
#    24/10/2013   V1.0   Thomas Forbriger
#    15/07/2016   V1.1   add line break
# 
# ============================================================================
#
printf "Date: " 
LANG=C date 2>&1 
printf "User: ${USER}; full name: " 
egrep "^${USER}" /etc/passwd | cut -f 5 -d ':' 
printf "\nHost and kernel:\n" 
uname -a 2>&1 
printf "\nOperating system:\n" 
if ! type lsb_release &> /dev/null; \
then printf "The program lsb_release wasn't installed.\n"; \
  printf "No information provided.\n"; \
fi
lsb_release -a 2>&1 
printf "\nCompilers:\n" 
(printf "${CC}: "; ${CC} --version) 2>&1 | head -1 
(printf "${CXX}: "; ${CXX} --version) 2>&1 | head -1 
(printf "${CPP}: "; ${CPP} --version) 2>&1 | head -1 
(printf "${FC}: "; ${FC} --version) 2>&1 | head -1 
(printf "doxygen: "; doxygen --version) 2>&1 | head -1 
(printf "make: "; make --version) 2>&1 | head -1 
# ----- END OF systemtype.sh ----- 
