#!/bin/bash
# this is <checklibs.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
# 
# check whether library packages required by Seitosh are installed
# The script is designed for openSuSE installations, using the rpm package
# manager
#
# ----
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. 
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# ----
# 
# REVISIONS and CHANGES 
#    20/12/2010   V1.0   Thomas Forbriger
# 
# ============================================================================
# location of rpm package manager
RPMCMD=/bin/rpm
# ============================================================================
#
# check a single package
#
# parameters:
# $1: comment describing the packages to be checked
# $2: regex pattern to identify package(s)
checkpkg()
{
  echo 
  echo checking "$1"
  ${RPMCMD} -qa | egrep "$2" | sed -e 's/^/  /'
}

# ============================================================================
#
# report a reference installation on which Seitosh compiles successfully
#
refout()
{
  echo 
  echo "Output on my system (as a reference)"
  echo "------------------------------------"
  echo
  cat << HERE
This system is:
Linux gpilap26.physik.uni-karlsruhe.de 3.16.7-21-desktop #1 SMP PREEMPT Tue Apr 14 07:11:37 UTC 2015 (93c1539) x86_64 x86_64 x86_64 GNU/Linux
LSB Version:	n/a
Distributor ID:	openSUSE project
Description:	openSUSE 13.2 (Harlequin) (x86_64)
Release:	13.2
Codename:	Harlequin
The installed SuSE release is:
openSUSE 13.2 (x86_64)
VERSION = 13.2
CODENAME = Harlequin
# /etc/SuSE-release is deprecated and will be removed in the future, use /etc/os-release instead

checking the BOOST libboost regex
  libboost_regex1_54_0-1.54.0-10.1.3.x86_64
  libboost_regex1_54_0-32bit-1.54.0-10.1.3.x86_64

checking the BOOST libboost development package
  boost-devel-1.54.0-10.1.3.x86_64

checking the LAPACK library
  lapack-devel-static-3.5.0-68.1.x86_64
  lapack-man-3.5.0-68.1.noarch
  liblapack3-3.5.0-4.1.3.x86_64
  liblapack3-32bit-3.5.0-68.1.x86_64
  lapack-devel-32bit-3.5.0-68.1.x86_64
  lapack-devel-3.5.0-68.1.x86_64

checking the BLAS library
  blas-devel-3.5.0-68.1.x86_64
  blas-devel-static-3.5.0-68.1.x86_64
  blas-man-3.5.0-68.1.noarch
  libblas3-3.5.0-4.1.3.x86_64
  libblas3-32bit-3.5.0-68.1.x86_64
  blas-devel-32bit-3.5.0-68.1.x86_64

checking the GSL library
  gsl-1.16-5.1.3.x86_64

checking the GSL library development package
  gsl-devel-1.16-5.1.3.x86_64

checking the FFTW3 library
  libfftw3-3-3.3.4-5.11.x86_64
  libfftw3-3-32bit-3.3.4-5.11.x86_64

checking the FFTW3 library development package
  fftw3-devel-3.3.4-5.11.x86_64

checking the GNU C compiler
  gcc48-32bit-4.8.3+r212056-2.2.4.x86_64
  gcc48-4.8.3+r212056-2.2.4.x86_64

checking the GNU C++ compiler
  gcc48-c++-4.8.3+r212056-2.2.4.x86_64

checking the GNU Fortran compiler
  gcc48-fortran-4.8.3+r212056-2.2.4.x86_64
  gcc48-fortran-32bit-4.8.3+r212056-2.2.4.x86_64

checking the GNU Fortran library
  libgfortran3-4.8.3+r212056-2.2.4.x86_64
  libgfortran3-32bit-4.8.3+r212056-2.2.4.x86_64

checking the GNU stdc++ library development package
  libstdc++48-devel-4.8.3+r212056-2.2.4.x86_64
  libstdc++-devel-4.8-7.1.2.x86_64
  libstdc++48-devel-32bit-4.8.3+r212056-2.2.4.x86_64

checking the f2c compiler
  f2c-32bit-0.11-1217.1.2.x86_64
  f2c-0.11-1217.1.2.x86_64

checking the GNU make utility
  makedepend-1.0.5-4.1.2.x86_64
  makeinfo-4.13a-38.1.2.x86_64
  make-4.0-2.2.3.x86_64

checking the doxygen documentation generator
  doxygen-1.8.8-1.2.x86_64
HERE
}
# ============================================================================
echo This script checks whether library packages required by Seitosh
echo source code are installed. The script is designed for openSuSE
echo installations using the rpm package manager. 
echo
echo
refout
echo
echo ------------------------------------------------------------------
echo Checking your system:
test -x ${RPMCMD} || { echo package manager ${RPMCMD} is missing; exit 2; }
echo
echo This system is:
uname -a
if test -x /usr/bin/lsb_release
then
  /usr/bin/lsb_release -a
else
  echo /usr/bin/lsb_release is missing
fi
if test -r /etc/SuSE-release
then
  echo The installed SuSE release is:
  cat /etc/SuSE-release
else
  echo ERROR: This does not appear to be an openSuSE system
  echo "       File /etc/SuSE-release is missing"
fi

checkpkg "the BOOST libboost regex" "boost.*regex"
checkpkg "the BOOST libboost development package" "boost.*devel"
checkpkg "the LAPACK library" "lapack"
checkpkg "the BLAS library" "(blas-)|(libblas)"
checkpkg "the GSL library" "gsl-[[:digit:]]"
checkpkg "the GSL library development package" "gsl-devel"
checkpkg "the FFTW3 library" "fftw3-[[:digit:]]"
checkpkg "the FFTW3 library development package" "fftw3-devel"
checkpkg "the GNU C compiler" "^gcc..-[[:digit:]]"
checkpkg "the GNU C++ compiler" "^gcc..-c++"
checkpkg "the GNU Fortran compiler" "^gcc..-fortran"
checkpkg "the GNU Fortran library" "libgfortran"
checkpkg "the GNU stdc++ library development package" "libstdc++.*devel"
checkpkg "the f2c compiler" "f2c"
checkpkg "the GNU make utility" "^make"
checkpkg "the doxygen documentation generator" "doxygen"

# ----- END OF checklibs.sh ----- 
