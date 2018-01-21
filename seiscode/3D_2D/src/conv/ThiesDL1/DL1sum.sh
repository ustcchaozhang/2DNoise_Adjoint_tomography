#!/bin/bash
# this is <DL1sum.sh>
# ----------------------------------------------------------------------------
VERSION=2016-01-07
# 
# Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# calculate daily and monthly cumulative sums
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
#    09/05/2014   V1.0   Thomas Forbriger
#    07/01/2015          implement option -y|--year
#    07/01/2016          abort if no month is selected
# 
# ============================================================================
#
#
usage() {
cat >&2 << HERE
usage: DL1sum.sh  [--basepath|-b dir] [--tmpfile|-t path]
                  [--verbose|-v] [--year|-y year] month [month...]
   or: DL1sum.sh --help|-h
HERE
}
#
# ----------------------------------------------------------------------------
#
longusage() {
cat >&2 <<HERE

create table of cumulative sums of precipitation

  -v or --verbose        be verbose
  -y or --year year      set year to process to \"year\"
  -b or --basepath dir   set data base path to \"dir\"
  -t or --tmpfile path   set temporary file to \"path\"

  month     month to process

data base path: $BASEPATH
temporary file: $TMPFILE
default year:   $YEAR
HERE
}
#
# ----------------------------------------------------------------------------
#
# echo if verbose
# ---------------
vecho() {
  test 0$VERBOSE -gt 0 -o 0$USAGE -gt 0 && echo "$@"
}
#
#
# ----------------------------------------------------------------------------
#
# return true, if verbose
# -----------------------
verbose() {
  test 0$VERBOSE -gt 0
  return $?
}
#
# ============================================================================
# ----
# read command line options
# -------------------------
TEMP=$(getopt -o hb:t:y:Dv --long \
  help,year:,basepath:,tmpfile:,verbose,debug \
  -n $(basename $0) -- "$@") || {
    echo >&2
    echo >&2 "ERROR: command line parameters are incorrect!"
    echo >&2 "aborting $0..."; exit 2
}
eval set -- "$TEMP"
# ----
#
echo '******* DL1sum.sh V'$VERSION' *******' 
#
# print usage if no command line parameter is present
USAGE=0
if test $# -le 1 
then
  USAGE=1
fi
#

#
# extract command line options
# ----------------------------
BASEPATH=/data/BFO/DL1/data/thiesdl1/
TMPFILE=$HOME/tmp/DL1sum
OUTFILE=DL1sum.txt
OUTPSFILE=DL1sum.ps
YEAR=$(date +'%Y')
VERBOSE=0
while true; do
  case "$1" in
    --help|-h) usage; echo; longusage; exit 1;;
    --) shift; break;;
    -b|--basepath) BASEPATH=$2; shift;;
    -y|--year) YEAR=$2; shift;;
    -t|--tmpfile) TMPFILE=$2; shift;;
    -v|--verbose) VERBOSE=1;;
    -D) set -x ;;
    *) echo >&2 "ERROR: option $1 unprocessed!";
       echo >&2 "aborting $0..."; exit 2;;
  esac
  shift
done
#
# ============================================================================
# print greetings
test $USAGE -gt 0 && usage 
# ============================================================================
#
# exit if no months are selected
test $# -eq 0 && exit 1
#
#
MAXLEN=0
echo > $OUTFILE
{
for month in $*
do
  vecho month: $month
  MONTHPAT=$(printf "%2.2d" $month)
  FILES=$(egrep '^# earliest date: ... ...'$MONTHPAT'.'$YEAR $(find $BASEPATH -type f) | cut -f 1 -d':' | sort)
  for file in $FILES
  do
    FILE=$(echo $file | sed -e s,$BASEPATH,,)
    printf "cus  0\nend\n" \
      | tidofi -o -cs -type thiesdl1:tr -Type bin $TMPFILE $file 
    export LANG=C
    LINE=$(printf "%s: %5.1f   %s\n" \
      $(sigval --format="%D %MAX" -type bin $TMPFILE) $FILE)
    LEN=$(echo "$LINE" | wc -c)
    if test $LEN -gt $MAXLEN; then MAXLEN=$LEN; fi
    echo "$LINE" >> $OUTFILE
  done
  echo '' >> $OUTFILE
done
} 
a2ps $OUTFILE -o $OUTPSFILE -r -2 -L 31 -l $MAXLEN \
  --center-title='DL1sum.sh V'$VERSION
#
# ----- END OF DL1sum.sh ----- 
