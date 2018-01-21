#!/bin/bash
# this is <compile.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# compile Seitosh
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
# Support automatic sequential compilation of maintained components in
# Seitosh and others
# 
# REVISIONS and CHANGES 
#    13/02/2014   V1.0   Thomas Forbriger (thof)
#    19/02/2014 thof:    added a few directories
#    09/02/2015 thof:    adjust to seitosh directories
#    13/05/2015 thof:    switch to home of compile.sh prior to execution
#    14/05/2015 thof:    introduce dry-run option
#                        read list of directories from file
#    15/05/2015 thof:    provide extended error handling
# 
# ============================================================================
#
# indicate version
VERSION=2015-05-15
# 
# ============================================================================
# **** define usage functions                                             ****
# ============================================================================
#
usage() {
cat >&2 << HERE
usage: compile.sh goal [goal ...]
                  [--verbose|-v] [--list|-l] [--find|-f] [--dry-run|-n]
                  [--dirlist|-d file] [--continue|-c] [--pause|-p]
                  [--missing|-m]
   or: compile.sh --help|-h
HERE
}
#
# ----------------------------------------------------------------------------
#
longusage() {
cat >&2 <<HERE

Compile maintained modules of Seitosh (or other source code collections)

  -v or --verbose       be verbose
  -l or --list          list the directories to be consideredѕ
                        these are the directories as defined in the dirlist
                        file (see default file name given below)
  -f or --find          find Makefiles
                        this option is used to check whether the list of files
                        defined in the dirlist file is complete (notice that
                        there might be more Makefiles in subdirectories than
                        there are to be addressed by compile.sh directly).
  -n or --dry-run       just check for goals, do not execute them
  -d or --dirlist file  use an alternate file to read the directory list from
  -p or --pause         in case of an error do not abort immediately
                        but pause and wait for confirmation by the user
  -c or --continue      in case of an error when running make for goal in one
                        directory, do not abort immediately, but collect all
                        error reports and issue a message at the end of the
                        loop
  -m or --missing       make a missing Makefile or goal an error condition

  goal                  make goal

The script traverses along a list of directories, checks, whether the selected
goal is present in the Makefile in the directory and then executes make for
this goal. Several goals may be passed as a white-space separated list.

The default file name for the list of directories is:
$DIRLISTFILE

Options can be passed to make by enclosing them in quotes together with the
goal. The goal must be given first followed by options. Example:

  compile.sh "install -i -j"
HERE
}
#
# ============================================================================
# **** define action functions                                            ****
# ============================================================================
#
# simply abort
abort() {
  echo >&2 "aborting..."
  exit 2
}
#
# ----------------------------------------------------------------------------
#
# return true if in verbose mode
verbose() {
  test 0$VERBOSE -gt 0
}
#
# ----------------------------------------------------------------------------
#
# check whether a directory contains a file called Makefile or not
#
# parameter $1: name of directory of expected Makefile
# return: true if Makefile is present and readable in directory
#
hasmake() {
  DIR="$1"
  test -r $DIR/Makefile
}
#
# ----------------------------------------------------------------------------
#
# check whether a given Makefile contains a given goal
#
# parameter $1: name of make goal
# parameter $2: path of Makefile
# return: true, if the specific goal is available in the Makefile
# 
hasgoal() {
  THEGOAL=$(echo $1 | cut -f 1 -d ' ')
  THEDIR="$2"
  hasmake "$THEDIR" \
    && test $(egrep -c '^'"${THEGOAL}": ${THEDIR}/Makefile) -gt 0
}
#
# ----------------------------------------------------------------------------
#
# print a formatted list of directories and issue a warning if the directory
# does not contain a Makefile
#
# parameter $*: white-space separated list of directories
#
reportdirs() {
  printf "\nDirectories:\n"
  while test -n "$1"
  do
    echo "  $1"
    if ! hasmake "$1"
    then
      echo >&2 "     ** missing Makefile!"
    fi
    shift
  done
}
#
# ----------------------------------------------------------------------------
#
# make goal in given directory
#
# parameter $1: make goal
# parameter $2: directory of Makefile
#
makegoalindir() {
  GOAL="$1"
  DIR="$2"
  printf "\nnext:\n  directory: $DIR\n  goal: $GOAL\n"
  if hasmake "$DIR"
  then
    if hasgoal "$GOAL" "$DIR"
    then
      if test $DRYRUN -gt 0
      then
        echo "    make" -C "$DIR" $GOAL 
      else
        make -C "$DIR" $GOAL 
        STATUS=$?
        if test $STATUS -gt 0
        then
          echo >&2 "    goal \"$GOAL\" in $DIR failed!"
          if test 0$ERROR_CONTINUE -gt 0
          then
            (( NERROR++ ))
            ERRORMSG[$NERROR]="goal \"$GOAL\" in $DIR failed!"
          else
            if test 0$ERROR_PAUSE -gt 0
            then
              pause abort
            fi
            abort
          fi
        fi
      fi
    else
      echo >&2 "  ** goal \"$GOAL\" is missing in $DIR!"
      if test 0$ERROR_MISSING -gt 0
      then
        if test 0$ERROR_CONTINUE -gt 0
        then
          (( NERROR++ ))
          ERRORMSG[$NERROR]="goal \"$GOAL\" is missing in $DIR"
        else
          if test 0$ERROR_PAUSE -gt 0
          then
            pause abort
          fi
          abort
        fi
      fi
    fi
  else
    echo >&2 "  ** Makefile is missing in $DIR!"
    if test 0$ERROR_MISSING -gt 0
    then
      if test 0$ERROR_CONTINUE -gt 0
      then
        (( NERROR++ ))
        ERRORMSG[$NERROR]="Makefile is missing in $DIR"
      else
        if test 0$ERROR_PAUSE -gt 0
        then
          pause abort
        fi
        abort
      fi
    fi
  fi
}
#
# ----------------------------------------------------------------------------
#
# make goal in a set of directories
#
# parameter $1: make goal
# parameter $2...: names of directories
# 
makegoal() {
  GOAL="$1"
  shift
  while test -n "$1"
  do
    makegoalindir "$GOAL" "$1"
    shift
  done
}
#
# ----------------------------------------------------------------------------
#
# print string an wait for confirmation
#
# parameter $*: string to print
#
pause() {
  echo press enter to $*
  read a
}
#
# ============================================================================
# **** define variables                                                   ****
# ============================================================================
#
# location of compile.sh and expected location of install_src_dirs.txt
BASEDIR=$(dirname $0)
#
# set default name of directory list file
DIRLISTFILE=$BASEDIR/install_src_dirs.txt
#
# number of errors encountered during operation
export NERROR=0
# array to store error messages
declare -a ERRORMSG
#
# ============================================================================
# **** action                                                             ****
# ============================================================================
#
#
echo '******* compile.sh V'$VERSION' *******' 
#
# ----
# read command line options
# -------------------------
TEMP=$(getopt -o Dhvnfld:pcm --long \
  help,verbose,dry-run,find,list,dirlist:,pause,continue,missing \
  -n $(basename $0) -- "$@") || {
    echo >&2
    echo >&2 "ERROR: command line parameters are incorrect!"
    echo >&2 "aborting $0..."; exit 2
}
eval set -- "$TEMP"
#
# print usage if no command line parameter is present
USAGE=0
if test $# -le 1 
then
  USAGE=1
fi
#
# extract command line options
# ----------------------------
export VERBOSE=0
export DRYRUN=0
export ACTION_FIND=0
export ACTION_LIST=0
export ERROR_PAUSE=0
export ERROR_CONTINUE=0
export ERROR_MISSING=0
while true; do
  case "$1" in
    --help|-h) usage; echo; longusage; exit 1;;
    --) shift; break;;
    -v|--verbose) VERBOSE=1;;
    --dry-run|-n) DRYRUN=1;;
    --find|-f) ACTION_FIND=1;;
    --list|-l) ACTION_LIST=1;;
    --pause|-p) ERROR_PAUSE=1;;
    --continue|-c) ERROR_CONTINUE=1;;
    --missing|-m) ERROR_MISSING=1;;
    --dirlist|-d) DIRLISTFILE=$2; BASEDIR=$(dirname $DIRLISTFILE); shift;;
    -D) set -x ;;
    *) echo >&2 "ERROR: option $1 unprocessed!";
       echo >&2 "aborting $0..."; exit 2;;
  esac
  shift
done
#
# ============================================================================
# read list of directories
verbose && echo read list of directories from $DIRLISTFILE
if test ! -r $DIRLISTFILE
then
  echo >&2 "ERROR: directory list $DIRLISTFILE is not available!"
  if test 0$ERROR_PAUSE -gt 0
  then
    pause abort
  fi
  echo >&2 "aborting $0..."; exit 2
fi
DIRECTORIES="$(egrep -v '^#' $DIRLISTFILE | xargs -I XX echo ${BASEDIR}/XX)"
#
# ----------------------------------------------------------------------------
#
#
# ============================================================================
# print greetings
test $USAGE -gt 0 && usage 
#
if test 0$ACTION_LIST -gt 0
then
  echo
  echo The following directories are read from
  echo $DIRLISTFILE
  echo to be considered for make goals.
  reportdirs $DIRECTORIES
fi
if test 0$ACTION_FIND -gt 0
then
  MAKEDIRECTORIES="$(find $BASEDIR -name Makefile | xargs dirname)"
  echo
  echo The following directories contain a file named Makefile.
  echo They might contain goals to be considered.
  reportdirs $MAKEDIRECTORIES
fi
while test -n "$1"
do
  verbose && echo run make for goal \"$1\"
  makegoal "$1" $DIRECTORIES
  shift
done
if test $NERROR -gt 0
then
  echo
  echo >&2 "reported errors:"
  for ((i=1; i<=NERROR; ++i))
  do
    echo >&2 ${ERRORMSG[$i]}
  done
  if test 0$ERROR_PAUSE -gt 0
  then
    pause abort
  fi
  abort
fi
# ----- END OF compile.sh ----- 
