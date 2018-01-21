#!/bin/bash
# this is <depcheck.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2010, 2014 by Thomas Forbriger (BFO Schiltach) 
# 
# check dependencies found in include statements in C and C++ source code
#
# ----
# depcheck.sh is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version. 
# 
# depcheck.sh is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
# ----
#
# I recommend to execute
#   depcheck.sh $(find . -name \*.cc -o -name \*.c -o -name \*.h)
# in your library source directory
# 
# REVISIONS and CHANGES 
#    16/10/2010   V1.0   Thomas Forbriger (thof)
#    24/01/2014   V1.1   locate dependencies
#    31/01/2014   V1.2   operate on source code directly
#    01/02/2014 thof:    traverse the full chain of included files
# 
# ============================================================================
#
dependency()
{
  printf "${1}:"
  cpp -M $CPPFLAGS $1 | sed -e 's/\\$/ /' | tr '\n' ' '
  echo
}

dependencies()
{
  for filename in $*
  do
    dependency $filename
  done
}

finddependencies()
{
      tr -d '\\' \
    | tr -s ' ' \
    | tr ' ' '\n' \
    | egrep -v '^ *$'\
    | egrep -v '^/usr/' \
    | egrep -v '^[^/]' \
    | sort \
    | uniq \
    | egrep -v '^ *$' \
    | sed -e "s,^${LOCINCLUDEDIR}/,," \
    | sed -e 's,/.*$,,' \
    | uniq
}

locatedependencies()
{
  read depend
  while test z$depend != z
  do
    echo 
    echo dependency \'$depend\' is used in:
    dependencies $* | grep $depend | cut -f 1 -d ':' | sort | uniq
    read depend
  done
}

#dependencies $* 

echo This is $0
echo 
echo checked files:
echo --------------
ls $*
echo
echo The files depend on:
echo --------------------
dependencies $* | finddependencies 

dependencies $* | finddependencies | locatedependencies $*

# ----- END OF depcheck.sh ----- 
