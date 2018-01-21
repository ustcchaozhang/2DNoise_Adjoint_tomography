#!/bin/bash
# this is <launchDL1logger.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
# 
# launch DL1logger from crontab
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
#    19/12/2008   V1.0   Thomas Forbriger
# 
# ============================================================================
echo "check logger" | /bin/logger -i -p user.info -t "DL1logger $0" 2>&1
SCRIPTDIR=$HOME/bin/scripts
LOGGERSCRIPT=$SCRIPTDIR/DL1logger.sh
LOGDIR=$HOME/DL1/log
mkdir -pv $LOGDIR | /bin/logger -i -p user.info -t "DL1logger $0" 2>&1
LOGFILE=$LOGDIR/$(date +'%Y%m%d.%H%M').log

/sbin/checkproc $LOGGERSCRIPT
status=$?
if test $status != 0
then
  echo "/usr/bin/nohup $LOGGERSCRIPT 2>&1 >> $LOGFILE &" \
    | /bin/logger -i -p user.notice -t "DL1logger $0" 2>&1
  /usr/bin/nohup $LOGGERSCRIPT >> $LOGFILE 2>&1 &
fi

# ----- END OF launchDL1logger.sh ----- 
