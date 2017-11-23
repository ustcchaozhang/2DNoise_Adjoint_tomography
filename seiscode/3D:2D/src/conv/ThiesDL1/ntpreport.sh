#!/bin/bash
# this is <ntpreport.sh>
# ----------------------------------------------------------------------------
# 
# Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
# 
# report ntp status of peers
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
#    01/01/2009   V1.0   Thomas Forbriger
# 
# ============================================================================
#
echo "check status of NTP peers" | /bin/logger -i -p user.notice -t "DL1logger $0" 2>&1
/usr/sbin/ntpq -p | /bin/logger -i -p user.notice -t "DL1logger $0" 2>&1
# ----- END OF ntpreport.sh ----- 
