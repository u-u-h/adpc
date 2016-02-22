# ------------------------------------------------------------------------------
# -- The ADP Compiler 
# -- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
# --                         Georg Sauthoff, Stefanie Schirmer
# --
# -- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
# -- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
# ------------------------------------------------------------------------------
# 
# This file is part of ADPC (The ADP Compiler).
# 
# ADPC is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
# 
# ADPC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with ADPC.  If not, see <http://www.gnu.org/licenses/>.

import os
import posix
import stat

def which(s):
  for i in os.getenv('PATH').split(':'):
    x = os.path.normpath(os.path.join(i, s))
    try:
      st = os.stat(x)
    except:
      continue
    if not stat.S_ISREG(st[stat.ST_MODE]):
      continue
    r = stat.S_IMODE(st[stat.ST_MODE])
    uid = st[stat.ST_UID]
    gid = st[stat.ST_GID]
    if (r & 0001) or (gid in posix.getgroups() and r & 0010) \
      or (uid == posix.getuid() and r & 0100):
      return x

