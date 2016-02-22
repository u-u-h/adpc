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

import cg
import cg.translate

import re

import testbox

class Equal:
  def __eq__(self, a):
    return True

  def __ne__(self, a):
    return not self.__eq__(a)


class WindowTranslate(cg.Translate):

  def extract(filename, algebra):
    pat_bounds = re.compile('^([0-9]+) +([0-9]+)')
    pat_cand = re.compile('^([.()\\[\\]]+)  \(([-.0-9]+)\)')

   
    f = open(filename)
    look = testbox.look_into([filename])
    d = cg.WindowData()
    e = cg.Data()
    bounds = None
    for i in f:
      match = pat_bounds.search(i)
      if match:
        e.optimal_score = Equal()
        e.algebra = algebra
        d.addWindow(bounds, e)
        e = cg.Data()
        g = match.groups()
        bounds = (int(g[0]), int(g[1]))
        length = len(i) - 1
      else:
        match = pat_cand.search(i)
        if match:
          g = match.groups()
          if len(g[0]) !=  length:
            raise cg.TranslateError('Length of candidate structure does not match in window ' + str(bounds) + ':\n"'
                + '  ' + g[0] + ' (len: ' + str(len(g[0])) + ') vs len: '
                + str(length) + look)
          e.addCandidate((g[1], g[0]))
        else:
          if len(i) - 1 != length:
            raise cg.TranslateError('Length of sequence does not match in window ' + str(bounds) + ':\n'
                + '  ' + i + ' (len: ' + str(len(i)-1) + ') vs. len: '
                + str(length) + look)

    f.close()

    e.optimal_score = Equal()
    e.algebra = algebra
    d.addWindow(bounds, e)
    d.windows.pop(0)
    return d
            
  extract = staticmethod(extract)

