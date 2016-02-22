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

import re

import testbox

class WindowTranslate(cg.Translate):

  def extract(filename, algebra):
    pat_score = re.compile('^Optimal Score: ([-.0-9]+)')
    pat_algebra = re.compile('^Algebra: ([A-Za-z0-9]+)')
    pat_end = re.compile('^================================================================================')
    pat_begin = re.compile('^============================== Suboptimal Output ===============================')
    pat_cand = re.compile('^(.+) \(Score: ([-.0-9]+)\)')
    pat_bounds = re.compile('^([0-9]+) +([0-9]+)')

   
    f = open(filename)
    look = testbox.look_into([filename])
    d = cg.WindowData()
    bounds = None
    length = -1
    state = -1
    for i in f:
      if state == -1:
        match = pat_algebra.search(i)
        if match:
          if algebra == '' or algebra == match.group(1):
            al = algebra
          else:
            raise cg.TranslateError('Wrong algebra in window mode: ' + algebra
              + ' vs. ' + match.group(1) + look)
          state = 0
      elif state == 0:
        match = pat_score.search(i)
        if match:
          e = cg.Data()
          e.algebra = al
          e.optimal_score = float(match.group(1)) / 100
          state = 1
          continue
        else:
          match =  pat_bounds.search(i)
          if match:
            bounds = ( int(match.group(1)), int(match.group(2)) )
        if length < 0:
          length = len(i) - 1
        elif len(i) - 1 != length:
          raise cg.TranslateError('Length of sequence/bounds differs in window '
              + str(bounds) + ':\n'
              + '  ' + i + ' (len: ' + str(len(i) - 1) + ' vs. len: ' 
              + str(length) + look)
      elif state == 1:
        match = pat_begin.search(i)
        if match:
          state = 2
        else:
          raise cg.TranslateError('Found no start in window '
              + str(bounds) +  '.' + look)
      elif state == 2:
        match = pat_cand.search(i)
        if match:
          (cand, score) = match.groups()
          if len(cand) != length:
            raise cg.TranslateError('Length of candidate structures does not match in window '
                + str(bounds) +  ':\n'
                + cand + ' (len: ' + str(len(cand)) + ') vs. len: '
                + str(length) + look)
          e.candidates.append((float(score) / 100, cand))
        else:
          match = pat_end.search(i)
          d.addWindow(bounds, e)
          if match:
            state = 0
          else:
            raise cg.TranslateError('Found no end or candidate in window '
                + str(bounds) + '.' + look)

    f.close()
    if state != 0:
      raise cg.TranslateError('Found no end at the end.' + look)
    return d
            
  extract = staticmethod(extract)

