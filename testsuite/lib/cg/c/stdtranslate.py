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

class StdTranslate(cg.Translate):

  def extract(filename, algebra):
    pat_score = re.compile('^Algebra: ([A-Za-z0-9]+), score: ([-.0-9]+)')
    pat_end = re.compile('^=================================================================')
    pat_begin = re.compile('^-----------------------------------------------------------------')
    pat_cand = re.compile('^[ \t]+([-0-9.]+) \| (.+)$')

    d = cg.Data()
    
    f = open(filename)
    state = 0
    for i in f:
      if state == 0:
        match = pat_score.search(i)
        if match:
          if algebra == 'count' and match.group(1) == algebra:
            d.setScore(match.group(2))
            d.algebra = 'count'
            state = 2
          elif match.group(1) == 'count':
            pass
          elif algebra == '' or match.group(1) == algebra:
            state = 1
            d.algebra = match.group(1)
            d.setScore(match.group(2))
      elif state == 1:
        match = pat_begin.search(i)
        if match:
          state = 2
      elif state == 2:
        match = pat_end.search(i)
        if match:
          state = 3
          break
        match = pat_cand.search(i)
        if match:
          d.addCandidate(match.groups())
    f.close()
    look = testbox.look_into([filename])
    if state == 0:
      raise cg.TranslateError('Algebra ' + algebra + ' not found.' + look)
    elif state != 3:
      raise cg.TranslateError('Missing begin and/or end.' + look)
    if len(d.candidates) == 0 and algebra != 'count':
      raise cg.TranslateError('Empty candidate list.' + look)
    f.close()
    return d

  extract = staticmethod(extract)

