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

class RnaTranslate(cg.Translate):

  def extract(filename, algebra):
    pat_score = re.compile('^([A-Za-z0-9]+): ([-.0-9]+)')
    pat_cand = re.compile('^([.()\\[\\]]+)  \(([-.0-9]+)\)')
    look = testbox.look_into([filename])

    d = cg.Data()
   
    f = open(filename)
    state = 0
    length = -1
    last_len = -1
    for i in f:
      if state == 0:
        match = pat_score.search(i)
        if match:
          state = 1
          d.algebra = match.group(1)
          d.setScore(match.group(2))
      elif state == 1:
        match = pat_cand.search(i)
        if match:
          if length == -1:
            length = last_len
          d.addCandidate( (match.group(2), match.group(1)) )
          if length != -1 and length != len(match.group(1)):
            raise cg.TranslateError('Candidate structure length deviates:\n'
                + '  ' + match.group(1) + ' (len: ' + str(len(match.group(1)))
                + ') vs. len: ' + str(length) + look)
          last_len = len(match.group(1))
    f.close()
    if state == 0:
      raise cg.TranslateError('Found no algebra at all.' + look)
    if len(d.candidates) == 0 and algebra != 'count':
      raise cg.TranslateError('Empty candidate list.' + look)
    f.close()
    return d

  extract = staticmethod(extract)

