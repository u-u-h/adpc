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

class Data:
  # optimal_score as int or float
  # candidates = [(score as int or float, candiate as str)]
  # algebra as str

  def __init__(self):
    self.candidates = []

  def compare(self, y):
    if self.optimal_score != y.optimal_score:
      raise cg.DataError('Optimal score is different: '
          + str(self.optimal_score)  + ' vs. ' + str(y.optimal_score))
    
    if len(self.candidates) != len(y.candidates):
      raise cg.DataError('Different number of candidates: '
          + str(len(self.candidates)) + ' vs. ' + str(len(y.candidates)))

    for i in range(0, len(self.candidates)):
      (s1, t1)  = self.candidates[i]
      (s2, t2)  = y.candidates[i]
      if s1 != s2:
        raise cg.DataError('Scores of ' + str(i) + '. candidate differ: ' + 
          str(s1) + ' vs. ' + str(s2) + '\nStructures are:\n' +
          t1 + '\nvs.\n' + t2)
      if t1 != t2:
        raise cg.DataError('Structures of ' + str(i) + '. candidate differ:\n' +
          t1 + '\nvs.\n' + t2 + '\nScore is equal: ' + str(s1))

    if self.algebra == '' or y.algebra == '':
      return True

    if self.algebra != y.algebra:
      raise cg.DataError('Algebra names differ: ' + self.algebra + ' vs. '
        + y.algebra)
    return True

  def __eq__(self, y):
    return self.compare(y)

  def __ne__(self, y):
    return not self.__eq__(y)

  def setScore(self, s):
    self.optimal_score = self.convert(s)

  def addCandidate(self, c):
    self.candidates.append((self.convert(c[0]), c[1]))

  def convert(self, s):
    if '.' in s:
      return float(s)
    else:
      return int(s)


class WindowData:

  #windows - list of Data
  def __init__(self):
    self.windows = []

  def addWindow(self, bounds, d):
    self.windows.append((bounds, d))

  def compare(self, y):
    if len(self.windows) != len(y.windows):
      raise cg.DataError('Different number of windows:\n'
          + '  ' + str(len(self.windows)) + ' vs. ' + str(len(y.windows)))
    l = zip(self.windows, y.windows)
    for ((a, i), (b, j)) in l:
      if a != b:
        raise cg.DataError('Window boundaries are different: ' + str(a)
        + ' vs. ' + str(b))
      try:
        i.compare(j)
      except cg.DataError, message:
        raise cg.DataError('Window ' + str(a) + ' is different:\n' 
            + str(message))

  def __eq__(self, y):
    return self.compare(y)

  def __ne__(self, y):
    return not self.__eq__(y)


