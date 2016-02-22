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


from unit import *
from tests import main

import traceback

class FileNameException(Exception):
  pass

_filenames = dict()

def checkFilename(filename):
  if _filenames.has_key(filename):
    raise FileNameException("Already present:" + filename + '\n' +
        'previous location: \n{' + _filenames[filename] + '}')
  else:
    stack = traceback.extract_stack()
    for (fn, ln, f, m) in stack:
      _filenames[filename] = fn + ':' + str(ln) +':' + f + ' '


def look_into(l):
  r = '\nFor details look into the files:\n'
  for i in l:
    r += '  ' + i + '\n'
  return r
