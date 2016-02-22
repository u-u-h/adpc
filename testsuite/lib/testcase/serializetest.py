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

import adpc
import os
import sysext

from adpc import Suffix
import testbox

class SerializeTest(adpc.GrammarTest):

  def serialize(self, out_dir):
    c = self.config
    os.chdir(c.grammars)
    out = self.grammar + Suffix.i + Suffix.out
    testbox.checkFilename(out)
    err = self.grammar + Suffix.i + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(out_dir, out)
    f2 = os.path.join(out_dir, err)
    i = sysext.sys(c.adpcompile,
        ['-iuc', '-te', os.path.join(c.grammars, self.grammar) ], f1, f2)
    self.failIf(i != 0, "Couldn't create serialized output."
        + testbox.look_into([f1, f2]))
    return i

  # geht davon aus, dass adpcompile -te keine Optionen in das
  # array serialisiert ... (am Anfang bevor NT-count)
  def testSerialize(self):
    """serialized output from adpc"""
    i = self.serialize(self.config.output)
    suff = self.grammar + Suffix.i + Suffix.out
    f1 = os.path.join(self.config.state, suff)
    f2 = os.path.join(self.config.output, suff)
    f = open(f1)
    g = open(f2)
    l1 = f.readline().split(';')
    l2 = g.readline().split(';')
    f.close()
    g.close()
    look = testbox.look_into([f1, f2])
    self.failIf(len(l1) != len(l2),
      "Length of serialized array is different" + look)
    self.failIf(l1 != l2, "Serialized arrays are not equal" + look)

  def generate(self):
    """generate serialized output from adpc"""
    i = self.serialize(self.config.state)

