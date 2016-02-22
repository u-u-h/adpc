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

import unittest

import config

import testbox


class GrammarTest(testbox.TestCase):
  config = config.Config

  # gramamr as str

  def __init__(self, foo):
    testbox.TestCase.__init__(self, foo)

  def id(self):
    return "%s.%s#%s" % (_strclass(self.__class__),
        self._TestCase__testMethodName,
        self.grammar)

  def shortDescription(self):
    doc = self._testMethodDoc
    return doc and doc.split("\n")[0].strip() + " (" + self.grammar + ")" \
      or None

  def generate(self):
    print "generate"


