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
import time

from logging import info

class TestCase(unittest.TestCase):
  def __init__(self, foo):
    self._unique_filename = ''
    unittest.TestCase.__init__(self, foo)

  def __eq__(self, a):
    return self.attrs == a.attrs

  def __ne__(self, a):
    return not self.__eq__(a)

  def unique_filename(self, exc = []):
    if self._unique_filename == '' or exc != []:
      r = str()
      l = self.attrs[:]
      l.sort()
      for i in l:
        if not i.name in exc:
          r += '.' + i.value
      if exc == []:
        self._unique_filename = self.testsuite_name + '.' + self.class_name + r
      else:
        return self.testsuite_name + '.' + self.class_name + r
    return self._unique_filename

  def run(self, foo):
    info('  Running Test: ' + self.shortDescription())
    l = len(foo.failures)
    m = len(foo.errors)
    r = unittest.TestCase.run(self, foo)
    if not foo.wasSuccessful():
      if l != len(foo.failures):
        info('    Failed:\n' + foo.failures[len(foo.failures)-1][1])
      elif m != len(foo.errors):
        info('    Error:\n' + foo.errors[len(foo.errors)-1][1])
    return r


class MultiTest(TestCase):
  def prepareTestSuite(self):
    raise Exception('not implemented')

  def cleanTestSuite(self):
    raise Exception('not implemented')


class TextTestRunner(unittest.TextTestRunner):

  def runTests(self, tests):
    t = True
    fail = 0
    error = 0
    run = 0
    start = time.time()
    for i in tests:
      self.stream.writeln("######################################################################")
      self.stream.writeln("# Running Test Suite: " + i.desc())
      info("Running Test Suite: " + i.desc())
      r = self.run(i)
      self.stream.writeln("######################################################################")
      fail += len(r.failures)
      error += len(r.errors)
      run += r.testsRun
      if not r.wasSuccessful():
        t = False
    stop = time.time()
    total = 'Status : ' + str(run-fail-error) + ' tests successful, ' \
        + str(fail) + ' tests failed, ' + str(error) \
        + ' tests had internal errors.\nTotal  : ' + str(run) \
        + ' tests (needed %.3fs seconds)' % (stop-start)
    info(total)
    self.stream.writeln('\n\n\n' + total)
    return t




