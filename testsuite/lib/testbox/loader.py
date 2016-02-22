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
from logging import debug

import testcase

import unit


class TestSuite(unittest.TestSuite):

  def __init__(self, tests=(), desc=''):
    unittest.TestSuite.__init__(self, tests)
    self.description = desc
    
  def desc(self):
    return self.description



class TestLoader(unittest.TestLoader):
  suiteClass = TestSuite

  # stack

  def print_stack(self, fn_name=''):
    if not fn_name == '':
      testCaseNames = [ fn_name ]
    else:
      testCaseNames = self.getTestCaseNames(self.testCaseClass)

    if hasattr(self.testCaseClass, 'construct'):
      bar = self.testCaseClass(testCaseNames[0])
      for i in self.stack:
        setattr(bar, i.name, i.value)
      foo = list()
      for i in bar.construct():
        foo.append(self.stack + i)
    else:
      foo = [ self.stack[:] ]

    for i in foo:
      old_stack = self.stack
      self.stack = i

      for i in testCaseNames:
        debug('ZZZ: ' + i)
        testCase = self.testCaseClass(i)
        testCase.type = self.testMethodPrefix
        debug('Erzeuge: ')
        for i in self.stack:
          setattr(testCase, i.name, i.value)
          debug('attr: ' + i.name + ' value: ' + str(i.value))
  
        testCase.attrs = self.stack[:]
        testCase.testsuite_name = self.testsuite_name
        testCase.class_name = self.class_name
  
        if len(self.testsuite._tests) > 0 and \
           self.testsuite._tests[-1] == testCase and \
           self.testsuite._tests[-1]._testMethodName == 'cleanTestSuite':
             self.testsuite._tests.insert(len(self.testsuite._tests)-1, testCase)
        elif isinstance(testCase, unit.MultiTest):
          tc = self.testCaseClass('prepareTestSuite')
          tc.type = self.testMethodPrefix
          tc.attrs = self.stack[:]
          tc.testsuite_name = self.testsuite_name
          tc.class_name = self.class_name
          for i in self.stack:
            setattr(tc, i.name, i.value)
          self.testsuite.addTest(tc)
          self.testsuite.addTest(testCase)
          tc = self.testCaseClass('cleanTestSuite')
          tc.type = self.testMethodPrefix
          tc.attrs = self.stack[:]
          tc.testsuite_name = self.testsuite_name
          tc.class_name = self.class_name
          for i in self.stack:
            setattr(tc, i.name, i.value)
          self.testsuite.addTest(tc)
        else:
          self.testsuite.addTest(testCase)

      self.stack = old_stack

  def print_branch(self, klasse):
    for i in klasse.specials.children():
      self.stack.append(i)
    if len(klasse.attrs.children()) == 0:
      self.print_stack()
    else:
      for i in klasse.attrs.children():
        self.stack.append(i)
        
        self.print_branch(i)

        self.stack.pop()
    for i in klasse.specials.children():
      self.stack.pop()
  
  def getClass(self, k):
    debug('XXX: ' + k.name + ' ' + k.value)
    # XXX k.value ueberfluessig nun
    klasse = getattr(testcase, k.name)
    return klasse

  def print_ts(self, ts):
    debug('Testsuite: ' + ts.name + ' ' + ts.value)
    for i in ts.children():
      self.class_name = i.name
      self.testCaseClass = self.getClass(i)
      if not issubclass(self.testCaseClass, unittest.TestCase):
        raise TypeError("No unittest.TestCase subclass!"\
            + '(' + i.name + 'from ' + i.value + ')')
      # for foo in testmethod names 
      # if class have global init insert testcase
      self.print_branch(i)
      # if class have global exit insert testcase

  def loadTestSuites(self, filename='test.conf'):
    p = config.Parser(filename)
    (ts, df, defs) = p.parse()
    ts_dict = dict()
    self.stack = []

    debug('Defaults:')
    debug(defs)

    l = zip(ts, defs)
    for i in ts:
      if self.testMethodPrefix == 'gen':
        ts_name = 'Generate state: ' + i.value
      else:
        ts_name = i.value
      self.testsuite_name = i.name
      self.testsuite = self.suiteClass((), ts_name)
      ts_dict[i.name] = self.testsuite
      self.print_ts(i)
    return (defs, ts_dict)

  def loadTestCollection(self, filename='test.conf'):
    a = self.loadTestSuites(filename)
    temp = self.testMethodPrefix
    self.testMethodPrefix = 'gen'
    b = self.loadTestSuites(filename)
    self.testMethodPrefix = temp
    return (a[0], a[1], b[1])


if __name__ == '__main__':
  l = TestLoader()
  a = l.loadTestSuites()
