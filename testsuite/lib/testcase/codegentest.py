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
import re
from logging import error, debug

import cg.c

import testbox

import unittest
import pickle

from adpc import Suffix

class CgBaseTest(testbox.MultiTest):
  # filename = ""
  # algebra = ""
  # type = ""

  # this language is used to generate the state files
  default_language = 'C'

  config = adpc.config.Config

  def __init__(self, foo):
    testbox.TestCase.__init__(self, foo)
    self.language = 'C'
    self.algebra = ''
    self.default_parameter = []
    self.language = self.default_language

  def shortDescription(self):
    if hasattr(self, 'parameter'):
      para = ', ' + self.parameter
    else:
      para = ''
    if not self._testMethodName == 'prepareTestSuite' and\
        not self._testMethodName == 'cleanTestSuite':
      return '   Executing ' + self.grammar + ' under algebra ' \
        + self.algebra + ' (' + self.filename + para + ') [' + self.language + ']'
    else:
      return testbox.MultiTest.shortDescription(self) + ' [' + self.language + ']'

  def create(self, outdir):
    cg.callTable[self.language].create(outdir, self)

  def clean(self, outdir):
    cg.callTable[self.language].clean(outdir, self)

  pat_prefix = re.compile('^(.*)\.lhs$')
  
  def prog(self):
    match = self.pat_prefix.search(self.grammar)
    if match:
      return match.group(1)
    else:
      self.fail()

  def call(self, outdir):
    cg.callTable[self.language].execute(outdir, self.prog(), self.filename,
        self)

  def prepareTestSuite(self):
    """ Create adp program """
    if self.type == 'gen':
      self.create(self.config.state)
    else:
      self.create(self.config.output)

  def cleanTestSuite(self):
    """ Remove adp program """
    if self.type == 'gen':
      self.clean(self.config.state)
    else:
      self.clean(self.config.output)

  def generate(self):
    self.call(self.config.state)
    if self.language != self.default_language:
      return
    filename = self.unique_filename() + Suffix.out
    d = self.extract(os.path.join(self.config.state, filename))
    filename = self.unique_filename(['language']) + Suffix.pickle
    testbox.checkFilename(filename)
    f = open(os.path.join(self.config.state, filename), 'w')
    pickle.dump(d, f)
    f.close()

  def unpickle(self):
    filename = self.unique_filename(['language']) + Suffix.pickle
    f = open(os.path.join(self.config.state, filename))
    d = pickle.load(f)
    f.close()
    return d

  def extract(self, filename):
    return cg.translateTable[self.language, self.flavour].extract(filename, self.algebra)
    
  def test(self):
    filename_out = self.unique_filename() + Suffix.out
    self.call(self.config.output)
    a = self.unpickle()
    f1 = self.config.state + os.path.sep + filename_out
    # uncomment this to debug the Translators
    #a = self.extract(f1)
    f2 = os.path.join(self.config.output, filename_out)
    b = self.extract(f2)
    try:
      a.compare(b)
    except cg.DataError, (message):
      self.fail('Different program output:\n' + str(message)
        + testbox.look_into([f1, f2]) + '\nor into the corresponding default language state-file\n')

 
  # ignores the laste value, which is algebra by default
  # thus, the programs get only generatad for different Grammars
  #  -> saves redundant recompilations ...
  def __eq__(self, other):
    (a, b) = (self.attrs, other.attrs)
    if len(a) != len(b):
      return False
    l = zip(a, b)
    for (x, y) in l:
      if x.name == 'algebra' or x.name == 'parameter' or x.name == 'filename':
        continue
      if x.name != y.name or x.value != y.value:
        return False
    return True




class CodegenTest(CgBaseTest):

  def __init__(self, foo):
    CgBaseTest.__init__(self, foo)
    self.flavour = 'std'


class RnaTest(CgBaseTest):

  def __init__(self, foo):
    CgBaseTest.__init__(self, foo)
    self.flavour = 'rna'

class WindowTest(CgBaseTest):
  def __init__(self, foo):
    CgBaseTest.__init__(self, foo)
    self.flavour = 'window'
    self.default_parameter = ['-x']


