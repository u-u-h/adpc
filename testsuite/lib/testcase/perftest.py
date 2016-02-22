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

import codegentest
import adpc
import sysext
import cg

from testbox.config import Content

import os

tout = 10 * 60

def sys_timeout(progname, params, a, b):
  # timeout version aufrufen ... und memtime chainen
  # FIXME im java fall garbage collection etc uebergeben
  return sysext.sys(adpc.Config.memtime, [progname] + params, "/dev/null", b, timeout = tout)


def set_timeout(i):
  global tout
  tout = i

class PerfBase():

  skip = -1

  def construct(self):
    r = list()
    if not hasattr(self, 'subopts'):
      self.subopts = 0
      self.sub_step = 1
    if hasattr(self, 'start'):
      start = int(getattr(self, 'start'))
    else:
      start = int(self.step)
    for k in range(0, int(self.subopts)+int(self.sub_step), int(self.sub_step)):
      for j in range(start, int(self.length)+int(self.step), int(self.step)):
        for i in range(0, int(self.repeats)):
          r.append(
              [Content('repeat', str(i)), Content('n', str(j)), Content('subopt', str(k)),
                Content('filename', 'rnaseq.' + str(j)) ] )
    return r

  def __eq__(self, other):
    (a, b) = (self.attrs, other.attrs)
    if len(a) != len(b):
      return False
    l = zip(a, b)
    for (x, y) in l:
      # FIXME should work for CgBase too -> move it there
      if x.name == 'language' and x.value != y.value:
        return False
    return True

  def unique_filename(self, exc = []):
    if self._unique_filename == '':
        self._unique_filename = self.testsuite_name + '.' + self.class_name + \
            '.language=' + self.language + '.n=' + self.n + '.subopt=' + self.subopt + '.repeat=' + \
            self.repeat
    return self._unique_filename


  def clean_quit(self):
    #self.cleanTestSuite()
    #self.result.stop()
    PerfBase.skip = self.subopt
    adpc.Config.input = self.old_input_dir
    adpc.Config.outpt = self.old_output_dir

  def fail(self, msg=None):
    self.clean_quit()
    raise self.failureException, msg

  def make_input(self):
    name = os.path.join(adpc.Config.input, self.filename)
    if not os.path.exists(name):
      i = sysext.sys(adpc.Config.randseq, [ '-l', self.n ], name, 
          name + adpc.Suffix.err)
      if i != 0:
        self.fail('randseq failed')

  def test(self):
    if not PerfBase.skip == self.subopt:
      PerfBase.skip = -1
    elif PerfBase.skip == self.subopt:
      return

    self.old_input_dir = adpc.Config.input
    adpc.Config.input = adpc.Config.perf_input
    self.old_output_dir = adpc.Config.output
    adpc.Config.output = adpc.Config.perf_output

    if hasattr(self, 'timeout'):
      set_timeout(int(self.timeout))

    if hasattr(self, 'parameter'):
      self.parameter += ' -c ' + self.subopt
    else:
      self.parameter = '-c ' + self.subopt
    self.make_input()
    try:
      cg.callTable[self.language].execute(adpc.Config.perf_output, self.prog(),
        self.filename, self, sys_fn = sys_timeout)
    except cg.CallError, error:
      self.clean_quit()
      raise error
    adpc.Config.input = self.old_input_dir
    adpc.Config.outpt = self.old_output_dir


  def run(self, result):
    self.result = result # call stop() on it if rest of testsuite should be
                         # aborted (if last perf test timed out for ex)
    codegentest.CgBaseTest.run(self, result)

  # extract time als extra programm
  # also ein viele kleine dateien nach gnuplot input converter ...


# Pro testsuite eine Klasse und eine Sprache nur, damit bei out-of-memory etc abgebrochen werden kann

class CodegenPerf(PerfBase,codegentest.CodegenTest):
  pass

class RnaPerf(PerfBase,codegentest.RnaTest):
  pass

class WindowPerf(PerfBase,codegentest.WindowTest):
  pass

import tuple

class TuplePerf(PerfBase,tuple.TupleTest):
  pass
