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

import shutil
import os

import cg
import adpc
import sysext

import re

from adpc import Suffix
import testbox

class Call(cg.Call):
  def create(outdir, tc):
    Call._create_a(outdir, tc)
    Call._create_b(outdir, tc)
  create = staticmethod(create)

  def _create_a(outdir, tc):
    c = adpc.Config
    os.chdir(c.workdir)
    shutil.copy(os.path.join(c.grammars, tc.grammar), c.workdir)
    out = tc.unique_filename() + Suffix.adp + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.adp + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.adpc_bin, [c.binpath, tc.grammar],
        f1, f2)
    if i != 0:
      raise cg.CallError('generating c src from adp program failed'
          + testbox.look_into([f1, f2]))
  _create_a = staticmethod(_create_a)

  def _create_b(outdir, tc):
    c = adpc.Config
    out = tc.unique_filename() + Suffix.make + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.make + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.make, ["BINPATH=" + c.binpath,
        "ADPCOMPILE=" + c.adpcompile,
        "ADPC=" + c.adpc_bin + " " + c.binpath],
        f1, f2)
    if i != 0:
      raise cg.CallError('making adp program failed' + 
          testbox.look_into([f1, f2]))
  _create_b = staticmethod(_create_b)

  def clean(outdir, tc):
    c = adpc.Config
    os.chdir(c.workdir)
    out = tc.unique_filename() + Suffix.clean + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.clean + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.make, ["clean"], f1, f2)
    if i != 0:
      raise cg.CallError("make clean failed" + testbox.look_into([f1, f2]))
    os.unlink(tc.grammar)
    ls = os.listdir(c.workdir)
    if not (ls == [] or ls == ['.svn'] or ls == ['.hg']):
      raise cg.CallError("make clean doesn't clean everything"
          + testbox.look_into([c.workdir]))
  clean = staticmethod(clean)

  def execute(outdir, progname, filename, tc, sys_fn = sysext.sys):
    c = adpc.Config
    os.chdir(c.workdir)

    if hasattr(tc, 'parameter'):
      para = tc.parameter.split()
    else:
      para = []

    out = tc.unique_filename() + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sys_fn('./' + progname,
       [ "-f", c.input + os.path.sep + filename] + para,
       f1, f2)
    if i != 0:
      raise cg.CallError("error executing adp program (exit status: " + str(i) + ")" 
          + testbox.look_into([f1, f2]))
  execute = staticmethod(execute)

class CallTuple(Call):

  def create(outdir, tc):
    grammar = tc.grammar
    c = adpc.Config
    Call._create_a(outdir, tc)
    out = tc.unique_filename() + Suffix.make + 'tup.' + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.make + 'tup.' + Suffix.err
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.make, ["BINPATH=" + c.binpath,
        "ADPCOMPILE=" + c.adpcompile,
        "ADPC=" + c.adpc_bin + " " + c.binpath,
        grammar[:-4] + '.c'], f1, f2)
    if i != 0:
      raise cg.CallError('making tupel step failed'
          + testbox.look_into([f1, f2]))
    sed_file = grammar[:-4] + '_' + CallTuple.algebra + '.sed'
    tmp = sed_file + '.tmp'
    pat = re.compile('result_score')
    pat_head = re.compile('int maxloop;\\\\')
    shutil.move(sed_file, tmp)
    f = open(tmp)
    g = open(sed_file, 'w')
    for i in f:
      if pat.search(i):
        j = pat.sub('result_score.tup1', i)
      elif pat_head.match(i):
        j = i
        g.write('#undef is_suboptimal\\\n#define is_suboptimal(a, b, c) abs(a.tup1 - b) <= c\\\n')
      else:
        j = i
      g.write(j)
    f.close()
    g.close()
    os.unlink(tmp)
    Call._create_b(outdir, tc)
  create = staticmethod(create)


