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

from adpc import Suffix
import testbox

class Call(cg.Call):


  default_opts = [ '-d', '10' ]
  default_opts_rna = ['-c', '10']


  def create(outdir, tc):
    grammar = tc.grammar
    c = adpc.Config
    os.chdir(c.workdir)
    shutil.copy(os.path.join(c.grammars, grammar), c.workdir)

    os.mkdir(grammar[:-4])

    out = tc.unique_filename() + Suffix.mf + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.mf + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.fe,
        [ grammar, '-p', c.fe_path, '--pure-prefix' ],
        f1, f2)
    if i != 0:
      raise cg.CallError('Metafrontend failed'
          + testbox.look_into([f1, f2]))
    out = tc.unique_filename() + Suffix.java + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.java + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sysext.sys(c.javac,
        [ '-cp', c.classpath,
          grammar[:-4] + os.path.sep + grammar[:-4] + '.java' ],
        f1, f2)
    if i != 0:
      raise cg.CallError('Java Compiler failed'
          + testbox.look_into([f1, f2]))

  create = staticmethod(create)

  def clean(outdir, tc):
    grammar = tc.grammar
    c = adpc.Config
    os.chdir(c.workdir)

    for i in os.listdir(grammar[:-4]):
      os.unlink(grammar[:-4] + os.path.sep + i)
    os.rmdir(grammar[:-4])
    os.unlink(grammar)
    ls = os.listdir(c.workdir)
    if not (ls == [] or ls == ['.svn'] or ls == ['.hg']):
      raise cg.CallError("not everything is clean" + 
          testbox.look_into([c.workdir]))

  clean = staticmethod(clean)

  def execute(outdir, progname, filename, tc, sys_fn = sysext.sys):
    c = adpc.Config
    os.chdir(c.workdir)

    para = []
    if tc.flavour == 'rna' or tc.flavour == 'window':
      para += Call.default_opts_rna
    else:
      para += Call.default_opts
    if hasattr(tc, 'parameter'):
      para += tc.parameter.split()

    out = tc.unique_filename() + Suffix.out
    testbox.checkFilename(out)
    err = tc.unique_filename() + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(outdir, out)
    f2 = os.path.join(outdir, err)
    i = sys_fn(c.java,
        [ '-cp', c.classpath,
          progname + '.' + progname, "-f", os.path.join(c.input, filename)]
        + tc.default_parameter + para,
        f1, f2)
    if i != 0:
      raise cg.CallError("error executing adp program (exit status: " + str(i) + ")"
        + testbox.look_into([f1, f2]))

  execute = staticmethod(execute)


