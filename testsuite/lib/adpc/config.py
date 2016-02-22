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

import os
from logging import info

import sysext

class Config:
  binpath = os.path.abspath('../interfacer')
  adpc = "adpc"
  adpc_bin = os.path.abspath("../interfacer/adpc-bin")
  adpcompile = os.path.abspath('../adpcompile')
  hand = os.path.abspath("../hand")
  make = sysext.which("make")
  cwd = os.getcwd()
  state = cwd + os.path.sep + "state"
  grammars = cwd + os.path.sep + "grammars"
  output = cwd + os.path.sep + "output"
  input = cwd + os.path.sep + "input"
  workdir = cwd + os.path.sep + "workdir"

  java = sysext.which("java")
  javac = sysext.which("javac")
  fe = os.path.abspath("../java/fe")
  fe_path = os.path.abspath('../java')
  classpath = '.:../../java'

  # don't check for these, because perf tests are not the standard
  memtime = os.path.abspath('../../memtime/memtime-1.3/memtime')
  randseq = os.path.abspath('../addons/randseq')
  perf_input = os.path.join(cwd, 'perf/in')
  perf_output = os.path.join(cwd, 'perf/out')


  def set_opts(opts):
    for i in opts:
      a = i.split('=')
      if hasattr(Config, a[0]):
        setattr(Config, a[0], a[1])
  set_opts = staticmethod(set_opts)

  def log():
    info('ADPC config:')
    for i in dir(Config):
      if '_' not in i:
        info('  Name: ' + i + ' Value: ' + str(getattr(Config, i)))
  log = staticmethod(log)



class ConfigException(Exception):
  pass

for i in [ Config.make, Config.java, Config.javac ]:
  if Config.make == None:
    raise ConfigException('Could not find ' + i + ' in $PATH.')

for i in [ Config.adpc_bin, Config.adpcompile, Config.hand, Config.fe ]:
  if not os.path.exists(i):
    raise ConfigException('Could not find the ' + i + ' executable.\n' +
      '  Probably you have to run \'make\' in the corresponding dirs.')

for i in [ Config.grammars, Config.state, Config.output, Config.workdir,
    Config.input ]:
  if not os.path.exists(i):
    raise ConfigException('Could not find the ' + i + ' path.\n' +
      '  Use \'mkdir\' or change lib/adpc/config.py.')

class Suffix:
  err = '.err'
  out = '.out'

  i = '.i'
  adp = '.adpc'
  java = '.javac'
  mf = '.mf'
  make = '.make'
  pickle = '.pickle'
  clean = '.clean'
