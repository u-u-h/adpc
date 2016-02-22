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
import re
import optparse
from logging import error, debug
import logging
import sys

import adpc
import unit
import loader

class TestCollection:

  loader = loader.TestLoader()

  def run(self):
    return unit.TextTestRunner(verbosity=2).runTests(self.run_suites)


  def generate(self):
    return unit.TextTestRunner(verbosity=2).runTests(self.gen_suites)

  def cleanDir(self, dir):
    pat = re.compile('\.(log|i|td)$')
    l = os.listdir(dir)
    for i in l:
      match = pat.search(i)
      if match:
        f = dir + os.path.sep + i
        print "removing: ", f
        os.remove(f)

  def clean(self):
    self.cleanDir(adpc.Config.output)

  def cleanState(self):
    c = adpc.Config
    self.cleanDir(adpc.Config.state)


  def setSuites(self, args, r):
    self.run_suites = []
    self.gen_suites = []
    defs = []
    for i in r[0]:
      j = i.split(' ')
      try:
        while j.remove(''):
          pass
      except ValueError:
        pass
      defs = defs + j
    self.defaults = defs
    run = r[1]
    gen = r[2]
    self.ts_names = r[1].keys()
    if args == []:
      args = ['all']
    if 'all' in args:
      if not defs == []:
        args = args + defs
      else:
        args = args + r[1].keys()
      args.remove('all')
    for i in args:
      try:
        self.run_suites.append(run[i])
        self.gen_suites.append(gen[i])
      except:
        error('Testsuite '+ i + ' does not exists.')
        return False
    return True

  def list(self):
    print 'All available testsuites: '
    print self.ts_names
    print 'Default set: '
    print self.defaults

def init_options():
  parser = optparse.OptionParser('%prog [:options:]* (:testsuite:|all)*',
      description='TestSuite for the ADP-Compiler. \'all\' is the default argument and is substituted with the testuite names specified via the default config directive if it is found or every available testsuite name.',
      version='$Id: $')
  parser.add_option('-g', '--generate', action='store_true', dest='generate',
    help='records the current state (used as reference in tests')
  parser.add_option('-v', '--verbose', action='count', dest='verbosity',
    help='enables debug output - use it more times, to be even more verbose')
  parser.add_option('-c', '--clean', action='store_true', dest='clean',
    help='removes intermediate (log) files')
  parser.add_option('-s', '--clean-state', action='store_true',
      dest='clean_state', help='removes the recorded state')
  parser.add_option('-f', '--file', dest='filename',
      help='Testsuite config FILE (default: test.conf)', metavar='FILE')
  parser.add_option('-l', '--list', action='store_true', dest='list',
      help='Lists all available testsuites')
  parser.add_option('-o', '--options opt1=val1.opt2=val2...', dest='opts',
      action='append', default = [],
      help='Set attributes of the config class')
  parser.add_option('-t', '--log-file', dest='log_filename',
      help='log file name', default='test.log')
  
  return parser
    

def main(argv):
  tc = TestCollection()

  parser = init_options()
  (options, args) = parser.parse_args()

  logging.basicConfig(level = logging.INFO,
      format='%(levelname)-8s %(message)s',
      datefmt='%Y-%M-%d %H:%M:%S',
      filename = options.log_filename, filemode = 'w')
    #%(asctime)s 
  
  adpc.Config.set_opts(options.opts)
  adpc.Config.log()

  if options.verbosity == None:
    options.verbosity = 0
  handler = logging.StreamHandler()
  format = logging.Formatter('%(levelname)-8s %(message)s')
  handler.setFormatter(format)
  # info, debug
  handler.setLevel(logging.WARNING - options.verbosity * 10)
  logging.getLogger().addHandler(handler)

  debug('Args:')
  debug(args)
  debug('Options: ')
  debug(options)
 
  filename = 'test.conf'
  if options.filename:
    filename = options.filename
  r = tc.loader.loadTestCollection(filename)

  if not tc.setSuites(args, r):
    return False
  
  if options.generate:
    return tc.generate()
  elif options.clean:
    tc.clean()
  elif options.clean_state:
    tc.cleanState()
  elif options.list:
    tc.list()
  else:
    return tc.run()

if __name__ == '__main__':
  r = main(sys.argv)
  if not r:
    sys.exit(1)

