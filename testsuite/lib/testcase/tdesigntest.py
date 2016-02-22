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
import sysext

import os
import re

from adpc import Suffix
import testbox

class DesignError(Exception):
  pass

class BaseDesign(adpc.GrammarTest):
  def __init__(self, foo):
    adpc.GrammarTest.__init__(self, foo)

  def design(self, dest):
    os.chdir(self.config.cwd)
    out = self.unique_filename() + Suffix.out
    testbox.checkFilename(out)
    err = self.unique_filename() + Suffix.err
    testbox.checkFilename(err)
    f1 = os.path.join(dest, out)
    f2 = os.path.join(dest, err)
    i = sysext.sys(self.config.hand,
        self.hand_opts + [ os.path.join(self.config.state,
          self.grammar + Suffix.i + Suffix.out) ],
        f1, f2)
    self.failIf(i != 0, "Couldn't generate hand output"
        + testbox.look_into([f1, f2]))

  def test(self):
    """Table Design"""
    self.design(self.config.output)
    try:
      (a, f1) = self.extract(self.config.state)
      (b, f2)  = self.extract(self.config.output)
    except DesignError, msg:
      self.fail(str(msg))
    look = testbox.look_into([os.path.join(self.config.state, f1),
      os.path.join(self.config.output, f2)])
    try:
      foo = (a == b)
    except DesignError, msg:
      self.fail(str(msg) + look)

  def generate(self):
    """generate state"""
    self.design(self.config.state)


class ExactData:
  #tab_all = ""
  #tab_non = ""
  #asm_rt = ""
  #nt_count = ""
  #red_count = ""
  #red_list = [""]
  #opt_asm_rt = ""
  #tab_conf_count = ""
  # this static if no assignment later and only append ...
  #opt_confs = [ ([""], "") ]

  def out(self):
    print "tab_all: ", self.tab_all
    print "tab_non: ", self.tab_non
    print "asm_rt: ", self.asm_rt
    print "nt_count: ", self.nt_count
    print "red_count: ", self.red_count
    print "red_list: ", self.red_list
    print "opt_asm_rt: ", self.opt_asm_rt
    print "tab_conf_count: ", self.tab_conf_count
    print "opt_confs: ", self.opt_confs

  def __eq__(self, other):
    #self.out()
    #other.out()

    if self.tab_all != other.tab_all:
      raise DesignError("differtent asymptotic runtime under the full configuration:\n"
          + '  ' + self.tab_all + ' vs. ' + other.tab_all)
    if self.tab_non != other.tab_non:
      raise DesignError("different asymptotic runtime under the empty configuration:\n"
          + '  ' + self.tab_non + ' vs. ' + other.tab_non)
    if self.asm_rt != other.asm_rt:
      raise DesignError("different asymptotic runtime preprocessed:\n"
          + '  ' + self.asm_rt + ' vs. ' + other.asm_rt)
    if self.nt_count != other.nt_count:
      raise DesignError("different number of Non-Terminals:\n"
          + '  ' + self.nt_count + ' vs. ' + other.nt_count)
    if self.red_count != other.red_count:
      raise DesignError("different number of reductions is computed:\n"
          + '  ' + self.red_count + ' vs. ' + other.red_count)
    if self.red_list != other.red_list:
      raise DesignError("elements from the reduction set are different:\n"
          + '  ' + str(self.red_list) + ' vs. ' + str(other.red_list))
    if self.opt_asm_rt != other.opt_asm_rt:
      raise DesignError("computed asymptotic rt is different:\n"
          + '  ' + self.opt_asm_rt + ' vs. ' + other.opt_asm_rt)
    if self.tab_conf_count != other.tab_conf_count:
      raise DesignError("number of table configurations is different:\n"
          + '  ' + self.tab_conf_count + ' vs. ' + other.tab_conf_count)
    l = zip (self.opt_confs, other.opt_confs)
    c = 0
    for (i, j) in l:
      if i != j:
        raise DesignError(str(c) + ". optimal configuration is diffent - exact rt or tables\n:"
            + '  ' + str(i[0]) + ' vs. ' + str(j[0]) + '\n'
            + '  ' + i[1] + ' vs. ' + j[1])
      c += 1

    return True

  def __ne__(self, other):
    return not self.__eq__(other)


class TDesignTest(BaseDesign):
  def __init__(self, foo):
    BaseDesign.__init__(self, foo)
    self.hand_opts = []



  pat_o = '[ \t]+: O\(([n^0-9]+)\)'
  pat_n = '[ \t]+: ([0-9]+)'
  pat_some = re.compile('^Some information')
  pat_tab_all = re.compile('tabulate all nonterminals' + pat_o)
  pat_tab_non = re.compile('tabulate no  nonterminals' + pat_o)
  pat_asm_rt = re.compile('optimal runtime' + pat_o)
  pat_nt_count = re.compile('^Total number of nonterminals' + pat_n)
  pat_red_count = re.compile('^Nonterminals defined by reductions' + pat_n)
  pat_set = re.compile('{([ 0-9l]+)}')
  pat_opt_asm_rt = re.compile('^Optimal runtime' + pat_o)
  pat_tab_conf_count = re.compile('^Optimal table configurations' + pat_n)
  pat_tab_conf = re.compile('{([ 0-9l]+)} +\(([n^0-9 +]+)\)')
  pat_wrt = re.compile('with respect to')


  def extract(self, dir):
    os.chdir(dir)
    filename = self.unique_filename() + Suffix.out
    f = open(filename)
    r = ExactData()
    state = 0
    for l in f:
      if state == 0:
        match = self.pat_some.search(l)
        if match:
          state = 1
      elif state == 1:
        match = self.pat_tab_all.search(l)
        if match:
          r.tab_all = match.group(1)
        else:
          match = self.pat_tab_non.search(l)
          if match:
            r.tab_non = match.group(1)
          else:
            match = self.pat_asm_rt.search(l)
            if match:
              r.asm_rt = match.group(1)
              state = 2
      elif state == 2:
        match = self.pat_nt_count.search(l)
        if match:
          r.nt_count = match.group(1)
        match = self.pat_red_count.search(l)
        if match:
          r.red_count = match.group(1)
          state = 3
      elif state == 3:
        match = self.pat_set.search(l)
        if match:
          r.red_list = match.group(1).split()
          state = 4
      elif state == 4:
        match = self.pat_opt_asm_rt.search(l)
        if match:
          r.opt_asm_rt = match.group(1)
        else:
          match = self.pat_tab_conf_count.search(l)
          if match:
            r.tab_conf_count = match.group(1)
            state = 5
            # WTF!?! - without this - opt_conf gets a static member
            r.opt_confs = []
      elif state == 5:
        match = self.pat_wrt.search(l)
        if match:
          state = 6
        else:
          match = self.pat_tab_conf.search(l)
          if match:
            r.opt_confs.append( (match.group(1).split(), match.group(2) ) )

    f.close()
    return (r, filename)
            
    

class GraspData:
  # asym_rt, int
  # tables, list of str
  # number, int

  def __eq__(self, other):
    if self.asym_rt != other.asym_rt:
      raise DesignError('Asymptotical runtime of optimal table configuration differs:\n'
          + '  n^' + str(self.asym_rt) + ' vs. n^' + str(other.asym_rt))

    if self.number != other.number:
      raise DesignError('Number of Tables differs:\n'
          + '  ' + str(self.number) + ' vs. ' + str(other.number) + '\n'
          + '  ' + str(self.tables) + ' vs. ' + str(other.tables))
    return True

  def __ne__(self, other):
    return not self.__eq__(other)



class GraspTest(BaseDesign):
  def __init__(self, foo):
    BaseDesign.__init__(self, foo)
    self.hand_opts = [ '-m', '1' ]


  def extract(self, dir):
    pat_limiter = re.compile('^\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+\\+')
    pat_set = re.compile('{([^}]+)} #([0-9]+)')
    pat_rt = re.compile('^Iteration:.+Runtime: ([ 0-9n^+]+)')
    pat_rt_last = re.compile('^Iteration:.+Runtime: .+n\^([0-9]+)$')
    pat_rt_lin = re.compile('^Iteration:.+Runtime: +[0-9]+n$')

    os.chdir(dir)
    filename = self.unique_filename() + Suffix.out
    m = ' (' + filename + ')'
    f = open(filename)
    r = GraspData()
    state = 0
    for i in f:
      if state == 0:
        match = pat_limiter.search(i)
        if match:
          state = 1
      elif state == 1:
        match = pat_rt_last.search(i)
        if match:
          r.asym_rt = int (match.group(1))
          state = 2
        else:
          match = pat_rt_lin.search(i)
          if match:
            r.asym_rt = 1
            state = 2
          else:
            raise DesignError('Cannot find Iteration signature' + m)
      elif state == 2:
        match = pat_set.search(i)
        if match:
          r.tables = match.group(1).split()
          r.number = int (match.group(2))
          if r.number != len(r.tables):
            raise DesignError('number of tables differs:\n'
                + i + '  ' + m)
          state = 3
        else:
          raise DesignError('Cannot find set signature' + m)
      elif state == 3:
        match = pat_limiter.search(i)
        if match:
          state = 0
        else:
          raise DesignError('Missing end signature' + m)
    f.close
    return (r, filename)


  def test(self):
    """Grasp Table Design"""
    BaseDesign.test(self)


