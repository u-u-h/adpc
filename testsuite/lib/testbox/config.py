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

import re
from logging import debug

class Content:

  def __init__(self,n='', v=''):
    self.name = n
    self.value = v

  def __cmp__(self, a):
    return cmp(self.name, a.name)

class Node:

  def __init__(self):
    self.childs = []

  def addChild(self, o):
    self.childs.append(o)

  def children(self):
    return self.childs

class Branch(Content):
  # attrs
  # specials
  def __init__(self, n='', v=''):
    Content.__init__(self, n, v)
    self.attrs = Node() # of Branch
    self.specials = Node() # of Content


class TestSuite(Node, Content):
  
  def __init__(self, n, v):
    Node.__init__(self)
    Content.__init__(self, n, v)
  
  pass # of Klasse

class Klasse(Branch):

  pass # of Branch, Content


class Error(Exception):
  # python does not have non-static Java like inner classes ... - since that x
  def __init__(self, x, message, c = True):
    self.value = message + ' at ' + x.filename
                   
    if c:
      self.value += ':' + str(x.nr) + ', context: \n' + x.raw_line

  def __str__(self):
    return self.value

class ParseError(Error):
  pass

class LexError(Error):
  pass


class Parser:


  def __init__(self, filename=''):
    self.filename = filename
    if not filename == '':
      self.f = open(filename)
    self.lex_init()
    

  def parse_var(self):
    s = self.lex()
    if s[0] == self.VAR:
      self.symbol_queue.pop(0)
      var = Branch('$', s[1][0])
      return (True, var)
    else:
      return (False, None)

  def parse_spec(self):
    s = self.lex()
    if s[0] == self.SPECIAL:
      self.symbol_queue.pop(0)
      spec = Content(s[1][0], s[1][1])
      return (True, spec)
    else:
      return (False, None)

  def parse_branch(self, branch):
    s =  self.lex()
    if s[0] == self.BEGIN:
      self.symbol_queue.pop(0)
      while 1:
        ret = self.parse_var()
        if ret[0]:
          branch.specials.addChild(ret[1])
        else:
          ret = self.parse_spec()
          if ret[0]:
            branch.specials.addChild(ret[1])
          else:
            ret = self.parse_attr()
            if ret[0]:
              branch.attrs.addChild(ret[1])
            else:
              s = self.lex()
              if s[0] == self.END:
                self.symbol_queue.pop(0)
                break
              else:
                raise ParseError(self, 'no branch end')
    else:
      return False

  
  def parse_attr(self):
    s = self.lex()
    if s[0] == self.ATTR:
      self.symbol_queue.pop(0)
      attr = Branch(s[1][0], s[1][1])
      self.parse_branch(attr)
      return (True, attr)
    else:
      return (False, None)

  def parse_class(self):
    s = self.lex()
    if s[0] == self.KLASSE:
      self.symbol_queue.pop(0)
      klasse = Klasse(s[1][0], s[1][1])
      if self.parse_branch(klasse):
        raise ParseError(self, 'no class begin')
      return (True, klasse)
    else:
      return (False, None)
  
  def parse_testsuite(self):
    s = self.lex()
    if s[0] == self.TESTSUITE:
      self.symbol_queue.pop(0)
      tsuite = TestSuite(s[1][0], s[1][1])
      s = self.lex()
      if s[0] == self.BEGIN:
        self.symbol_queue.pop(0)
        while(1):
          ret = self.parse_class()
          if ret[0]:
            tsuite.addChild(ret[1])
          else:
            ret = self.parse_var()
            if ret[0]:
              tsuite.addChild(ret[1])
            else:
              s = self.lex()
              if s[0] == self.END:
                self.symbol_queue.pop(0)
                break
              else:
                raise ParseError(self, 'no testsuite end')
      else:
        raise ParseError(self, 'no testsuite begin')
      return (True, tsuite)
    else:
      return (False, None)

  def parse_def(self):
    s = self.lex()
    if s[0] == self.DEFI:
      self.symbol_queue.pop(0)
      defi = Branch(s[1][0])
      t = self.lex()
      if t[0] == self.BEGIN:
        self.symbol_queue.pop(0)
        ret = self.parse_class()
        if ret[0]:
          defi = TestSuite(s[1][0], '')
          defi.childs.append(ret[1])
          u = self.lex()
          if u[0] == self.END:
            self.symbol_queue.pop(0)
          else:
            raise ParseError(self, 'no def end')
        else:
          self.symbol_queue.insert(0, t)
          if self.parse_branch(defi):
            raise ParseError(self, 'no def begin')
      else:
        raise ParseError(self, 'no def begin')
      return (True, defi)
    else:
      return (False, None)

  def parse_default(self):
    s = self.lex()
    if s[0] == self.DEFAULT:
      self.symbol_queue.pop(0)
      default = s[1][0]
      return (True, default)
    else:
      return (False, None)

  def insert_branch(self, br, defs):
    if hasattr(br, 'visited'):
      if br.visited == True:
        raise ParseError(self, 'Detected Cycle in testsuite: ' + self.ts_name, False)
    else:
      br.visited = True

    seen = []
    changed = True
    while changed:
      changed = False
      new_children = []
      for i in br.specials.childs:
        n = i.name
        v = i.value
        if n == '$':
          if v in seen:
            raise ParseError(self, 'Detected Cycle in testsuite: ' + self.ts_name,
                False)
          seen.append(v)
          if v in defs.keys():
            for j in defs[v].attrs.childs:
              br.attrs.childs.append(j)
            for j in defs[v].specials.childs:
              new_children.append(j)
          else:
            raise ParseError(self, 'Variable ' + v + ' is not defined.', False)
          changed = True
        else:
          new_children.append(i)
        br.specials.childs = new_children

    for i in br.attrs.children():
      self.insert_branch(i, defs)

    br.visited = False
    return True
        
  
  def insert_defs(self, ts, defs):
    for i in ts:
      self.ts_name = i.name

      seen  = []
      changed = True
      while changed:
        changed = False
        new_children = []
        for j in i.childs:
          n = j.name
          v = j.value
          if n == '$':
            if v in seen:
              raise ParseError(self, 'Detected Cycle in testsuite: ' + self.ts_name,
                 False)
            seen.append(v)
            if v in defs.keys():
              for k in defs[v].childs:
                new_children.append(k)
            else:
              raise ParseError(self, 'Variable ' + v + ' is not defined.', False)
            changed = True
          else:
            new_children.append(j)
            self.insert_branch(j, defs)
        i.childs = new_children

  def parse(self, filename=''):
    if not filename=='':
      self.f = open(filename)
      lex_init()

    testsuites = []
    defs = dict()
    defaults = []
    s = self.lex()
    if s[0] == self.BEGIN:
      self.symbol_queue.pop(0)
      while 1:
        ret = self.parse_testsuite()
        if ret[0]:
          testsuites.append(ret[1])
        else:
          ret = self.parse_def()
          if ret[0]:
            defs[ret[1].name] = ret[1]
          else:
            ret = self.parse_default()
            if ret[0]:
              defaults.append(ret[1])
            else:
              s = self.lex()
              if s[0] == self.END:
                self.symbol_queue.pop(0)
                break
              else:
                raise ParseError(self, 'Missing end at top level')
    else:
      raise ParseError(self, 'No begin at head')
    self.insert_defs(testsuites, defs)
    self.f.close()
    return (testsuites, defs, defaults)

  
  # ind_stack
  # symbol_queue
  # nr
  # line
  # raw_line
  # f
  BEGIN = 0
  END = 1
  EOF = 2
  TESTSUITE = 3
  KLASSE = 4
  DEFI = 5
  DEFAULT = 6
  SPECIAL = 7
  VAR = 8
  ATTR = 9
  
  
  def lex_init(self):
    self.ind_stack = [0]
    self.symbol_queue = [(self.BEGIN, None)]
    self.nr = 0
  
  pat_ind = re.compile('( *)[$@A-Za-z]')
  pat_ts = re.compile('testsuite +([^ ]+) +\'([^\']+)\'')
  pat_class = re.compile('class +([A-Za-z]+) +([A-Za-z.0-9]+)')
  pat_def = re.compile('def +([A-Za-z0-9]+)')
  pat_default = re.compile('default +(.+$)')
  pat_spec = re.compile('@([A-Za-z_]+) +(.+$)')
  pat_var = re.compile('^ +\$([A-Za-z0-9]+)')
  pat_attr = re.compile('^ +([A-Za-z_]+) +(.*$)')

  def lex(self):
    if len(self.symbol_queue) == 0:
      while 1:
        self.raw_line = self.f.readline()
        if self.raw_line == '':
          while len(self.ind_stack) > 0:
            self.symbol_queue.append((self.END, None))
            self.ind_stack.pop()
          self.symbol_queue.append((self.EOF, None))
          break
        debug('>' + self.raw_line)

        self.nr = self.nr + 1
        i = self.raw_line.find('#')
        if i != -1:
          self.line = self.raw_line[0:i]
        else:
          self.line = self.raw_line[0:-1]

        match = self.pat_ind.search(self.line)
        if not match:
          continue
        ind = len(match.group(1))
        if ind < self.ind_stack[-1]:
          t = 0
          while len(self.ind_stack) > 0:
            self.ind_stack.pop()
            
            self.symbol_queue.append((self.END, None))
            try:
              if self.ind_stack[-1] == ind:
                t = 1
                break
            except:
              raise LexError(self, "wrong identation")
          if t == 0:
            raise LexError(self, "nesting error ('+str(self.ind_stack[-1])+')")
        elif ind > self.ind_stack[-1]:
          self.ind_stack.append(ind)
          self.symbol_queue.append((self.BEGIN, None))

        match = self.pat_ts.search(self.line)
        if match:
          self.symbol_queue.append((self.TESTSUITE, match.groups()))
        else:
          match = self.pat_class.search(self.line)
          if match:
            self.symbol_queue.append((self.KLASSE, match.groups()))
          else:
            match = self.pat_def.search(self.line)
            if match:
              self.symbol_queue.append((self.DEFI, match.groups()))
            else:
              match = self.pat_default.search(self.line)
              if match:
                self.symbol_queue.append((self.DEFAULT, match.groups()))
              else:
                match = self.pat_spec.search(self.line)
                if match:
                  self.symbol_queue.append((self.SPECIAL, match.groups()))
                else:
                  match = self.pat_var.search(self.line)
                  if match:
                    self.symbol_queue.append((self.VAR, match.groups()))
                  else:
                    match = self.pat_attr.search(self.line)
                    if match:
                      self.symbol_queue.append((self.ATTR, match.groups()))
                    else:
                      raise LexError(self, 'unknown or missing symbol')

        break
    return self.symbol_queue[0]
      
      
      
