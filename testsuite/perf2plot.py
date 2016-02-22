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

import sys
import os
import re

pat_exit = re.compile('^Exit\[[^0][0-9]*\]')
pat_data = re.compile('^([0-9.]+) user, ([0-9.]+) system, ([0-9.]+) elapsed -- Max VSize = ([0-9]+)KB, Max RSS = ([0-9]+)KB')
pat_normal = re.compile('repeat=[0-9]+\.err$')


src = './perf/out'

class Data:
  def __init__(self, cats):
    self.atts = dict(cats)

  def set(self, a):
    self.atts['utime'] = a[0]

    self.atts['system'] = a[1]
    self.atts['elapsed'] = a[2]
    self.atts['vsize'] = a[3]

    self.atts['mem'] = a[4]

def get_cats(s):
  r = list()
  for i in s:
    if '=' in i:
      r.append(i.split('='))
  return r

def read_data():
  os.chdir(src)
  table = dict()
  for i in os.listdir('.'):
    match = pat_normal.search(i)
    if not match:
      continue
    f = open(i)
    l = i.split('.')
    name = l[0]
    cats = get_cats(l)
    jump = False
    obj = Data(cats)
    found = False
    for j in f:
      match = pat_exit.search(j)
      if match:
        jump = True
        break
      match = pat_data.search(j)
      if match:
        obj.set(match.groups())
        found = True
        break
    if not found:
      raise Exception('Did not found memtime pattern in ' + i)
    f.close()
    if jump:
      continue
    if not name in table:
      table[name] = [obj]
    else:
      table[name].append(obj)
    #for (a, b) in cats:
    #  table['name', i] = obj
  return table

def plot(table, ps = [('name','fold'), ( 'x', 'n'), ('y', 'utime')], add_ps = [], fil = [('language', 'java'), ('subopt', '0')], add_fil = []):
  ps = dict(ps + add_ps)
  fil = dict(fil + add_fil)
  sys.stderr.write('Params: ' + str(ps) + '\n')
  sys.stderr.write('Filters: ' + str(fil) + '\n')
  r = list()
  for a in ps['name'].split(','):
    l = table[a]
    for i in l:
      cont = False
      for j in fil:
        if i.atts[j] != fil[j]:
          cont = True
          break
      if cont:
        continue
      r.append(i.atts[ps['x']] + ' ' + i.atts[ps['y']])
  return r

def delim():
  print '\n'

def output(r):
  for i in r:
    print i
  delim()

def print_subopt(ps = [], fs = []):
  table = read_data()
  for i in range(0,20,5):
    r = plot(table, add_ps = ps, add_fil = [('subopt', str(i))] + fs)
    output(r)

def print_lang(ps = [], fs = []):
  table = read_data()
  for i in [ 'java', 'C' ]:
    r = plot(table, add_ps = ps, add_fil = fs + [ ('language', i)])
    output(r)

def main(argv):
  fs = []
  ps = []
  for i in argv:
    if i == '-s':
      print_subopt(ps, fs)
    elif i == '-l':
      print_lang(ps, fs)
    elif i == '-h':
      print 'perf2plot [-s|-l] [-fn1:v1...nn:vn] [-pn1:v1...nn:vn] [-idir]\n' \
          + '(subopts, languages, filters, parameters, input dir)'
    elif i[:2] == '-f':
      fs = fs + get_cats(i[2:].split('.'))
    elif i[:2] == '-p':
      ps = ps + get_cats(i[2:].split('.'))
    elif i[:2] == '-i':
      global src
      src = i[2:]

if __name__ == '__main__':
  sys.exit(main(sys.argv))


