/*
------------------------------------------------------------------------------
-- The ADP Compiler 
-- Copyright (C) 2001-2008 Peter Steffen, Christian Lang, Marco Ruether, 
--                         Georg Sauthoff, Stefanie Schirmer
--
-- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
-- Updates: http://bibiserv.techfak.uni-bielefeld.de/adp/adpcomp.html
------------------------------------------------------------------------------

This file is part of ADPC (The ADP Compiler).

ADPC is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

ADPC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ADPC.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <Python.h>

#include "sysext.h"

static int init_obj(PyObject *o, char **s)
{
  if (!(PyString_Check(o) && (*s = PyString_AsString(o))))
    if (PyTuple_Check(o) && PyTuple_Size(o) == 0)
      *s = NULL;
    else
      return 1;
  return 0;
}

/* Why do we do this?
 *
 *   - nicer API
 *   - known system() problems:
 *     - security implications because of shell meta-Chars
 *     - signal problems (e.g. Ctrl+C is always consumed by the child!)
 *     - /bin/sh bloat
 *     - temptation to use PATH-feature and get undesired results
 *
 */
static PyObject * py_sys(PyObject *self, PyObject *foo)
{
  char **argv;
  int i, argc, r;
  PyObject *args, *o1, *o2;
  char *sout, *serr, *fname;
  int timeout = 0;

  if (!PyArg_ParseTuple(foo, "etOOOi", Py_FileSystemDefaultEncoding,
        &fname, &args, &o1, &o2, &timeout))
      return NULL;
  if (init_obj(o1, &sout) || init_obj(o2, &serr))
    return NULL;

  if (PyList_Check(args))
    argc = PyList_Size(args);
  else {
    PyErr_SetString(PyExc_TypeError, "sys() arg2 must be a list");
    return NULL;
  }
  argv = malloc((argc+1) * sizeof(char*));
  if (!argv)
    return NULL;
  for (i = 0; i < argc; i++)
    if (!PyArg_Parse(PyList_GetItem(args, i), "et",
          Py_FileSystemDefaultEncoding, argv + i)) {
      PyErr_SetString(PyExc_TypeError, "Only a List of strings is valid.");
      return NULL;
    }
  argv[argc] = NULL;

  r = sys(fname, argv, sout, serr, timeout);
  //PyMem_Free(sout);
  //PyMem_Free(serr);
  PyMem_Free(fname);
  for (i=0; i<argc; i++)
    PyMem_Free(argv[i]);
  free(argv);
  return PyInt_FromLong((long) r);

}

static PyMethodDef sysext_methods[] = {
  {"sys",  py_sys, METH_VARARGS, "Execute a process. Use () to disable redirection."},
  {NULL, NULL, 0, NULL}
};


//PyMODINIT_FUNC initsysext(void)
PyMODINIT_FUNC initsysext(void)
{
  Py_InitModule("sysext", sysext_methods);
}


/*
int
main(int argc, char *argv[])
{
  // Pass argv[0] to the Python interpreter 
  Py_SetProgramName(argv[0]);

  // Initialize the Python interpreter.  Required. 
  Py_Initialize();

  // Add a static module 
  initsysext();

  return 0;
}

*/
