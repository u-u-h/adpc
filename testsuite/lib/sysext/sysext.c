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

#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>

#include <signal.h>
// Solaris does not define this
#ifndef SA_NOMASK
#define SA_NOMASK 0
#endif

#include <stdlib.h>
#include <stdio.h>

#include "sysext.h"

#define exit_print(A, B) if ((A) == -1) { perror("after fork: " B); exit(77); }

static int time_over;

static char **argv_addstr(char *filename, char **argv)
{
  int j, i=0;
  char **r;
  while (argv[i])
    i++;

  r = malloc((i+2) * sizeof(char*));
  if (!r)
    return NULL;
  r[0] = filename;
  for (j=0; j<i; j++)
    r[j+1] = argv[j];
  r[j+1] = 0;
  return r;
}

static int waitpid_exit(pid_t r)
{
  int status;
  do {
    pid_t t = waitpid(r, &status, 0);
    if (t == -1)
      if (time_over)
        return -2;
      else
        return -1;
  } while (WIFSTOPPED(status));
  if (WIFEXITED(status))
    return WEXITSTATUS(status);
  return -3;
}

static void timeout(int i)
{
  time_over = 1; 
}

static int do_parent(int r, int time)
{
  int ret;
  struct sigaction sa = {}, sa_old = {};
  if (time) {
    time_over = 0;
    sa.sa_handler = timeout;
    sa.sa_flags = SA_NOMASK;
    signal(SIGALRM, timeout);
    sigaction(SIGALRM, &sa, &sa_old);
    alarm(time);
  }
  ret = waitpid_exit(r);
  if (time) {
    if (time_over)
      kill(r, SIGTERM);
    alarm(0);
    signal(SIGALRM, SIG_DFL);
    sigaction(SIGALRM, &sa_old, NULL);
  }
  return ret;
}

/* see open FIXME */
int sys(char *filename, char **av, char *sout, char *serr, int time)
{
  pid_t r;
  r = vfork();
  if (r == -1)
    return -1;
  if (r)
    return do_parent(r, time);
  else {
    char **argv;
    argv = argv_addstr(filename, av);
    if (!argv)
      exit(88);
    if (sout) {
      exit_print(close(1), "Closing stdout error");
      /* FIXME what about O_LARGEFILE?!? */
      exit_print(open(sout, O_WRONLY|O_CREAT|O_TRUNC, 0666), "Opening stdout");
    }
    if (serr) {
      exit_print(close(2), "Closing stderr");
      exit_print(open(serr, O_WRONLY|O_CREAT|O_TRUNC, 0666), "Opening stderr");
    }
    exit_print(execv(filename, argv), "Execing");
    return 0;
  }
}

