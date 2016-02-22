#include <stdio.h>
#include <stdlib.h>

#include <sysext.h>

int main(int argc, char **argv)
{
  int r;
  r = sys(argv[1], argv+5, argv[2], argv[3], atoi(argv[4]));
  fprintf(stderr, "Return status: %d\n", r);
  return r;
}
