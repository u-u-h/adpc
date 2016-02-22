#include <stdio.h>
#include <stdlib.h>

#include "gray.h"

int main(int argc, char **argv)
{
  struct gray_code g, h;
  int j, state;
  uint64 a, b;

  a = strtoull(argv[1], NULL, 10);
  b = strtoull(argv[2], NULL, 10);

  gray_init(&g, a);

  gray_print(&g);
  do {
    j = gray_inc(&g, &state);
    gray_print(&g);
    if (state)
      printf("inserted %d \n", j);
    else
      printf("removed %d \n", j);
  } while (g.bin_count < b);

  return 0;
}
