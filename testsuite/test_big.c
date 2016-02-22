#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
  char *buffer;

  size_t s = 4 * 1024 * 1024;

  buffer = malloc(s);
  buffer[s-1] = 1;
  buffer[0] = 1;
  buffer[s/2] = 1;

  printf("Test output: %d\n", buffer[1]);
  sleep(atoi(argv[1]));
  return 0;
}
