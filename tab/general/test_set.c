#include <stdio.h>
#include "set.h"

int main() {
  struct set *a, *b, *c;
  int i;
  a = set_init(100);
  b = set_init(100);
  set_add(a, 2);
  set_add(a, 70);
  set_add(a, 3);
  set_add(a, 1);
  set_add(b, 80);
  set_add(b, 99);
  set_print(a);
  printf("|a| %d\n", a->elements);
  set_print(b);
  printf("|b| %d\n", b->elements);
  c = set_union(a, b);
  set_print(c);
  printf("|c| %d\n", c->elements);
  printf("union ip: ");
  set_union_ip(a,b);
  set_print(a);
  printf("|a| %d\n", a->elements);

  for (i=0; i<c->size; i++)
    if (set_contains(c, i))
        printf("%d ", i);
  printf("\n");

  set_remove(c, 70);
  set_print(c);
  printf("|c| %d\n", c->elements);
  return 0;
}
