#include <stdio.h>
#include <stdlib.h>

#include "index_list.h"

struct int_head {
  struct list_head head;
  int n;
  int i;
};

int main() {
  int i;

  struct list_head **il;
  struct list_head *itr;
  struct int_head *el;

  il = index_list_init(100);

  for (i=1; i<100+1; i++) {
    if (i % 5 == 0) {
      el = malloc(sizeof(struct int_head));
      el->i = i*i;
      el->n = i;
      index_list_add(il, i, &(el->head));
    }
  }
  for (i=5; i<100+1; i+=10)
    index_list_remove(il, i);

  itr = il[0]->next;
  while (itr != il[0]) {
    el = (struct int_head*) itr;
    printf("%d -> %d, ", el->n, el->i);
    itr = itr->next;
  }
  index_list_free(il);
  printf("\n");
  return 0;
}
