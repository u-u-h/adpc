#include <stdio.h>
#include <stdlib.h>

#include "poly_type.h"
#include "poly_scan.h"
#include "dll.h"

#define POLY_SIZE 20

void init() {
  list_init((head = malloc(sizeof(struct list_head))));
  max_length = 0;
  number = 1;
}

Poly poly_create(struct list_head *h, int length) {
  struct poly_head *itr;
  Poly result;
  itr = (struct poly_head*) (h->next);
  result = calloc(POLY_SIZE + 2, sizeof(int));
  result[0] = length;
  while (itr != (struct poly_head*) h) {
    result[1 + itr->l] = itr->x;
    itr = (struct poly_head*) (itr->head.next);
  }
  return result;
}


int main(int argc, char **argv) {
  Poly p1, p2, r1, r2, r3, r4;
  yy_scan_string(argv[1]);
  init();
  yylex();
  p1 = poly_create(head, max_length);
  printf("p1\n");
  poly_print(stdout, p1);
  printf("\n");

  yy_scan_string(argv[2]);
  init();
  yylex();
  p2 = poly_create(head, max_length);
  printf("p2\n");
  poly_print(stdout, p2);
  printf("\n");

  r1 = poly_copy(p1, POLY_SIZE);
  printf("copy p1\n");
  poly_print(stdout, r1);
  printf("\n");

  poly_add(r1, p2);
  printf("p1 + p2\n");
  poly_print(stdout, r1);
  printf("\n");
  r2 = poly_times(p1, p2, POLY_SIZE);
  printf("p1 * p1\n");
  poly_print(stdout, r2);
  printf("\n");
  r2 = poly_copy(p2, POLY_SIZE);
  printf("copy p2\n");
  poly_print(stdout, r2);
  printf("\n");
  poly_div(r2, p1, POLY_SIZE);
  printf("p2 / p1\n");
  poly_print(stdout, r2);
  printf("\n");
  printf("exp\n");
  poly_print(stdout, poly_exp());
  printf("\n");

  printf("n^2\n");
  poly_print(stdout, poly_n(2, POLY_SIZE));
  printf("\n");
/*
  printf("exp / n^10\n");
  poly_print(stdout, poly_div(poly_exp(), poly_n(10, POLY_SIZE), POLY_SIZE));
  printf("\n");
*/
  printf("1*n^0\n");
  poly_print(stdout, poly_int(1, POLY_SIZE));
  printf("\n");

  printf("degree(n^3)\n");
  printf("%d degree", poly_degree(poly_n(3, POLY_SIZE)));
  printf("\n");

  printf("compare(p1, p2)\n");
  printf("%d \n", poly_compare(p1, p2));
  return 0;
}
