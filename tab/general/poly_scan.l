%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dll.h"
#include "poly_type.h"
#include "poly_scan.h"

  int number = 1;
  static struct poly_head *el;
  int max_length = 0;
  struct list_head *head;
  static int doing = 0;

%}

%x N

%%

[0-9]+ { number = atoi(yytext); doing = 1; }
<<EOF>> {
  if (doing) {
  el = malloc(sizeof(struct poly_head));
  el->l = 0;
  el->x = number;
  list_add_tail(&(el->head), head);
  doing = 0;
  }
  /* Attention! The state of this lexer is not resetted, while calling
     lex() / yy_scan_string() a 2nd time! */
  BEGIN INITIAL;
  return 0;}
"+" { 
  doing = 0;
  el = malloc(sizeof(struct poly_head));
  el->l = 0;
  el->x = number;
  number = 1;
  list_add_tail(&(el->head), head);
}
"n^" { BEGIN N; }
. ;

<N>[0-9]+ {
  el = malloc(sizeof(struct poly_head));
  el->l = atoi(yytext);
  el->x = number;
  number = 1;
  list_add_tail(&(el->head), head);
  max_length = el->l > max_length ? el->l : max_length;
  doing = 0;
}
<N>"+" { BEGIN INITIAL; }
<N>. ;

%%

