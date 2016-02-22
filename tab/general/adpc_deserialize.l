%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "../../tabulate.h"
#include "dll.h"

struct int_list {
  struct list_head head;
  int i;
};

static int i;
static int j = 0;
static struct list_head *head;
static struct int_list *el;

%}

%%

[0-9]+ { i = atoi(yytext);
         el = malloc(sizeof(struct int_list));
         el->i = i;
         list_add_tail(&(el->head), head);
         j++;
        }
\n ;
. ;

%%



int* adpc_readarray(char *filename, int offset)
{
  int *array;
  struct int_list *itr;
  int z=offset;
  FILE *file;
  YY_BUFFER_STATE state;

  head = malloc(sizeof (struct list_head));
  list_init(head);

  if (filename) {
    if (!(file = fopen(filename, "r"))) {
      perror("fopen");
      abort();
    }
    state = yy_create_buffer(file, YY_BUF_SIZE);
    yy_switch_to_buffer(state);

    yylex();
    yy_delete_buffer(state);
    fclose(file);
  } else
    yylex();

  if (!j) {
    fprintf(stderr, "Found nothing in input, probably wrong input format.\n");
    exit(1);
  }

  array = malloc(sizeof(int) * (j + offset));
  itr = (struct int_list*) head->next;
  while (((struct list_head*) itr != head)) {
    array[z++] = itr->i;
    itr = (struct int_list*) itr->head.next;
  }

  list_free(head);
  j=0;

  return array;
  
} 

Nts adpc_deserialize(char *filename)
{
  int *array;
  int size;
  Nts result;
  array = adpc_readarray(filename, 0);
  result = deserializeGraph(array, &size);
  free(array);
  return result;
}
