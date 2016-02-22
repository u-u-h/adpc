#include <stdio.h>
#include <stdlib.h>

#include <tabulate.h>
#include <asm_typedefs.h>
#include <asm_runtime.h>
#include <functions.h>
#include <input.h>
#include <adpc_deserialize.h>
#include <functions.h>

void mark_asm_nts(Asm_Nts asm_nts, int count, int argc, char **argv)
{
  int i, j, k;
  if (argc > 2) {
    j = 2;
    k = atoi(argv[j]) - 1;
  } else
    k = -1;
  for (i=0; i<count; i++) {
    if (asm_nts[i].nt == k) {
      asm_nts[i].tab = 0;
      if (j < argc - 1) {
        j++;
        k = atoi(argv[j]) - 1;
      }
    } else
      asm_nts[i].tab = 1;
  }
}

int main(int argc, char **argv)
{
  char *filename;
  Nts nts;
  Asm_Nts asm_nts;
  int count;
  Run_Type run_state;
  char* tab_runtime;
  int result, i;
  char *s;

  filename = argv[1];
/*
  int* arr;
  int size;
  FILE* file = fopen(filename,"r");
  read_array(file,&arr,&size);
  fclose(file);
  nts = deserializeGraph(arr,&size);
*/  
  nts = adpc_deserialize(filename);
  convert_input(nts, &asm_nts, &count);
  mark_asm_nts(asm_nts, count, argc, argv);

  run_state = malloc(count * sizeof(struct run_type));
  for (i = 0; i < count; i++) {
    run_state[i].runstate = -1;
    run_state[i].list = NULL;
  }

  tab_runtime = calloc(count, sizeof(char));

  result =
    calc_asm_runtime(asm_nts, 0, count, EXPONENTIAL, run_state, tab_runtime);

  s = asm2string(result);
  printf("\nRuntime: %s\n", s);
  
  return 0;
}
