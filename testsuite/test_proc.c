#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <procfs.h>

#define exit_print(A, B) if ((A) == -1) { perror(B); exit(77); }
#define error_print(A, B) if ((A) == -1) { perror(B); }

int main(int argc, char **argv)
{
  pid_t r;

  printf("size: %zu %zu\n", sizeof(struct psinfo), sizeof(size_t));

  r = fork();
  if (r>0) {
    int buffer_size = sizeof(psinfo_t) > 512 ? sizeof(psinfo_t) : 512;
    char *buffer;
    char filename[255];
    ssize_t count = 0;
    size_t rss = 0, size = 0;
    int foo, fd;
    psinfo_t psi = { 0 };
    pid_t t;
    int status;
    char model = 0;

    psi.pr_size = 0;
    psi.pr_rssize = 0;

    buffer = calloc(buffer_size, 1);
    filename[0] = 0;
    snprintf(filename, 255, "/proc/%d/psinfo", (int)r);
    fd = open(filename, O_RDONLY);
    exit_print(fd, NULL);
    do {
      size_t n;
      n = pread(fd, buffer, buffer_size, 0);
      if ((count != 0 && count != n) || n <= 0)
        fprintf(stderr, "Error reading proc: %zd vs %zd\n", count, n);
      else {
        count = n;
        if (n >= sizeof(psinfo_t)) {
          memcpy(&psi, buffer, sizeof(psinfo_t));
          rss = rss < psi.pr_rssize ? psi.pr_rssize : rss;
          size = size < psi.pr_size ? psi.pr_size : size;
          model = psi.pr_dmodel;
        }
      }
      t = waitpid(r, &status, WNOHANG);
      usleep(200);
    } while (t == 0 || !(WIFEXITED(status) || WIFSIGNALED(status) ||  WCOREDUMP(status)));
    printf("Needes rss: %zu size: %zu read: %zu model:%d\n", rss, size, count, (int)model);
    foo = close(fd);
    exit_print(foo, "(close)");
  } else if (r == 0)  {
    int foo;
    foo = execv(argv[1], argv+2);
    exit_print(foo, "(execv)");
  } else
    exit_print(r, "(fork)");
  return 0;
}
