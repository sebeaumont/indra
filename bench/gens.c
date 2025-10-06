/* good ole c */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <complex.h>

char const generators [] = {'a','B','A','b'};
int const maxdepth = 15;
int const ngens = (sizeof(generators)/sizeof(generators[0]));

double complex w; 

void explore(int l, int d, char p[]) {
  if (d < maxdepth) {
    printf("%s --> ", p);
    for (int i = l-1; i <= l+1; i++) {
      // put path on the stack
      char prefixed[strlen(p)+2];
      // index wrap around
      int k = (i < 0) ? ngens+i : i % ngens;
      sprintf(prefixed, "%c%s", generators[k], p);
      explore(k, d+1, prefixed);
    }
  }
} 

int main(/* int argc, char* argv[argc+1] */) {
  //assert(ngens == 4);
  for (int l=0; l < ngens; l++) {
    char start[2];
    sprintf(start, "%c", generators[l]);
    explore(l, 0, start);
    printf("\n");
  }
  return(0);
}
