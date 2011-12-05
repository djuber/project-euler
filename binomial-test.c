#include <stdio.h>
#include "pascal.h"

/* test loop to validate binomial function.

int main(){
  int a, b;

  for (a = 10; a >= 2; a--)
    for (b =0 ; b <= a; b++)
      printf("%d %d %d\n", a, b, binomial(a,b));
  return 0;
}

*/


int main() {
  pascal(8);
  return 0;
}
