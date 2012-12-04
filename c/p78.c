#include <stdio.h>

// modified to use mod 1000000
void numbparts(const int *n, unsigned long long int *pp){
  /* p(1)...p(n) calculated using
     Euler's recursive formula on p825
     of Abramowitz & Stegun */
  int i, s, f, r;
  unsigned long long int *ip;
  const int million = 1000000;
  pp[0] = pp[1] = 1;
  for(i=2 ; i< *n ; i++){
    /* first do r = m(3m+1)/2  */
    s = 1;  /* "s" for "sign" */
    f = 5;  /* f is first difference  */
    r = 2;  /* initial value (viz m(3m+1)/2 for m=1) */
    ip = pp+i;
    *ip = 0;
    while(i-r >= 0){
      *ip += s*pp[i-r];
      r += f;
      f += 3;  /* 3 is the second difference */
      /* change sign of s */
      s *= -1;
      *ip = (*ip )%million;
    }
    /* now do r = m(3m-1)/2 */
    s = 1;
    f = 4; /*first difference now 4 */
    r = 1; /* initial value (viz m(3m-1)/2 for m=1) */
    while(i-r >= 0){
      *ip += s*pp[i-r];
      *ip = (*ip) % million;
      r += f;
      f += 3;
      s *= -1;
    }
  }
}

int main(){
  unsigned long long int p[60000];
  const int size = 60000;
  size_t i;
  // populate the array
  numbparts(&size, p);
  // check for the zero
  for(i=0;i<size;i++){
    if(p[i]==0)
      printf("%d\n", i);
  }
  return 0;
}
