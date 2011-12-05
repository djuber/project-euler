#include <stdio.h>

unsigned long cube (unsigned long a)
{
  return a * a * a;
}

unsigned long pluscubes(unsigned long a, unsigned long b)
{
  return cube(a) + cube(b);
}

int sumofcubes (unsigned long a)
{
  int cnt = 0;
  unsigned long b,c,last;
  last=0;
  for(b=1;cube(b)<a;b++)
    for(c=1;c<=b;c++)
      if(pluscubes(b,c)==a)
	last++;
  return last;
}

int findways(unsigned long start, unsigned long stop, int ways)
{
  unsigned long candidate;
  for( candidate = start; candidate <= stop; candidate++)
    if(sumofcubes(candidate) >= ways)
      printf("%lu\n", candidate);
  return 0;
}

int main()
{
  findways(1, 10000, 2);
  //  printf("////////////////\n");
  //  findways(1, 10000000, 3);
  return 0;
}
