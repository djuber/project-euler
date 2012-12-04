// The prime factors of 13195 are 5, 7, 13 and 29.

// What is the largest prime factor of the number 600851475143 ?

#include <iostream>
using namespace std;

// return the largest factor of n
unsigned long int factor(unsigned long int n) {

  // probably unnecessary, but:
  if (n==1)
    return 1;

  unsigned long int c = 2;

  // start at 2, and keep dividing out factors.
  // it will skip over all composites (since they cannot divide once the primes
  // are factored out

  while(n!=1){
    if (!(n%c))
      n /= c;
    else
      c++;
  }

  // now c is the largest prime divisor of n, and n = 1
  return c;
}

int main(){

  // this is a joke loop to test it out in the neighbourhood.
  // warning: 600851475145 is 5*120170295029, and this will take forever!
  /*
  for(unsigned long int i = 600851475140; i<600851475150; ++i)
    cout<<i<<" "<<factor(i)<<endl;
  */

  cout<<factor(600851475143)<<endl;
  return 0;
}
