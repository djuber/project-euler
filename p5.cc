// 2520 is the smallest number that can be divided by each of the numbers from
// 1 to 10 without any remainder.

// What is the smallest positive number that is evenly divisible by all of the
// numbers from 1 to 20?
#include <iostream>
using namespace std;

// a pretty good thing to have, and fairly fast
unsigned int gcd(unsigned int m, unsigned int n){
  unsigned int r;
  while(n){
    r = m % n;
    m = n;
    n = r;
  }
  return m;
}

//smallest integer evenly divisible by all numbers 1 to upto
unsigned int lcm(int upto){
  unsigned int value = 1;
  for(int i = upto; i > 1; --i)
    if (value%i)
      // since value isn't a multiple of i, multiply by gcd
      value *= (i/gcd(value,i));
  return value;
}

int main(){
  cout<<lcm(20)<<endl;
  return 0;
}
