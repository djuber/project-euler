// The sum of the squares of the first ten natural numbers is,
// 12 + 22 + ... + 102 = 385

// The square of the sum of the first ten natural numbers is,
// (1 + 2 + ... + 10)2 = 552 = 3025

// Hence the difference between the sum of the squares of the first ten natural
// numbers and the square of the sum is 3025 − 385 = 2640.

// Find the difference between the sum of the squares of the first one hundred
// natural numbers and the square of the sum.

#include <iostream>
using namespace std;

unsigned long int sumofsquares(unsigned long int upto){
  unsigned long int sum = 0;
  for(unsigned long int i = 1; i<= upto; ++i)
    sum += i*i;
  return sum;
}

unsigned long int squareofsum(unsigned long int upto){
  unsigned long int sum = 0;
  while(upto){
    sum += upto;
    upto--;
  }
  return sum*sum;
}

unsigned long int euler6(unsigned long int upto){
  return squareofsum(upto) - sumofsquares(upto);
}

// using closed form for summations
int problemSix(int n) {
return ((n * (n+1) / 2) * (n * (n+1) / 2)) - (n * (n+1) * ((2 * n) + 1) / 6);
}

int main(){
  cout<<euler6(100)<<endl;
}
