// find the sum of the numbers less than 1000
// which are multiples of 3 or 5.

// c++ rewrite May 18 2011
// correct output verified

#include <iostream>
using namespace std;

// divby is totally superfluous
bool divby(int n, int b);

// this is the main work
int add3and5(int upto);

// display result
int main(){
  int c = add3and5(1000);
  cout<<c<<endl;
  return 0;
}

bool divby(int n, int b){
  return !(n%b) ? true : false;
}

int add3and5(int upto){
  int sum = 0;
  int counter = 1;
  while(counter < upto){
    if( divby(counter, 3) || divby(counter, 5))
      sum += counter;
    counter++;
  }
  return sum;
}
