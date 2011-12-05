
//  A palindromic number reads the same both ways. The largest palindrome
//  made from the product of two 2-digit numbers is 9009 = 91 × 99.

//  Find the largest palindrome made from the product of two 3-digit numbers.

//  Answer:
//     906609
#include <sstream>
#include <iostream>
using namespace std;

//requires sstream
string digits(int n){
  string s;
  stringstream out;
  out<<n;
  s = out.str();
  return s;
}
//requires string
bool palindrome(string s){
  int c = s.length();
  for(int i = 0; i<c; i++)
    if( s[i] != s[c-i-1])
      return false;
  return true;
}

//requires iostream
int main(){
  int maxp = 0;
  int prod;
  for(int i =999; i > 0; i--)
    for(int j = i; j>0; j--)
      if( palindrome(digits(prod=i*j)) && prod>maxp)
	maxp = prod;
  cout<<maxp<<endl;
  return 0;
}
