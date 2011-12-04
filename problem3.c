/*
ProjectEuler.net
Problem 3
02 November 2001


The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

*/
/*
int main(){
unsigned long int zero = 0;
printf("%u\n",(unsigned long)(zero-1));
return 0;

}
4294967295 is max unsigned, c is inadequate without arbitrary length integers

*/

/* Python code to find this:

import math
n  = 600851475143
for x in range(2,math.floor(math.sqrt(n))):
	if n % x == 0:
		print(n,'eq',x,'*', n//x)
		n=n//x


600851475143 eq 71 * 8462696833
8462696833 eq 839 * 10086647
10086647 eq 1471 * 6857
6857 eq 6857 * 1

6857 is the verified answer
*/
