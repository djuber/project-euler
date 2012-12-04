/*

Problem 9
25 January 2002


A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

*/
#include <math.h>
#include <stdio.h>
/*
int main(){
*/
int problem9(){
unsigned long int a, b, c;
for(a=1;a<1000;a++){
    for(b=1;b<a;b++){
        c=floor(sqrt(a*a + b*b));
        if((a+b+c)==1000)
            if((a*a+b*b)==(c*c)){
                printf("a %d b %d c %d\n",a,b,c);
                printf("product = %d\n", (a*b*c));
            }
    }
}
return 0;
}
