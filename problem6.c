/*
Problem 6
14 December 2001


The sum of the squares of the first ten natural numbers is,

1^2 + 2^2 + ... + 10^2 = 385
The square of the sum of the first ten natural numbers is,

(1 + 2 + ... + 10)^2 = 55^2 = 3025
Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025  - 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

The difference between 338350 and 25502500 is 25164150

*/

unsigned long int sum(int limit){
int i;
unsigned long int r=0;
for(i=0;i<=limit;i++)
    r+=i;
return r;
}

unsigned long int sumofsquares(int limit){
int i;
unsigned long int r=0;
for(i=0;i<=limit;i++)
    r+=i*i;
return r;
}
/*
int main(){
unsigned long int i;
int limit = 100;
i = sum(limit);
i=i*i;
printf("The difference between %u and %d is %u\n", sumofsquares(limit), i, i - sumofsquares(limit));
return 0;
}
*/
