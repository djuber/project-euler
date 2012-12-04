/*
ProjectEuler.net
Problem 2
19 October 2001

Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

Find the sum of all the even-valued terms in the sequence which do not exceed four million
*/

/* Compute the nth fibonacci number */
unsigned long int fibonacci(long int n) {
long int a = 0;
long int b = 1;
long int counter = 1;

while (counter < n){
long int tmp = a;
a = b;
b += tmp;
counter++;
}

return b;
}

/*
This produces the right answer, but checks for the even can slow this down (especially in cases where the limit is above 4M, say 4M*4M
every third term is even in the fibonacci sequence, and a relation of F(n)=4F(n-1)+F(n-2) exists...
*/
/* test driver
int main(){
    unsigned long int sum=0;
    long int c=1;
    unsigned long int fn;
    while((fn=fibonacci(c++))<4000000){
        if (!(fn%2))
            sum+=fn;
    }
    printf("%d\n",sum);
    return 0;
}
*/
/* verified answer: 4613732 */
