/* find a path to one
    if even, divide by 2
    if odd, multiply by 3 and add 1
    return number of steps to 1.
    */
#include <stdio.h>
char isEven(long int n);

long int collatz(long int n)
{
    long int count = 0;

    while(n>1)
    {
        if(isEven(n))
        {
            n/=2;
            count++;
        }
        else
        {
            n=n*3 +1;
            count++;
        }

    }
    /* added check to ensure this really did settle on n equals 1 */
    /* this caught an error where collatz(113383) after count = 120 had gone negative...
    all results are likely invalid beyond this point */
    if(n==1) return count;
    else return -1;
}
char isEven(long int n)
{
    return ((n%2)==0);
}
