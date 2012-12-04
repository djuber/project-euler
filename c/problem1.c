
/*
ProjectEuler.net
Problem 1
05 October 2001


If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

*/

/* add all multiples of 3 or 5 below max */
unsigned long int add3and5(unsigned long int max){
    unsigned long int result = 0;
    unsigned long int c = 1;
    while(c<max){
        if(((c%3)==0) || ((c%5)==0))
            result+=c;
        c++;
    }
    return result;
}
/* Test driver:
int main(){
printf("%d\n", add3and5(1000));
return 0;
}
*/
/* answer verified 233168 */
