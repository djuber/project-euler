/* ProjectEuler.net
Problem 4
16 November 2001


A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.

*/

/* Verified answer:
906609 is the largest palindromic product of 2 three digit numbers
It is 993 * 913
*/

typedef struct maxstruct {
long int max;
int a;
int b;
} maxstruct;

char isPalindrome(unsigned long int n){
    char digits[16];
    int i,j;
    for(i=15;i>=0;i--){
            digits[i]=n%10;
            n=n/10;
    }
    for(i=0;digits[i]==0;i++);
    // i now points to the first non-zero digit in n
    j=15;
    while(j>i){
        if(digits[i++]!=digits[j--])
            return 0;
    }
    return 1;
}

/*
int main(){
int a,b;
long int c;
maxstruct max;
max.max=0;
for(a=999;a>0;a--)
    for(b=999;b>0;b--)
        if(isPalindrome(c=a*b)){
            if(c>max.max){
                max.max=c;
                max.a=a;
                max.b=b;
            }
        }
printf("%d is the largest palindromic product of 2 three digit numbers\n",max);
printf("It is %d * %d\n", max.a, max.b);
return 0;
}
*/
