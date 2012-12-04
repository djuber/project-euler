/* bugs: int is too small for numbers like 33 choose 16
         consider either long or unsigned long, and possibly double or long double
         if extending the int type, to prevent inaccuracies
        */


unsigned long int binomial(unsigned long int n, unsigned long int k)
{
    float c=1.0;

    if((n<0) || (k<0))
        return -1;
    while(k)
        c*=(1.0*n--)/k--;
    return (unsigned long int)c;
}


