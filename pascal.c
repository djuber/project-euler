/* display pascals triangle n rows tall */
/* there is a bug in binomial which will affect rows 33 and higher
    rolls over to negative.
    binomial changed to unsigned long, moved issue to row 34
*/
#include "pascal.h"
#include "binomial.h"


char *pascalrow(int row, int rows, char *buffer)
{
    char number[64];
    int j;
    /*  int pad = rows/2; //number of cells to pad, skipping for now */
    for(j=0; j <= row; j++)
    {
        sprintf(number, "%d ", binomial((unsigned long) row, (unsigned long) j));
        strcat(buffer, number);
    }
    strcat(buffer, "\n");
    return buffer;
}

char *clearbuffer(char *buffer, int count)
{
    int i;
    for(i = 0; i < count; i++)
        buffer[i]=0;
    return buffer;
}

void pascal(int n)
{
    char store[1024];
    char *buf = store;
    int i;
    for (i=0; i<=n; i++)
        printf("%s", pascalrow(i, n, clearbuffer(buf,1024)));
    return;
}
