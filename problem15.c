/*
Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.


How many routes are there through a 2020 grid?

*/

/* if there is one row, then the answer is number of columns +1
    so a 1x1 grid is 2
    and a 1x2 grid is 3 (DRR,RDR,RRD)

    similarly, if there is 1 column, the answer is number of rows +1

    for a MxN grid, this can be seen traced by recursion
    given x between 0 and m, and y between 0 and n,
        there are 2 options, increase x, and increase y

        recursion of course, is slow and unwieldly!

*/
/* STUPID SMALL NUMBERS! : C cannot represent this */
/*
int main(){
        int i,j;
        unsigned long int paths[20][20];
        for(i=0;i<20;i++){
            paths[i][0]=i+2;
            paths[0][i]=i+2;
        }
        for(i=1;i<20;i++)
                paths[i][j]=paths[i-1][j]+paths[i][j-1];

        printf("%u\n", paths[19][19]);
        return 0;
}

/* use delicious recursion for the stack overflow!*/
int path(int m, int n){
    /* trivial cases */
    if (m==1)
        return n+1;
    if(n==1)
        return m+1;
    else
        return path(m-1,n) + path(m, n-1);
}
