from isEven import isEven


def collatz(n):
    count = 0

    while(n>1):
        if(isEven(n)):
            n//=2
            count +=1
        else:
            n=n*3+1
            count +=1
    if(n==1):
        return count
    else:
        return -1

def problem14(upper):
    """identify starting point of longest collatz path below uppper"""
    n=1
    max = 0
    maxn = 0
    while(n<upper):
        c=collatz(n)
        if c>max:
            max=c
            maxn=n
            print(max,maxn)
        n+=1
    print("max", max, "n", maxn)

