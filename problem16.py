#problem 16
# add the digits in 2^1000

def problem16(n): #return sum of digits in 2^n
    sum=0
    for i in str(pow(2,n)):
        sum+=int(i)
    return sum



