import math

def isPrime(c):
	if c == 2:
		return True
	if c % 2 == 0:
		return False
	t = 3
	limit = math.sqrt(c)
	limit+=1
	while t <= limit:
		if c % t == 0:
			return False
		t += 2
	return True
	
def primes(limit):	#return a list of primes up to limit
	result = []
	result.append(2)
	c=3
	while(c<=limit):
		if isPrime(c):
			result.append(c)
		c += 2
	return result
    


p32=primes(65536)               # list of primes below int_max

def triangle(n):
    c=0
    for x in range(1,n+1):
        c+=x
    return c

def nextTriangle(n, last):
    return n+ last

def factors(n):                 #return the number of factors of n, in no hurry
    c=0
    for x in range(1,n+1):
        if n%x == 0:
            c+=1
    return c

def factor(n):                  #print the factors of n (some mess in here!)
        for x in range(2,math.floor(math.sqrt(n)+1)):
                if n % x == 0:
                        print(n,'eq',x,'*', n//x)
                        n=n//x
                        factor(n)

def factors2(n):                #return the number of factors of n, faster
    f=[1]
    for x in range(1,math.floor(math.sqrt(n)+1.1)):
            if n % x == 0:
                if(x not in f):
                    f.append(x)
                if(n not in f):
                    f.append(n)
                if(n//x not in f):
                    f.append(n//x)
    return len(f)

def factors3(n):                #return the number of factors of n, using primes
    p=0
    exponents=[0]
    while(n>1):
        if(n%p32[p] ==0):         #n is divisible by the (n+1)th prime
            exponents[p]+=1
            n//=p32[p]
        else:                   # fill with 0
            exponents.append(0)
            p+=1
    product = 1
    for i in exponents:
        product *= (i+1)
    return product


def minimumTriangleWithAtLeastFactors(n):
    c=1
    last = triangle(c)
    while(factors3(last) < n):
        last += c + 1
        c += 1
    return c

    
def problem12(n):
    last=0
    f=0
    x=1
    while x < n+1 :
        if x > f:
            last = minimumTriangleWithAtLeastFactors(x)
            t = triangle(last)
            f = factors3(t)
            print("factors", f, "n", last, "value", t)
        x+=1   
    return triangle(last)

def problem15(n):   #return the number of paths through an n by n grid
    paths=[]
    for i in range(0,n):
        paths.append(i+2)

    for x in range(1,n):
        paths[0]=x+2
        for i in range(1,n):
            paths[i]+=paths[i-1]
    return paths[n-1]
    
    


