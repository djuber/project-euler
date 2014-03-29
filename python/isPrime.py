import math

def isPrime(c):
	""" check every odd number up to c^1/2 for division """
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
    

def prime(n):           #return the nth prime
        if (n==1):
                return 2
        elif(n==2):
                return 3
        c = 1
        i = 1
        while (i < n):
                c+=2
                if isPrime(c):
                        i+=1    #increment the counter
                
        return c

def factor(n):                  # (some mess in here!)
        """print the factors of n"""
        for x in range(2,math.floor(math.sqrt(n)+1)):
                if n % x == 0:
                        print(n,'eq',x,'*', n//x)
                        n=n//x
                        factor(n)
                        
def sumofprimes(n):             
        """ print the sum of primes below n"""
        c = 0
        for x in range(2,n):
                if(isPrime(x)):
                        c+=x
        print(c)
