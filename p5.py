#!/usr/bin/env python
import math

# 06 Apr 2007 06:24 am 
# colcob   (Python)  colcob is from England
# My python solution. Uses a sieve method to find prime factors and works out LCM from prime factors. For 1-20 it runs in 1ms.
# Within the 1 minute rule it can calculate the LCM of all numbers between 1 and 100000(43,452 digits long).
# So despite being lots of not very elegant code, it seems to be pretty quick. 

def primeFactors(n):  #returns list of all prime factors
    primes = primeSieve(int(math.ceil(math.sqrt(n)))+1) 
    factors = list()
    i = 0
    fact = n
    while fact not in primes and i < len(primes):
        if fact%primes[i] == 0:
            fact = fact/primes[i]
            factors.append(primes[i])
        else:
            i+=1
    factors.append(fact) #remaining factor must be prime so add
 
    uniques = set(factors)
    primeFactorisation = dict()
    for prime in uniques:
        exp = factors.count(prime)        
        primeFactorisation[prime]= exp
    return primeFactorisation

def primeSieve(n):  # returns list of all primes < n
    top =int( math.ceil(math.sqrt(n)))
    poss = set(range(2, n))
    notPrime = set()
    for i in range(2, top, 1):
        if i in notPrime:
            start = 1
        else:
            start = 2
        for j in range(start, n/i+1):
            try:
                notPrime.add(i*j)
            except:
                pass
    primes = list(poss.difference(notPrime))
    primes.sort()
    return primes

def lcm(numList):
    """takes list of numbers, returns lowest common multiple of all numbers"""
    maxExp = dict()
    i = 0
    answer = 1
    for n in numList:
        pFactors = primeFactors(n)
        for factor in pFactors:
            if maxExp.has_key(factor):
                if pFactors[factor] > maxExp[factor]:                    
                    maxExp[factor] = pFactors[factor]
            else:
                maxExp[factor] = pFactors[factor]
        i += 1
 
    for n, e in maxExp.iteritems():
        answer *= pow(n, e)
 
    return answer
 
print lcm(range(1, 21))
