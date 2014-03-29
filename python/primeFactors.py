from divides import divides
from isPrime import primes
from isEven import isEven


""" 
Do the simplest thing possible. 
Given n, generate a list of primes from 2 to sqrt(n).

Check each prime. If any is a divisor, we add it to the list result.
Then join the list with the factorization of the remainder.

If no prime factors were found, then n should be prime, add it to the list.

Return the list.
"""


def primeFactors(n):
    """a list of prime factors of n"""

    result = []    
    upper_bound = 1 +int(n**0.5) # only have to seek to the square root 
    searchList = primes(upper_bound)
    
    for divisor in searchList:
        if divides(divisor, n):
            result.append(divisor)
            result += primeFactors(int(n // divisor))
            break
    # no factors found, so assume prime.
    if result == []:
        result.append(n)
    return result
