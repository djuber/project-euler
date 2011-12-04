# happy numbers, taken from a Programming Praxis topic
from isPrime import primes

happiness=[1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68,
           70, 79, 82, 86, 91, 94, 97, 100, 103, 109, 129,
           130, 133, 139, 167, 176, 188, 190, 192, 193, 203,
           208, 219, 226, 230, 236, 239, 262, 263, 280, 291,
           293, 301, 302, 310, 313, 319, 320, 326, 329, 331,
           338, 356, 362, 365, 367, 368, 376, 379, 383, 386,
           391, 392, 397, 404, 409, 440, 446, 464, 469, 478,
           487, 490, 496, 536, 556, 563, 565, 566, 608, 617,
           622, 623, 632, 635, 637, 638, 644, 649, 653, 655,
           656, 665, 671, 673, 680, 683, 694, 700, 709, 716,
           736, 739, 748, 761, 763, 784, 790, 793, 802, 806,
           818, 820, 833, 836, 847, 860, 863, 874, 881, 888,
           899, 901, 904, 907, 910, 912, 913, 921, 923, 931,
           932, 937, 940, 946, 964, 970, 973, 989, 998, 1000]

happyfill=[1000]
primenumbers=[]

def happy(n):
    t=[]    # paths
    while(n!=1):
        if n in t:  #we've found a loop
            return False
        elif n in happiness:    # do we know about this?
            for i in t:
                if i not in happiness:
                    happiness.append(i) # add success
            return True
        else:
            t.append(n)     # record the number to the path
            n=sumOfSquareOfDigits(n)
    # found a happy number
    for i in t:
        if i not in happiness:
            happiness.append(i)
    return True


def sumOfSquareOfDigits(n): #does just what it says
    s=0
    for i in str(n):
       s+=int(i)**2
    return s

def fillHappiness(n):   #find happy numbers up to n, add to happiness
    if n <= happyfill[0]:
        return len(happiness)
    else:
        for i in range(happyfill[0],n):
            happy(i)
        happyfill[0]=n
        return len(happiness)

def primalHappiness(n): # find happy primes up to n, return list of them
    fillHappiness(n)
    primenumbers=primes(n)
    happyPrimes=[]
    for i in happiness:
        if i in primenumbers:
            happyPrimes.append(i)
    return happyPrimes
