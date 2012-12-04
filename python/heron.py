import math

def prod(L):
    """ analog to sum using product of all elements in L """
    p = 1
    for i in L:
        p *= i
    return p

def amean(L):
    """ arithmetic mean of elements of L """
    return 1.0*sum(L)/len(L)

def arithmean(a,b):
    return (a + b) / 2.0

def gmean(L):
    return prod(L)**(1.0/len(L))

def geommean(a,b):
    return math.sqrt(a*b)

def heron(a,b):
    return 2.0/3.0 * arithmean(a,b) + 1.0/3.0 * geommean(a,b)

def harmonic(a,b):
    t = 1.0 / a + 1.0 / b
    t = 1.0 / t
    return t/2

def main():
    for i in range(1,10):
        for j in range(i,10):
            print "%d\t%d\t%.3f\t%.4f\t%.4f"\
                  % (i, j, amean([i,j]), gmean([i,j]), heron(i,j))

main()
