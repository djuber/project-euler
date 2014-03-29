"""


If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

"""

from multiplesList import multiplesList

def euler1():
    threes = multiplesList(3, 1000)
    fives = multiplesList(5,1000)
    fifteens = multiplesList(15, 1000)

    return sum(threes) + sum(fives) - sum(fifteens)


if __name__ == '__main__':
    print euler1()