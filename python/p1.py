"""
Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
"""

from divides import divides

def euler_p1():
    return sum(filter(lambda x: divides(3,x) or divides(5,x), range(1000)))