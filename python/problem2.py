"""
Even Fibonacci numbers
Problem 2

Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

"""


# the big idea is that every third fibonacci number is even,
# and we don't want to call fib over and over again to discard the intermediate results
# thus, we just build up the evens and collect the sum along the way
def problem2():
    sum = 0
    fibs = [ 2, 1, 1]
    while fibs[0] < 4000000:
        sum += fibs[0]
        # we can build the next three values from the last two: 
        fibs = [3*fibs[0] + 2*fibs[1] ,2 * fibs[0] + fibs[1] , fibs[0] + fibs[1]]
    return sum
    