/*
Problem 5
30 November 2001


2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


*/

/* Thoughts 2 * 2 * 2 * 3 * 3 * 5 * 7 = 2520
    so for 1 through 20, we find 20 = 4 * 5 (2^2*5)
                                 19 = 19
                                 18 = 3^2 * 2
                                 17 = 17
                                 16 = 2^4
                                 15 = 3*5
                                 14 = 7*2
                                 13 = 13
                                 12 = 3 * 2^2
                                 11 = 11
our answer will be 2^4 * 3^2 * 5  * 7 * 11 * 13 * 17 * 19
calculator gave 232792560,
this was verified.

a better way is to find a function lcm that gives the least common multiple of 2 numbers.
then do:
r=1
for(i=1;i<max;i++)
    if t=lcm(r,i) > r
    r=t
*/
