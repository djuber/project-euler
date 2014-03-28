import unittest
from problem1 import *


class eulerTest(unittest.TestCase):
    def testMultiplesList1(self):
        """If we list all the natural numbers below 10 that are multiples of 3 or 5,
        we get 3,5,6,9. The sum is 23."""
        self.assertEqual(sum(set(multiplesList(3, 10)).union(multiplesList(5,10))), 23)
    def testMultiplesList2(self):
        self.assertEqual(multiplesList(5,10), [5])
    def testMultiplesList3(self):
        self.assertEqual(multiplesList(3,10), [3,6,9])




if __name__=='__main__':
    unittest.main()