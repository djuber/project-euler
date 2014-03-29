""" divides : factoring out a common enough operation
use :
 from divides import divides
"""

def divides(a, b):
    """true if a divides b"""
    return b % a == 0
