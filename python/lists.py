# list utilities
# in general these are unnecessary.
def union(a,b):
    "return the union of two collections."
    return list(set(a).union(b))

def intersection(a,b):
    "return the intersection of two collections"
    return list(set(a).intersection(b))

def unique(a):
    "return only one instance of each item in the list"
    return list(set(a))