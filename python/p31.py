# problem 31
def rup2(t,coinsize):
    if t == 1 or coinsize ==1 : return 1
    s = 0
    for i in range(0,t/coinsize+1):
        s += rup2(t - coinsize*i,coinsize-1)
        return s 

