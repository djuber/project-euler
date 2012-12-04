"""
Hah. I'm so low on sleep. I'm going to post this just because it's odd enough and worked on my first try. I reimplemented long division, horribly I'm sure, and kept a list of 'remainders I had seen before' - obviously if you hit the same remainder, you're looping. Then I let it through one more loop of the list so I could mark the start and finish of it cleanly.

It's quirky, ran decently fast, and I enjoyed it. Time for coffee. Or sleep.

The python in question:
"""

def frac(n):
   s,hist,rem,point,freeshift,loopfound = '',[],1.,1,0,0
   while (rem > 0.):
      if (rem < n):
         if (freeshift == 0):
            s += '0'
         else:
            freeshift = 0
         if (point):
            point = 0
            s += '.'
         rem *= 10.
      else:
         if (not (rem in hist)):
            hist.append(rem)
            dig = int(math.floor(rem/n))
            s += str(dig)
            rem -= n*dig
            freeshift = 1
         else:
            s += "!!!"
            if (not (loopfound)):
               loopfound = 1
               hist = [rem]
            else:
               return s
            dig = int(math.floor(rem/n))
            s += str(dig)
            rem -= n*dig
            freeshift = 1              
   return s

maxn,maxl = 0,0
for x in range(1,1000):
   f = frac(x)
   if ('!!!' in f):
      seq = f.split('!!!')[1]
      if (len(seq) > maxl):
         maxn,maxl = x,len(seq)

print maxn
