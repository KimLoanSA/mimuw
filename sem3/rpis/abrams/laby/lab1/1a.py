import math
import matplotlib.pyplot as plt

def silnia(a, b):
    res = 1
    for i in range(a, b + 1):
        res *= i
    return res
    
def p(d, n):
    return 1 - silnia(d-n+1, d) / d ** n

def p2(d, n):
    return 1 -  math.exp(-((n*(n-1))/(2*d)))
    
xs = list(range(1, 61))
ys = [p(365, n) for n in xs]
plt.plot(xs, ys)
ys2 = [p2(365, n) for n in xs]
plt.plot(xs, ys2)
plt.show()