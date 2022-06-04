import math
import numpy as np
import matplotlib.pyplot as plt
import random

N = 1000
lam = 10

x_n = np.random.poisson(lam, N)

plt.hist(x_n, bins = 17)
plt.show()


sum = np.cumsum(x_n)
avr = sum / range(1, N + 1)
lam_n = [lam] * N

plt.plot(range(1, N + 1), avr)
plt.plot(range(1, N + 1), lam_n)

q = np.array(lam_n)
q = q / range(1, N + 1)
q = np.sqrt(q)


plt.plot(range(1, N + 1), lam - q)
plt.plot(range(1, N + 1), lam + q)
plt.show()

