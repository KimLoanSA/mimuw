import math
import numpy as np
import matplotlib.pyplot as plt
import random

days = 365
stats = np.loadtxt('us_births_69_88.csv', skiprows=1, delimiter=',', dtype=int)
number_of_births = stats[:, 2]
p = sum(number_of_births)
p_n = np.array(list(number_of_births[i] / p for i in range(days)))
p_max = np.max(p_n)
length = days * 10_000

days = np.random.randint(0, days, size=length)
d = np.random.uniform(0, 1, size=length)
days = days[d < p_n[days] / p_max]
indeks = 0


def liczba_losowan():
    global indeks
    global length

    mySet = set()
    date = days[indeks]
    counter = 0
    indeks += 1

    while (date not in mySet):
        mySet.add(date)
        date = date = days[indeks]
        indeks += 1
        counter += 1

    return counter


res = [liczba_losowan() for i in range(1, 100_000)]
plt.hist(res, bins=range(70))
plt.show()