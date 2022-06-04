import math
import numpy as np
import matplotlib.pyplot as plt
import random

days = 365
stats = np.loadtxt('us_births_69_88.csv', skiprows=1, delimiter=',', dtype=int)
number_of_births = stats[:, 2]
p = sum(number_of_births)
p_n = [number_of_births[i] / p for i in range(days)]
p_max = max(p_n)


def losuj_date():
    while True:
        d1 = random.randrange(days)
        m = random.uniform(0, 1)

        if (m < p_n[d1] / p_max):
            return d1


def liczba_losowan():
    mySet = set()
    date = losuj_date()
    counter = 0

    while (date not in mySet):
        mySet.add(date)
        date = losuj_date()
        counter += 1

    return counter


res = [liczba_losowan() for i in range(1, 100000)]
plt.hist(res, bins=range(70))
plt.show()

