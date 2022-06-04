import math
import numpy as np
import matplotlib.pyplot as plt
import random

stats = np.loadtxt('us_births_69_88.csv', skiprows=1, delimiter=',', dtype=int)

months = np.arange(1, np.max(stats[:, 0]) + 2)
days = np.arange(1, np.max(stats[:, 1]) + 2)
numbers = np.cumsum(stats[:, 2])
maxV = numbers[-1]
values = np.zeros((len(months), len(days)))

for s in stats:
    values[s[0] - 1][s[1] - 1] = s[2]

plt.pcolor(days, months, values)
plt.show()

d = 372


def getRandom():
    return random.randrange(d) + 1


def repeat():
    mySet = set()
    date = getRandom()
    counter = 0

    while (date not in mySet):
        mySet.add(date)
        date = getRandom()
        counter += 1

    return counter


res1 = [repeat() for i in range(1, 100_000)]
plt.hist(res1, bins=range(70))


def getRandom2():
    return random.randrange(maxV) + 1


def getDate():
    a = getRandom2()
    return np.searchsorted(numbers, a)


def repeat2():
    mySet = set()
    date = getRandom2()
    counter = 0

    while (date not in mySet):
        mySet.add(date)
        date = getRandom()
        counter += 1

    return counter


res2 = [repeat2() for i in range(1, 100000)]
plt.hist(res2, bins=range(70))

plt.show()
