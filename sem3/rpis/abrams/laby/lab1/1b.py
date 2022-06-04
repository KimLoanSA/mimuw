import math
import matplotlib.pyplot as plt
import random
import statistics

d = 365

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


repeat()
res = [repeat() for i in range(1, 100000)]
plt.hist(res, bins=range(70))
plt.show()

print("avrage: ")
print(statistics.mean(res))

print("median: ")
print(statistics.median(res))

print("mode: ")
print(statistics.mode(res))

