import math
import numpy as np
import matplotlib.pyplot as plt
import random

square = 1
r = 1
N = 1000
A = 4


def rand():
    return random.uniform(-square, square)


def rand_point():
    return rand(), rand()


def check(x, y):
    return x ** 2 + y ** 2 <= r ** 2


def count():
    inside = sum(check(*rand_point()) for i in range(N))

    return A * inside / N


res = [count() for i in range(N)]

plt.hist(res, bins=30)
plt.show()