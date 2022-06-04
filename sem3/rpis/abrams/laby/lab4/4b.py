import math
import numpy as np
import matplotlib.pyplot as plt
import random

N = 1000


def f(x):
    return 2 * math.sqrt(1 - x ** 2)


def get_f():
    return f(random.uniform(-1, 1))


avr = sum(get_f() for i in range(N)) / N

print(avr * 2)

