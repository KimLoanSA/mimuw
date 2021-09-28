import math
import numpy as np
import matplotlib.pyplot as plt

N = 10000


def cube_rand():
    cube = np.random.randint(1, 5)

    if cube <= 2:
        return 0
    elif cube == 3:
        return 1
    else:
        return -1


def validate_k_prim(k_prim, x):
    return (k_prim + 1) * (k_prim + 2) >= (2 / x)


def find_k_prim(x):
    candidate = math.floor(math.sqrt(2 / x)) - 2

    return candidate if candidate > 1 else 1


def find_k(x):
    k_prim = find_k_prim(x)

    while not validate_k_prim(k_prim, x):
        k_prim += 1

    return k_prim


def sample():
    x = np.random.uniform(0, 1)

    return cube_rand() * find_k(x)


def p_not_0(k):
    return 1 / (abs(k) * (abs(k) + 1) * (abs(k) + 2))


def p(k):
    return 1 / 2 if k == 0 else p_not_0(k)


xs_1 = np.arange(-10, 11)
ys_1 = np.vectorize(p)(xs_1)

plt.plot(xs_1, ys_1)
plt.show()

xs_N = np.arange(1, N + 1)
y_n = np.array([sample() for i in xs_N])
y_n_sum = np.cumsum(y_n)
y_n_avr = y_n_sum / xs_N

plt.plot(xs_N, y_n_avr)
plt.show()

y_n_med = [np.median(y_n[:i]) for i in xs_N]

plt.plot(xs_N, y_n_med)
plt.show()
