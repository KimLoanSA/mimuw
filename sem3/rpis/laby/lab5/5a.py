import random
import statistics

M = 200
N = 20
SAMPLES = 1000


def estimator1(x_n):
    return (sum(x_n) / len(x_n) * 2) - 1


def estimator2(x_n):
    return max(x_n) * (len(x_n) + 1) / len(x_n) - 1


def sample_estimator1():
    x_n = [random.randrange(M) for i in range(N)]
    return estimator1(x_n)


def sample_estimator2():
    x_n = [random.randrange(M) for i in range(N)]
    return estimator2(x_n)


est1_res = [sample_estimator1() for i in range(SAMPLES)]
est2_res = [sample_estimator2() for i in range(SAMPLES)]

est1_avr = sum(est1_res) / SAMPLES
est2_avr = sum(est2_res) / SAMPLES

print("estimator 1 avr: ", est1_avr, "var: ", statistics.variance(est1_res))
print("estimator 2 avr: ", est2_avr, "var: ", statistics.variance(est2_res))
