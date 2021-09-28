import math
import numpy as np
import matplotlib.pyplot as plt
import random

days = 365
v = 1 / days
sample_size = 10_000_000

stats = np.loadtxt('us_births_69_88.csv', skiprows=1, delimiter=',', dtype=int)

number_of_births = stats[:, 2]
p = sum(number_of_births)
p_n = np.array(list(number_of_births[i] / p for i in range(days)))


def add_to_stacks(index, large_stack, small_stack):
    global p_n

    if (p_n[index] >= v):
        large_stack.append(index)
    else:
        small_stack.append(index)


def balance_stacks(large_stack, small_stack):
    global p_n

    alias = np.arange(days)

    while (len(large_stack) > 0 and len(small_stack) > 0):
        large_index = large_stack.pop()
        small_index = small_stack.pop()

        alias[small_index] = large_index
        p_n[large_index] -= v - p_n[small_index]

        add_to_stacks(large_index, large_stack, small_stack)

    return alias


def transfer(v, days):
    global p_n

    large_stack = []
    small_stack = []
    alias = np.arange(days)

    for index in range(0, days):
        add_to_stacks(index, large_stack, small_stack)

    return balance_stacks(large_stack, small_stack)


def calculate_date_repetitions(dates_to_sample):
    result = list()
    used_dates = set()
    counter = 1

    for sample_date in dates_to_sample:
        if sample_date not in used_dates:
            counter += 1
            used_dates.add(sample_date)
        else:
            result.append(counter)
            used_dates.clear()
            counter = 1

    return result


buckets = [transfer(v, days)]
rand_indexes = np.random.randint(0, days, size=sample_size)
rand_p_n = np.random.uniform(0, v, size=sample_size)

is_my_alias = rand_p_n < p_n[rand_indexes]
dates_to_sample = np.where(is_my_alias, rand_indexes, np.take(buckets, rand_indexes))

plt.hist(calculate_date_repetitions(dates_to_sample), bins=range(1, 80))
plt.show()