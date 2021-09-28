import numpy as np
import scipy.stats

ROWS = 2
COLUMNS = 8
ALPHA = 0.05


# Rows: women, men
# Columns: PiS, PO, Kukiz, Nowoczesna, Lewica, PSL, Razem, KORWiN
# data = np.array([ [39.7,26.4,7.5,7.1,6.6,5.0,4.2,2.8],
#                  [38.5,20.3,10.6,7.1,6.6,5.4,3.5,7.1]])

data = np.array([[17508, 11642, 3308, 3131, 2911, 2205, 1852, 1235],
                 [17672, 9318, 4865, 3259, 3029, 2479, 1606, 3259]])


def f(i, j, r_i_sum, c_i_sum,  n):
    return r_i_sum[i] * c_i_sum[j] / n


def diff(i, j, r_i_sum, c_i_sum,  n):
    return ((f(i, j, r_i_sum, c_i_sum,  n) - data[i][j]) ** 2) / f(i, j, r_i_sum, c_i_sum,  n)


def calculate(columns):
    r_i_sum = [sum(data[i][:columns]) for i in range(ROWS)]
    c_i_sum = [sum(data[j][i] for j in range(ROWS)) for i in range(columns)]
    n = sum(r_i_sum)

    s = sum(sum(diff(i, j, r_i_sum, c_i_sum,  n) for i in range(ROWS)) for j in range(columns))
    degrees_of_freedom = (ROWS - 1) * (columns - 1)
    if 1 - scipy.stats.chi2.cdf(s, degrees_of_freedom) < ALPHA:
        print("mamy podstawy, aby odrzucic hipoteze zerowa")
    else:
        print("mamy podstawy, aby przyjac hipoteze zerowa")


print("Z KORWINEM")
calculate(COLUMNS)

print("BEZ KORWINA")
calculate(COLUMNS - 1)

print("oznacza to, ze plec i pogladay polityczne nie sa niezalezne w obu przypadkach")
