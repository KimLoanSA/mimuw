#include <stdlib.h>
#include <cstdio>
#include <vector>
#define MAX_N 1000000

using namespace std;

int n, m, indeks;
long long minP, minNP, suffix;
long long koszta[MAX_N + 7], wynik[MAX_N + 7], maxP[MAX_N + 7], maxNP[MAX_N + 7];

int main() {

    scanf("%d", &n);

    for (int i = 1; i <= n; i++) {
        scanf("%lld", &koszta[i]);

        maxNP[i] = maxNP[i - 1];
        maxP[i] = maxP[i - 1];
        wynik[i] = -1;

        if (koszta[i] % 2 == 1) {
            maxNP[i] = koszta[i];
        } else {
            maxP[i] = koszta[i];
        }
    }

    for (int i = n; i; i--) {
        int wynikInd = n - i + 1;
        suffix += koszta[i];

        if (koszta[i] % 2 == 1) {
            minNP = koszta[i];
        } else {
            minP = koszta[i];
        }

        if (suffix % 2 == 1) {
            wynik[wynikInd] = suffix;
        } else {

            if (maxP[i - 1] > 0LL && minNP > 0) {
                wynik[wynikInd] = max(wynik[wynikInd], suffix + maxP[i - 1] - minNP);
            }

            if (maxNP[i - 1] > 0LL && minP > 0) {
                wynik[wynikInd] = max(wynik[wynikInd], suffix + maxNP[i - 1] - minP);
            }
        }
    }

    scanf("%d", &m);

    for (int i = 1; i <= m; i++) {
        scanf("%d", &indeks);

        printf("%lld\n", wynik[indeks]);
    }

    return 0;
}