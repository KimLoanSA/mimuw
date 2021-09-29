#include<cstdio>

#define N 1007

using namespace std;

const long long mod = 1000 * 1000 * 1000;

int n;
int tab[N];
long long dpFirst[N][N], dpLast[N][N];


int main() {

  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &tab[i]);

    dpLast[1][i] = 1;
  }

  for (int dl = 2; dl <= n; dl++) {

    for (int i = 1; i <= n - dl + 1; i++) {

      // ja pierwszy

      if (tab[i] < tab[i + 1]) {
        dpFirst[dl][i] = dpFirst[dl - 1][i + 1];
      }

      if (tab[i] < tab[i + dl - 1]) {
        dpFirst[dl][i] += dpLast[dl - 1][i + dl - 1];
      }

      // ja drugi

      if (tab[i + dl - 1] > tab[i]) {
        dpLast[dl][i + dl - 1] = dpFirst[dl - 1][i];
      }

      if (tab[i + dl - 1] > tab[i + dl - 2]) {
        dpLast[dl][i + dl - 1] += dpLast[dl - 1][i + dl - 2];
      }

      dpFirst[dl][i] %= mod;
      dpLast[dl][i] %= mod;

      // printf("%d %d\n", dpFirst[dl][i], dpLast[dl][i + dl - 1]);
    } 

    // printf("\n");
  }


  printf("%lld", (dpFirst[n][1] + dpLast[n][n]) % mod);

	return 0;
}