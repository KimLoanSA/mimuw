#include <cstdio>
#include <algorithm>

using namespace std;

const int N = 1e5 + 7;
const int MAX = 1e9 + 7;

int n, result;
int tab[N];

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &tab[i]);
  }

  sort(tab, tab + n);
  tab[n] = MAX;

  for (int i = 1, counter = 1; i <= n; i++) {
    if (tab[i] - tab[i - 1] <= 1) {
      counter++;
    } else {
      result = max(result, counter);
      counter = 1;
    }
  }

  printf("%d\n", result);

  return 0;
}