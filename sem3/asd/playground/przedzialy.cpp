#include <cstdio>
#include <utility>
#include <algorithm>

using namespace std;

const int N = 5e5 + 7;

int n, result = 1e9 + 7;
pair<int, int> tab[N];

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d %d", &tab[i].first, &tab[i].second);
  }

  sort(tab, tab + n);

  for (int i = 1; i < n; i++) {
    int act = max(0, tab[i].first - tab[i - 1].second);
    result = min(result, act);
  }

  printf("%d", result);

  return 0;
}