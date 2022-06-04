#include <cstdio>
#include <vector>
#include <unordered_map>

using namespace std;

const int N = 1e5 + 7;
const int L = 2e5 + 7;

int n, l, a, result = 1;
vector<int> tab[N];
unordered_map<int, int> map[L];

int main() {

  scanf("%d %d", &n, &l);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < l; j++) {
      scanf("%d", &a);

      tab[i].push_back(a);
    }
  }


  for (int j = 0; j < l; j++) {
    for (int i = 0; i < n; i++) {
      if (j == 0) {
        map[j][tab[i][j]] = 1;
      } else {
        map[j][tab[i][j]] = map[j - 1][tab[i][j]] + 1;
        result = max(result, map[j][tab[i][j]]);
      }
    }
  }

  printf("%d", result);

  return 0;
}