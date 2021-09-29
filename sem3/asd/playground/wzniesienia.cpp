#include <cstdio>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 5e5 + 7;
const int M = 5e5 + 7;

int n, m, a, b, w;
long long res;
vector<int> V[N];

int main() {

  scanf("%d %d", &n, &m);

  for (int i = 0; i < m; i++) {
    scanf("%d %d %d", &a, &b, &w);

    V[a].push_back(w);
    V[b].push_back(w);
  }

  for (int i = 1; i <= n; i++) {
    sort(V[i].begin(), V[i].end());

    int prevW = 0;
    long long counter = 0;
    int size = V[i].size();
    for (int j = 0; j < size; j++) {
      if (prevW != V[i][j]) {
        res += (long long)(size - j) * counter;

        prevW = V[i][j];
        counter = 0;
      }

      counter++;
    }
  }

  printf("%lld", res);

  return 0;
}

//5 6
//1 2 1
//2 3 2
//3 4 3
//4 1 4
//1 5 2
//5 2 1