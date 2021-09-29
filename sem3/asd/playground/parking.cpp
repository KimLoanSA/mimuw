#include <cstdio>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 5e5 + 7;
const int M = 5e5 + 7;

int n, m, a, b;
vector<int> G[N];
bool visited[N];
bool car[N];
vector<int> result;

void dfs(int v) {
  visited[v] = true;

  if(car[v]) {
    result.push_back(v);
  } else {
    for (auto w : G[v]) {
      if (!visited[w]) {
        dfs(w);
      }
    }
  }
}

int main() {

  scanf("%d %d", &n, &m);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &car[i]);
  }

  for (int i = 0; i < m; i++) {
    scanf("%d %d", &a, &b);

    G[a].push_back(b);
    G[b].push_back(a);
  }

  dfs(1);

  sort(result.begin(), result.end());

  for (auto it : result) {
    printf("%d\n", it);
  }

  return 0;
}