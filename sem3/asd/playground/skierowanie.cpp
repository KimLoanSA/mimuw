#include <cstdio>
#include <vector>

using namespace std;

const int N = 5e5 + 7;

int n, m, a, b;
bool result = true;
vector <int> G[N];
int visited[N];
int color[N];

void dfs(int v) {
  visited[v] = 1;

  for (auto w : G[v]) {
    if (color[w] > 0 && color[w] != 3 - color[v]) {
      result = false;
    }

    color[w] = 3 - color[v];

    if (!visited[w]) {
      dfs(w);
    }
  }
}

int main() {

  scanf("%d %d", &n, &m);

  for (int i = 0; i < m; i++) {
    scanf("%d %d",  &a, &b);

    G[a].push_back(b);
    G[b].push_back(a);
  }

  for (int i = 1; i <= n; i++) {
    if (!visited[i]) {
      color[i] = 1;

      dfs(i);
    }
  }

  if (result) {
    printf("TAK\n");
  } else {
    printf("NIE\n");
  }
  return 0;
}