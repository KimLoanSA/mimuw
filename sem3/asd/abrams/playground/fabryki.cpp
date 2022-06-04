#include <cstdio>
#include <vector>

using namespace std;

const int N = 2e5 + 7;

int n, a, b, result = 0;
vector<int> G[N];
int type[N], typeRep[12];
int dist[N];

void dfs(int v, int f) {
  dist[v] = dist[f] + 1;

  for (auto w : G[v]) {
    if (dist[w] == -1) {
      dfs(w, v);
    }
  }
}

void initDist() {
  for (int i = 0; i <= n; i++) {
    dist[i] = -1;
  }
}

int maxDistanced(int typeFilter) {
  int w = 0, d = -1;

  for (int i = 1; i <= n; i++) {
    if (dist[i] > d && type[i] == typeFilter) {
      d = dist[i];
      w = i;
    }
  }

  return w;
}


int main() {

  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &type[i]);
    typeRep[type[i]] = i;
  }

  for (int i = 1; i < n; i++) {
    scanf("%d %d", &a, &b);

    G[a].push_back(b);
    G[b].push_back(a);
  }

  for (int t = 1; t <= 10; t++) {
    if (typeRep[t] > 0) {
      int v = typeRep[t];

      initDist();
      dfs(v, 0);
      int maxV = maxDistanced(t);

      initDist();
      dfs(maxV, 0);
      int resultV = maxDistanced(t);

      result = max(result, dist[resultV]);
    }
  }

  printf("%d", result);

  return 0;
}