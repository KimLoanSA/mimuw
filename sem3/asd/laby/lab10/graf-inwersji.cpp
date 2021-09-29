#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <vector>

#define F first
#define S second

using namespace std;

const int N = 1e6 + 7;

int n;
int val[N];
int rep[N];
bool inFU[N];
int mini[N];
bool used[N];
vector<int> graph[N];
pair<int, int> tab[N];
pair<int, int> resTable[N];

void init() {
  for (int i = 1; i <= n; i++) {
    rep[i] = i;
    mini[i] = i;
  }
}

int _find(int a) {
  return rep[a] == a ? a : rep[a] = _find(rep[a]);
}

void _union(int a, int b) {
  int repA = _find(a);
  int repB = _find(b);

  mini[repB] = min(mini[repB], mini[repA]);
  rep[repA] = repB;
}

int main() {
  scanf("%d", &n);

  init();

  for (int i = 1; i <= n; i++) {
    scanf("%d", &val[i]);
    tab[i].F = val[i];
    tab[i].S = i;
  }

  sort(tab + 1, tab + n + 1);

  for (int i = n; i; i--) {
    if (!inFU[tab[i].second]) {
      int j;
      for (j = tab[i].second; j <= n && !inFU[j]; j++) {
        _union(tab[i].first, val[j]);
        inFU[j] = 1;
      }

      if (j <= n && mini[_find(val[j])] < tab[i].F) {
        _union(tab[i].F, val[j]);
      }
    }
  }

  int res = 0;

  for (int i = 1; i <= n; i++) {
    graph[_find(i)].push_back(i);

    if (!used[_find(i)]) {
      res++;
      used[_find(i)] = 1;
    }
  }

  printf("%d\n", res);

  for (int i = 1; i <= n; i++) {
    if (graph[i].size() > 0) {
      printf("%lu ", graph[i].size());

      for (int v : graph[i]) {
        printf("%d ", v);
      }

      printf("\n");
    }
  }

  return 0;
}