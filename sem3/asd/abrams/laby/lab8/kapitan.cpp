#include <iostream>
#include <utility>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

const int N = 200 * 1000 + 7;
const int INF = 1e9 + 69 + 69 + 69;

int n;
int dist[N];
pair<int, int> point[N], xs[N], ys[N];
vector <pair<int, int> > G[N];

struct comp {
  bool operator()(const int &a, const int &b) const {
    if (dist[a] < dist[b]) return true;
    if (dist[a] > dist[b]) return false;
    return a < b;
  }
};

set<int, comp> sett;

void dijkstra() {
  for (int i = 2; i <= n; i++) {
    dist[i] = INF;
  }

  sett.insert(1);

  while (!sett.empty()) {
    int v = *(sett.begin());
    sett.erase(sett.begin());

    for (pair<int, int> w : G[v]) {
      if (dist[v] + w.second < dist[w.first]) {
        sett.erase(w.first);
        dist[w.first] = dist[v] + w.second;
        sett.insert(w.first);
      }
    }
  }
}

int main() {

  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    int x, y;

    scanf("%d %d", &x, &y);

    point[i] = make_pair(x, y);
    xs[i] = make_pair(x, i);
    ys[i] = make_pair(y, i);
  }

  sort(xs + 1, xs + n + 1);
  sort(ys + 1, ys + n + 1);

  for (int i = 1; i <= n; i++) {
    if (i > 1) {
      G[xs[i].second].push_back(
        make_pair(xs[i - 1].second, xs[i].first - xs[i - 1].first));
      G[ys[i].second].push_back(
        make_pair(ys[i - 1].second, ys[i].first - ys[i - 1].first));
    }

    if (i < n) {
      G[xs[i].second].push_back(
        make_pair(xs[i + 1].second, xs[i + 1].first - xs[i].first));
      G[ys[i].second].push_back(
        make_pair(ys[i + 1].second, ys[i + 1].first - ys[i].first));
    }
  }

  dijkstra();

  printf("%d", dist[n]);

  return 0;
}