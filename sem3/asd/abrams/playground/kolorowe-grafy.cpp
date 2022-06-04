#include <cstdio>
#include <vector>
#include <utility>
#include <queue>

using namespace std;

const int N = 5e5 + 7;

int n, m, a, b, c;
vector<int> G[2][N];
bool visit[2][N];
int dist[2][N];
queue<pair<int, int> > q;

void bfs() {
  q.push(make_pair(1, 1));
  q.push(make_pair(1, 0));

  while(!q.empty()) {
    auto p = q.front();
    q.pop();
    int type = p.second;
    int v = p.first;

    visit[type][v] = 1;

    for (auto w : G[1 - type][v]) {
      if (!visit[1 - type][w]) {
        dist[1 - type][w] = dist[type][v] + 1;
        q.push(make_pair(w, 1 - type));
      }
    }
  }
}

int main() {

  scanf("%d %d", &n, &m);

  for (int i = 0; i < m; i++) {
    scanf("%d %d %d", &a, &b ,&c);

    G[c][a].push_back(b);
    G[c][b].push_back(a);
  }

  bfs();

  for (int i = 2; i <= n; i++) {
    if (dist[0][i] > 0 && dist[1][i] > 0) {
      printf("%d\n", min(dist[0][i], dist[1][i]));
    } else if (dist[0][i] == 0 && dist[1][i] > 0) {
      printf("%d\n", dist[1][i]);
    } else if (dist[1][i] == 0 && dist[0][i] > 0) {
      printf("%d\n", dist[0][i]);
    } else {
      printf("-1\n");
    }

  }

  return 0;
}

//6 5
//1 2 1
//2 3 0
//1 4 0
//4 5 0
//5 3 0