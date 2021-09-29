#include <iostream>
#include <cstdlib>
#include <algorithm>
#include <vector>
#include <queue>

#define PB push_back

using namespace std;

const int N = 5 * 1e5 + 7;

int n, m, k, a, b;
int value[N], degree[N];
vector<int> G[N];
priority_queue<int> Kth;
queue<int> actualV;

void addToQueue(int v) {
  if (Kth.size() < k) {
    Kth.push(v);
  } else if (Kth.top() > v) {
    Kth.pop();
    Kth.push(v);
  }
}


void odghyzanie() {
  while(!actualV.empty()) {
    int actV = actualV.front();
    actualV.pop();

    addToQueue(value[actV]);

    for (auto w : G[actV]) {
      value[w] = max(value[w], value[actV]);
      degree[w]--;

      if (degree[w] == 0) {
        actualV.push(w);
      }
    }
  }
}


int main() {

  scanf("%d %d %d", &n, &m, &k);

  for (int i = 1; i <= n; i++) {
    scanf("%d", &value[i]);
  }

  for (int i = 0; i < m; i++) {
    scanf("%d %d", &a, &b);

    G[b].PB(a);
    degree[a]++;
  }

  for (int i = 1; i <= n; i++) {
    if (degree[i] == 0) {
      actualV.push(i);
    }
  }

  odghyzanie();
  printf("%d", Kth.top());

  return 0;
}

// 5 3 3
// 10
// 500
// 150
// 200
// 100
// 1 2
// 1 3
// 4 5