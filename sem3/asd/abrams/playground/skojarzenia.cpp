#include <cstdio>
#include <algorithm>

using namespace std;

const int N = 1e5 + 7;
const int INF = 1e9;

int n, m, a, b, c;
int G[N][3];
int dp[N];

int main() {

  scanf("%d %d", &n, &m);

  for (int i = 0; i < m; i++) {
    scanf("%d %d %d", &a, &b, &c);
    int v1 = min(a, b);
    int v2 = max(a, b);

    G[v1][v2 - v1] = c;
  }

  if (n % 2 == 1) {
    printf("-1\n");
    return 0;
  }

  for (int i = 1; i <= n; i++) {
    dp[i] = INF;
  }

  for (int i = 1; i <= n; i += 2) {
    if (G[i][1] > 0) {
      dp[i] = min(dp[i], dp[max(0, i - 2)] + G[i][1]);
    }

    if (G[i][2] > 0 && G[i + 1][2] > 0) {
      dp[i + 2] = dp[max(0, i - 2)] + G[i][2] + G[i + 1][2];
    }
  }

  if (dp[n - 1] < INF) {
    printf("%d\n", dp[n - 1]);
  } else {
    printf("-1\n");
  }

  return 0;
}