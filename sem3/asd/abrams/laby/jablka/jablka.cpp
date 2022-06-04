#include <iostream>
#include <cstdlib>
#include <algorithm>

#define N (1000 * 1000 + 7
using namespace std;

constexpr int N = 1e6 + 7;
const int MOD = 1000 * 1000 * 1000;

int n, d;
int subtreeSize[N], leftSubtree[N], rightSubtree[N], dp[N];
 
int safeMod(int value) {
  value %= MOD;

  if (value < 0) {
    value += MOD;
  }

  return value;
}

int solve() {
  int result = 0;
  leftSubtree[0] = rightSubtree[0] = n + 1; // zeby nie ifowac w dp
  dp[n] = 1;

  for (int depth = 1; depth <= d; depth++) {
    for (int i = 0; i <= n; i++) {
      dp[leftSubtree[i]] = safeMod(dp[leftSubtree[i]] + dp[i]);
      dp[rightSubtree[i]] = safeMod(dp[rightSubtree[i]] + dp[i]);

      result = safeMod(result + dp[i]);

      dp[i] = 0;
    }
  }

  return result;
}

int main() {

  scanf("%d %d", &n, &d);

  subtreeSize[0] = 1;

  for (int i = 1; i <= n; i++) {
    scanf("%d %d", &leftSubtree[i], &rightSubtree[i]);

    subtreeSize[i] = safeMod(subtreeSize[leftSubtree[i]] + subtreeSize[rightSubtree[i]] + 1);
  }

  printf("%d", safeMod(subtreeSize[n] - solve()));

  return 0;
}

// 4 4
// 0 0 
// 1 0
// 2 1
// 3 2

// 4 3
// 0 0 
// 1 0
// 2 1
// 3 2