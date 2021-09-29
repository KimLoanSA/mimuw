#include <iostream>
#include <cstdlib>
#include <algorithm>

using namespace std;

const int N = 5 * 1e5 + 7;
const int LOG_2_N = 19;

int n, m, a, b, d;
int leftSubtree[N], rightSubtree[N], secondSubtree[N];
int maxDepth[N], subtreeDepthest[N], depth[N];
int extremeDistanceFromFather[N], extremeNodeFromFather[N];
int extremeDistance[N], extremeNode[N];
int fathers[N][LOG_2_N + 2];

int childIndex(int a) {
  return a == -1 ? n + 1 : a;
}

int secondSubtreeIndex(int a) {
  return a == -1 ? n + 2 : a;
}

void setSubtreeDepthest(int v, int depthest) {
  subtreeDepthest[v] = depthest == 0 ? v : depthest;
}

void calculateFathers(int v, int father) {
  fathers[v][0] = father;

  for (int i = 1; i <= LOG_2_N; i++) {
    fathers[v][i] = fathers[fathers[v][i - 1]][i - 1];
  }
}


void subtreeDepthDfs(int v, int father) {
  if (v < n + 1) {
    depth[v] = depth[father] + 1;

    calculateFathers(v, father);

    subtreeDepthDfs(leftSubtree[v], v);
    subtreeDepthDfs(rightSubtree[v], v);

    if (maxDepth[rightSubtree[v]] > maxDepth[leftSubtree[v]]) {
      maxDepth[v] = maxDepth[rightSubtree[v]] + 1;
      setSubtreeDepthest(v, subtreeDepthest[rightSubtree[v]]);
    } else {
      maxDepth[v] = maxDepth[leftSubtree[v]] + 1;
      setSubtreeDepthest(v, subtreeDepthest[leftSubtree[v]]);
    }
  }
}

void extremeNodeFromFatherDfs(int v, int father) {
  if (v == n + 1) {

  } else {
    extremeDistanceFromFather[v] = extremeDistanceFromFather[father] + 1;
    extremeNodeFromFather[v] = extremeNodeFromFather[father];

    if (maxDepth[secondSubtree[v]] + 2 > extremeDistanceFromFather[v]) {
      extremeDistanceFromFather[v] = maxDepth[secondSubtree[v]] + 2;
      extremeNodeFromFather[v] = subtreeDepthest[secondSubtree[v]];
    }

    extremeNodeFromFatherDfs(leftSubtree[v], v);
    extremeNodeFromFatherDfs(rightSubtree[v], v);
  }
}

void extremeNodeDfs(int v, int father) {
  if (v < n + 1) {

    extremeDistance[v] = maxDepth[v];
    extremeNode[v] = subtreeDepthest[v];

    if (extremeDistanceFromFather[v] > extremeDistance[v]) {
      extremeDistance[v] = extremeDistanceFromFather[v];
      extremeNode[v] = extremeNodeFromFather[v];
    }

    extremeNodeDfs(leftSubtree[v], v);
    extremeNodeDfs(rightSubtree[v], v);
  }
}

int distanceToAncestor(int a, int b) {
  return depth[b] - depth[a];
}


int findNodeInDDistance(int a, int dist) {
  for (int i = 0; i <= LOG_2_N; i++) {
    if (dist & (1 << i)) {
      a = fathers[a][i];
    }
  }

  return a;
}

int lcaSameDepth(int a, int b) {
  for (int i = LOG_2_N; i >= 0; i--) {
    if (fathers[a][i] != fathers[b][i]) {
      a = fathers[a][i];
      b = fathers[b][i];
    }
  }

  return fathers[a][0];
}

int findLca(int a, int b) {
  if (depth[b] > depth[a]) 
    return findLca(b, a);

  int diff = depth[a] - depth[b];
  a = findNodeInDDistance(a, diff);

  return a == b ? a : lcaSameDepth(a, b);
}


int findNode(int a, int d) {

  if (extremeNode[a] == subtreeDepthest[a]) {
    return findNode(extremeNode[a], maxDepth[a] - d);
  }

  int lca = findLca(a, extremeNode[a]);

  return distanceToAncestor(lca, a) >= d 
    ? findNodeInDDistance(a, d) 
    : findNodeInDDistance(extremeNode[a], distanceToAncestor(lca, extremeNode[a]) - d + distanceToAncestor(lca, a));
}

int query(int a, int d) {
  return d > extremeDistance[a] ? -1 : findNode(a, d);
}


int main() {
  
  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    scanf("%d %d", &a, &b);

    leftSubtree[i] = childIndex(a);
    rightSubtree[i] = childIndex(b);

    secondSubtree[a] = secondSubtreeIndex(b);
    secondSubtree[b] = secondSubtreeIndex(a);
  }

  maxDepth[n + 1] = -1;
  maxDepth[n + 2] = -3;

  extremeDistanceFromFather[n + 2] = -3;
  secondSubtree[1] = n + 2;

  subtreeDepthDfs(1, 0);
  extremeNodeFromFatherDfs(1, n + 2);
  extremeNodeDfs(1, n + 2);

  scanf("%d", &m);

  for (int z = 0; z < m; z++) {
    scanf("%d %d", &a, &d);

    printf("%d\n", query(a, d));
  }

  return 0;
}


// 8
// 3 4
// -1 6
// 2 5
// -1 -1
// 7 -1
// -1 -1
// 8 -1
// -1 -1
// 6
// 1 3
// 1 4
// 1 5
// 6 1
// 6 4
// 6 5






















