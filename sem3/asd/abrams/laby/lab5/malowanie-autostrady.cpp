#include <iostream>
#include <cstdlib>
#include <algorithm>

using namespace std;

const int BASE = (1 << 20);

int n, m, a, b;
char c;
int tree[BASE * 2], lazy[BASE * 2];

int goLeft(int node) {
  return node * 2;
}

int goRight(int node) {
  return node * 2 + 1;
}

int rangeLength(int a, int b) {
  return b - a + 1;
}

int middle(int a, int b) {
  return (a + b) / 2;
}

bool isLeaf(int node) {
  return node >= BASE;
}

void pushLazy(int node, int rangeB, int rangeE) {
  if (lazy[node] == 1 && !isLeaf(node)) {
    lazy[goRight(node)] = 1;
    lazy[goLeft(node)] = 1;

    tree[goRight(node)] = rangeLength(middle(rangeB, rangeE) + 1, rangeE);
    tree[goLeft(node)] = rangeLength(rangeB, middle(rangeB, rangeE));
  }

  if (lazy[node] == -1 && !isLeaf(node)) {
    lazy[goRight(node)] = -1;
    lazy[goLeft(node)] = -1;

    tree[goRight(node)] = 0;
    tree[goLeft(node)] = 0;
  }

  lazy[node] = 0;
}

int updateLazy(int value) {
  return value == 1 ? 1 : -1;
}
int insert(int a, int b, int rangeB, int rangeE, int node, int value) {

  if (rangeB > b || rangeE < a) {
    return tree[node];
  }

  pushLazy(node, rangeB, rangeE);

  if (rangeB >= a && rangeE <= b) {
    lazy[node] = updateLazy(value);
    tree[node] = rangeLength(rangeB, rangeE) * value;
  } else {
    tree[node] = insert(a, b, rangeB, middle(rangeB, rangeE), goLeft(node), value) 
      + insert(a, b, middle(rangeB, rangeE) + 1, rangeE, goRight(node), value);
  }

  return tree[node];
}

int query() {
  return tree[1];
}

int main() {

  scanf("%d %d", &n, &m);

  for (int z = 0; z < m; z++) {
    scanf("%d %d %c",  &a, &b, &c);

    if (c == 'B') {
      insert(a, b, 0, BASE - 1, 1, 1);
    } else {
      insert(a, b, 0, BASE - 1, 1, 0);
    }

    printf("%d\n", query());
  }

  return 0;
}

// 12
// 4
// 1 5 C
// 2 10 B
// 4 6 B
// 4 7 C
