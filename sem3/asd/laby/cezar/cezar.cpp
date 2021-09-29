#include <iostream>

using namespace std;

const int N = 1e5 + 7;
const int MOD = 1e9 + 7;

int n, m;
int input[N];


long long safeSum(long long a, long long b, long long c, long  long d) {
  return (a + b + c + d) % MOD;
}

class Matrix {
public:
  long long val[2][2];

  Matrix() {
    val[0][0] = val[0][1] = val[1][0] = val[1][1] = 0;
  }

  Matrix(int value) {
    val[0][0] = val[0][1] = val[1][0] = val[1][1] = 0;

    val[value][value] = 1;
  }

  Matrix(const Matrix &matrix) {
    val[0][0] = matrix.val[0][0];
    val[1][0] = matrix.val[1][0];
    val[0][1] = matrix.val[0][1];
    val[1][1] = matrix.val[1][1];
  }

  Matrix(Matrix matrix1, Matrix matrix2) {
    long long result0010 = matrix1.val[0][0] * matrix2.val[1][0];
    long long result0110 = matrix1.val[0][1] * matrix2.val[1][0];
    long long result0100 = matrix1.val[0][1] * matrix2.val[0][0];
    long long result0000sum = matrix1.val[0][0] + matrix2.val[0][0];
    val[0][0] = safeSum(result0010, result0110, result0100, result0000sum);

    long long result0101 = matrix1.val[0][1] * matrix2.val[0][1];
    long long result0111 = matrix1.val[0][1] * matrix2.val[1][1];
    long long result0011 = matrix1.val[0][0] * matrix2.val[1][1];
    long long result0101sum = matrix1.val[0][1] + matrix2.val[0][1];
    val[0][1] = safeSum(result0101, result0111,  result0011, result0101sum);

    long long result1100 = matrix1.val[1][1] * matrix2.val[0][0];
    long long result1110 = matrix1.val[1][1] * matrix2.val[1][0];
    long long result1010 = matrix1.val[1][0] * matrix2.val[1][0];
    long long result1010sum = matrix1.val[1][0] + matrix2.val[1][0];
    val[1][0] = safeSum(result1100, result1110, result1010, result1010sum);

    long long result1101 = matrix1.val[1][1] * matrix2.val[0][1];
    long long result1111 = matrix1.val[1][1] * matrix2.val[1][1];
    long long result1011 = matrix1.val[1][0] * matrix2.val[1][1];
    long long result1111sum = matrix1.val[1][1] + matrix2.val[1][1];
    val[1][1] = safeSum(result1101, result1111, result1011, result1111sum);
  }

  Matrix reverse() {
    swap(val[1][0], val[0][1]);

    return *this;
  }

  int get() {
    return (int)safeSum(val[0][0], val[0][1], val[1][0], val[1][1]);
  }
};

typedef class _Node *Node;

class _Node {
  int value;
  int counter;
  bool reversed;

  Matrix dp;

public:
  int priority;
  Node leftSubtree;
  Node rightSubtree;

  _Node(int _value, Node leftSubtree, Node rightSubtree)
    : value(_value),
    leftSubtree(leftSubtree),
    rightSubtree(rightSubtree),
    counter(1),
    dp(_value),
    reversed(false) {
    priority = (rand() << 16) | rand();
  }

  void _setReverse() {
    reversed ^= true;
  }

  bool _isReversed() {
    return reversed;
  }

  void _push() {
    if (reversed) {
      reversed = false;

      if (leftSubtree) {
        leftSubtree->reversed ^= true;
      }

      if (rightSubtree) {
        rightSubtree->reversed ^= true;
      }

      swap(leftSubtree, rightSubtree);
    }
  }

  int _getCounter() {
    return counter;
  }

  Matrix _getMatrix() {
    return _isReversed() ? Matrix(dp).reverse() : Matrix(dp);
  }

  Matrix _getNewMatrix() {
    return Matrix(value);
  }

  void _setDp(Matrix &dp) {
    this->dp = dp;
  }

  void _setCounter(int counter) {
    this->counter = counter;
  }

  int _getDp() {
    return dp.get();
  }
};

void push(Node node) {
  if (node) {
    node->_push();
  }
}

int getCounter(Node node) {
  return node ? node->_getCounter() : 0;
}

Matrix getReversedMatrixIfNeeded(Node node) {
  return node ? node->_getMatrix() : Matrix();
}

void calculateDp(Node node) {
  Matrix leftMatrix = getReversedMatrixIfNeeded(node->leftSubtree);
  Matrix middleMatrix = node->_getNewMatrix();
  Matrix rightMatrix = getReversedMatrixIfNeeded(node->rightSubtree);

  Matrix leftRes = Matrix(leftMatrix, middleMatrix);
  Matrix result = Matrix(leftRes, rightMatrix);
  node->_setDp(result);
}

void update(Node node) {
  if (node) {
    node->_setCounter(
      getCounter(node->leftSubtree) +
        getCounter(node->rightSubtree) + 1);

    calculateDp(node);
  }
}

void merge(Node &tree, Node left, Node right) {
  push(left);
  push(right);

  if (!left) {
    tree = right;
  } else if (!right) {
    tree = left;
  } else if (left->priority > right->priority) {
    merge(left->rightSubtree, left->rightSubtree, right);
    tree = left;
  } else {
    merge(right->leftSubtree, left, right->leftSubtree);
    tree = right;
  }

  update(tree);
}

void split(Node tree, Node &left, Node &right, int key, int add = 0) {
  left = right = nullptr;

  if (tree) {
    push(tree);

    if (key <= add + getCounter(tree->leftSubtree)) {
      split(tree->leftSubtree, left, tree->leftSubtree, key, add);
      right = tree;
    } else {
      int indexShift = add + getCounter(tree->leftSubtree) + 1;
      split(tree->rightSubtree, tree->rightSubtree, right, key, indexShift);
      left = tree;
    }

    update(tree);
  }
}

int query(Node tree, int l, int r) {
  Node left, middle, right;

  split(tree, left, middle, l);
  split(middle, middle, right, r - l + 1);

  int result = middle->_getDp();

  merge(tree, left, middle);
  merge(tree, tree, right);

  return result;
}

void reverseQuery(Node node, int l, int r) {
  Node left, middle, right;

  split(node, left, middle, l);
  split(middle, middle, right, r - l + 1);

  middle->_setReverse();

  merge(node, left, middle);
  merge(node, node, right);
}

bool comparePriority(Node node1, Node node2) {
  return node1->priority > node2->priority; }

void heapify(Node node) {
  if (node) {
    Node max = node;

    if (node->leftSubtree && comparePriority(node->leftSubtree, max))
      max = node->leftSubtree;
    if (node->rightSubtree && comparePriority(node->rightSubtree, max))
      max = node->rightSubtree;
    if (max != node) {
      swap(node->priority, max->priority);
      heapify(max);
    }
  }
}

Node build(int *a, int n) {
  if (n > 0) {
    int mid = n / 2;
    Node t = new _Node(a[mid], build(a, mid), build(a + mid + 1, n - mid - 1));

    heapify(t);
    update(t);
    return t;
  }

  return nullptr;
}

void resolveQuery(Node root) {
  char c;
  int a, b;
  scanf(" %c %d %d", &c, &a, &b);

  if (c == '?') {
    printf("%d\n", query(root, a - 1, b - 1));
  } else {
    reverseQuery(root, a - 1, b - 1);
  }
}

int main() {
  scanf("%d %d", &n, &m);
  getc(stdin);

  for (int i = 0; i < n; i++) {
    input[i] = getc(stdin) == 'R' ? 1 : 0;
  }

  Node root = build(input, n);

  for (int i = 0; i < m; i++) {
    resolveQuery(root);
  }

  return 0;
}


//3 3
//GRG
//? 1 3
//O 1 2
//? 1 3