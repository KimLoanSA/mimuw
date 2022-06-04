#include <cstdio>
#include <unordered_map>

using namespace std;

int n, a, result;
unordered_map<int, bool> bits;

void push(int x) {
  if (bits[x]) {
    result--;
    bits[x] = false;

    push(x + 1);
  } else {
    result++;
    bits[x] = true;
  }
}

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &a);

    push(a);

    printf("%d\n", result);
  }
  return 0;
}