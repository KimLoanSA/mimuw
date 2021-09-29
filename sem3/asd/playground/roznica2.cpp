#include <cstdio>
#include <unordered_map>

using namespace std;

int n, op, a, d, counter;
unordered_map<int, bool> elements;

int main() {

  scanf("%d %d", &n, &d);

  for (int i = 0; i < n; i++) {

    scanf("%d %d", &op, &a);

    if (op > 0) {
      if (!elements[a]) {
        counter += elements[a - d] + elements[a + d];
        elements[a] = true;
      }
    } else {
      if (elements[a]) {
        counter -= elements[a - d] + elements[a + d];
        elements[a] = false;
      }
    }

    if (counter > 0) {
      printf("TAK\n");
    } else {
      printf("NIE\n");
    }
  }
  return 0;
}