#include <cstdio>
#include <set>

using namespace std;

int n, a;
set<int> items;

int main() {

  scanf("%d", &n);

  for (int i = 1; i <= n; i++) {
    items.insert(i);
  }

  for (int i = 0; i < n; i++) {
    scanf("%d", &a);

    auto it = items.find(a);
    auto it2 = it;
    it2++;

    if (it == items.begin()) {
      printf("-1 ");
    } else {
      it--;
      printf("%d ", *it);
    }

    if (it2 == items.end()) {
      printf("-1\n");
    } else {
      printf("%d\n", *it2);
    }

    items.erase(a);
  }

  return 0;
}