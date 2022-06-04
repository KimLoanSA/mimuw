#include <cstdio>
#include <unordered_map>

using namespace std;

int n, a, result, counter = 1;
unordered_map<int, int> map;

int main() {
  scanf("%d",  &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &a);

    if (map[a] == counter) {
      result++;
      counter++;
    } else {
      map[a] = counter;
    }
  }

  printf("%d", result);
}
