#include <cstdio>
#include <unordered_map>

using namespace std;

const int N = 3e5 + 7;

int n, d;
int tab[N];
unordered_map<int, int> map;

int main() {

  scanf("%d %d", &n, &d);

  for (int i = 0; i < n; i++) {
    scanf("%d", &tab[i]);
    map[tab[i]]++;
  }

  for (int i = 0; i < n; i++) {
    if ((d == 0 && map[tab[i] + d] > 1)
      || (d != 0 && map[tab[i] + d] > 0)) {
      printf("%d %d\n", tab[i] + d, tab[i]);

      return 0;
    }
  }

  printf("NIE\n");

  return 0;
}