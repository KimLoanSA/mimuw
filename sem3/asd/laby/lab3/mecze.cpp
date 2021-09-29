#include <stdlib.h>
#include <string>
#include <set>

#define M 50
#define N 40 * 1000 + 7

using namespace std;

int n, m, a;
string id[N];
set<string> mySet;
bool result = true;

int main() {

  scanf("%d %d", &n, &m);

  for (int z = 0; z < m ; z++) {
    for (int i = 0; i < n; i++) {
      scanf("%d", &a);

      if (i < n / 2) {
        id[a] += "1";
      } else {
        id[a] += "2";
      }
    }
  }


  for (int i = 1; result && i <= n; i++) {
    if (mySet.count(id[i])) {
      result = false;
    }

    mySet.insert(id[i]);
  }

  printf("%s", (result ? "TAK" : "NIE"));

  return 0;
}

// 6 3
// 4 6 1 3 5 2
// 1 4 5 2 3 6
// 1 2 6 4 5 3

// 6 3
// 4 6 1 3 5 2
// 1 4 5 2 3 6
// 1 2 3 4 5 6