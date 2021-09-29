#include <cstdio>
#include <set>
#include <utility>
#include <unordered_map>

using namespace std;

const int N = 3e5 +7;

int n, x, a, b;
long long result;
unordered_map<int, set<pair<int, int> > > odcinki;
set<int> iksy;

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d %d %d", &x, &a, &b);

    odcinki[x].insert(make_pair(a, 1));
    odcinki[x].insert(make_pair(b, -1));
    iksy.insert(x);
  }

  for (auto x : iksy) {
    int licznik = 0;
    auto aktualneOdcinki = odcinki[x];

    for (auto odc : aktualneOdcinki) {
      if (odc.second > 0) {
        result += licznik;
      }
      licznik += odc.second;
    }
  }

  printf("%lld", result);

  return 0;
}