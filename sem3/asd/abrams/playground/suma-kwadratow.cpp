#include <cstdio>
#include <set>

using namespace std;

int n, a;
long long result;
set<int> biggerElements;
set<int> smallerElements;

long long square(long long a) {
  return a * a;
}

int main() {

  scanf("%d", &n);

  for (int i = 0; i < n; i++) {
    scanf("%d", &a);

    if (biggerElements.empty()) {
      biggerElements.insert(a);
      smallerElements.insert(-a);
    } else {
      auto bigger = biggerElements.upper_bound(a);
      auto smaller = smallerElements.upper_bound(-a);

      if (bigger != biggerElements.end() && smaller != smallerElements.end()) {
        int biggerInt = *bigger;
        int smallerInt = -(*smaller);

        result -= square(biggerInt - smallerInt);

        result += square(biggerInt - a);
        result += square(a - smallerInt);
      } else if (bigger != biggerElements.end()) {
        int biggerInt = *bigger;
        int smallerInt = -(*smaller);

        result += square(biggerInt - a);
      } else {
        int biggerInt = *bigger;
        int smallerInt = -(*smaller);

        result += square(a - smallerInt);
      }

      biggerElements.insert(a);
      smallerElements.insert(-a);

      printf("%lld\n", result);
    }
  }

  return 0;
}

//5
//4
//2
//10
//7
//12