#include <iostream>
#include <cstdlib>
#include <algorithm>

using namespace std;

const int N = 300 * 1000 + 7;
const long long MOD = 1e9 + 69 + 69 + 69;
const long long BASE = 37;

int n, m;
string s;
long long stringHash[N], pow[N];

int length(int l, int r) {
  return r - l + 1;
}

void hashIt() {
  for (int i = 1; i < s.size(); i++) {
    stringHash[i] = ((stringHash[i - 1] * BASE + MOD) % MOD + s[i] + MOD) % MOD;
  }
}

void generatePow() {
  pow[0] = 1;
  for (long long i = 1; i <= n; i++) {
    pow[i] = ((pow[i - 1] * BASE + MOD) % MOD + MOD) % MOD;
  }
}

long long getHash(int l, int r) {
  return (stringHash[r] - (stringHash[l - 1] * pow[length(l, r)] + MOD) % MOD + MOD) % MOD;
}

int findLength(int l1, int r1, int l2, int r2) {
  int l = 0;
  int r = min(length(l1, r1), length(l2, r2));

  while (l != r) {
    int mid = (r + l) / 2;

    if (getHash(l1, l1 + mid) == getHash(l2, l2 + mid)) {
      l = mid + 1;
    } else {
      r = mid;
    }
  }

  return r;
}

int query(int l1, int r1, int l2, int r2) {
  int foundLength = findLength(l1, r1, l2, r2);

  if (foundLength == length(l1, r1) && foundLength == length(l2, r2)) {
    return 0;
  }

  if (foundLength == length(l1, r1)) {
    return -1;
  }

  if (foundLength == length(l2, r2)) {
    return 1;
  }

  if (s[l1 + foundLength] < s[l2 + foundLength]) {
    return -1;
  }

  return 1;
}


int main() {
  ios_base::sync_with_stdio(0);

  cin >> n >> m >> s;
  s = "#" + s;

  hashIt();
  generatePow();

  for (int i = 0; i < m; i++) {
    int a, b, c, d;

    cin >> a >> b >> c >> d;

    int res = query(a, b, c, d);

    if (res == 0) {
      cout << "=" << endl;
    } else if (res == 1) {
      cout << ">" << endl;
    } else {
      cout << "<" << endl;
    }
  }

  return 0;
}

//13 3
//abaababaabaab
//8 13 7 7
//6 11 4 6
//3 5 11 13