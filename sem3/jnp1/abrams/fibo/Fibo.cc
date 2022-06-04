//
// Created by Marcin Abramowicz and Michał Szczęśniak
//

#include "Fibo.h"


static std::vector<uint64_t> fibSequence = {
  1ULL, 2ULL, 3ULL, 5ULL, 8ULL, 13ULL, 21ULL, 34ULL, 55ULL, 89ULL, 144ULL,
  233ULL, 377ULL, 610ULL, 987ULL, 1597ULL, 2584ULL, 4181ULL, 6765ULL, 10946ULL,
  17711ULL, 28657ULL, 46368ULL, 75025ULL, 121393ULL, 196418ULL, 317811ULL,
  514229ULL, 832040ULL, 1346269ULL, 2178309ULL, 3524578ULL, 5702887ULL,
  9227465ULL, 14930352ULL, 24157817ULL, 39088169ULL, 63245986ULL, 102334155ULL,
  165580141ULL, 267914296ULL, 433494437ULL, 701408733ULL, 1134903170ULL,
  1836311903ULL, 2971215073ULL, 4807526976ULL, 7778742049ULL, 12586269025ULL,
  20365011074ULL, 32951280099ULL, 53316291173ULL, 86267571272ULL,
  139583862445ULL, 225851433717ULL, 365435296162ULL, 591286729879ULL,
  956722026041ULL, 1548008755920ULL, 2504730781961ULL, 4052739537881ULL,
  6557470319842ULL, 10610209857723ULL, 17167680177565ULL, 27777890035288ULL,
  44945570212853ULL, 72723460248141ULL, 117669030460994ULL, 190392490709135ULL,
  308061521170129ULL, 498454011879264ULL, 806515533049393ULL,
  1304969544928657ULL, 2111485077978050ULL, 3416454622906707ULL,
  5527939700884757ULL, 8944394323791464ULL, 14472334024676221ULL,
  23416728348467685ULL, 37889062373143906ULL, 61305790721611591ULL,
  99194853094755497ULL, 160500643816367088ULL, 259695496911122585ULL,
  420196140727489673ULL, 679891637638612258ULL, 1100087778366101931ULL,
  1779979416004714189ULL, 2880067194370816120ULL, 4660046610375530309ULL,
  7540113804746346429ULL, 12200160415121876738ULL
};

// ===================== USECASE ===============================================

void Fibo::fixRecursively(size_t pos) {
  if (fibits.test(pos) && fibits.test(pos - 1)) {
    if (fibits.size() == pos + 1) {
      fibits.resize(pos + 2);
    }
    fibits[pos + 1] = 1;
    fibits[pos] = 0;
    fibits[pos - 1] = 0;
    if (fibits.size() > pos + 2) {
      fixRecursively(pos + 2);
    }
  }
}

Fibo& Fibo::normalize() {
  size_t s = fibits.size();
  if (s == 1) {
    return *this;
  }
  for (size_t i = s - 1; i >= 1; --i) {
    fixRecursively(i);
  }
  s = fibits.size();
  while (!fibits.test(s - 1) && s > 1) {
    s--;
  }
  if (s != fibits.size()) {
    fibits.resize(s);
  }
  return *this;
}

Fibo &Fibo::add(const Fibo &fibo) {
  if (fibits.size() < fibo.fibits.size()) {
    fibits.resize(fibo.fibits.size());
  } else if (fibits.size() == fibo.fibits.size()) {
    fibits.resize(fibits.size() + 1);
  }

  int64_t i = (int64_t) std::min(fibits.size(), fibo.fibits.size()) - 1;
  bool additional = false;
  while (i >= 0) {
    if (fibits.test(i) && fibo.fibits.test(i)) {
      if (fibits.size() > (uint64_t) i + 1 && fibits.test(i + 1)) {
        normalize();
        i++;
      }
      else {
        if (!additional) {
          fibits[i] = false;
        }
        additional = true;
        fibits[i + 1] = true;
        if (i == 1) {
          fibits[i - 1] = true;
        }
        i--;
      }
    } else if (additional && (fibits.test(i) || fibo.fibits.test(i))) {
      if (fibits.size() > (uint64_t) i + 1 && fibits.test(i + 1)) {
        fibits[i] = true;
        normalize();
        fibits[i] = true;
        additional = false;
      } else if (i == 1) {
        fibits[2] = true;
        if (fibits.test(0) || fibo.fibits.test(0)) {
          fibits[1] = true;
          fibits[0] = false; 
        }
        else {
          fibits[1] = false;
          fibits[0] = true;
        }
        return normalize();
      } else {
        fibits[i + 1] = true;
        fibits[i] = false;
        if (i > 0) {
          fibits[i - 1] |= fibo.fibits[i - 1];
        }
        i--;
      }
    } else {
      fibits[i] |= additional;
      additional = false;
      fibits[i] |= fibo.fibits[i];
    }
    i--;
  }
  return normalize();
}

Fibo &Fibo::fiboAnd(const Fibo &fibo) {
  size_t sizeDiff = fibo.fibits.size() - fibits.size();
  if (sizeDiff != 0) {
    fibits.resize(fibo.fibits.size());
  }
  fibits &= fibo.fibits;
  if (sizeDiff > 0) {
    fibits.resize(fibits.size() - sizeDiff);
  }
  return normalize();
}

Fibo &Fibo::fiboOr(const Fibo &fibo) {
  if (fibo.fibits.size() > fibits.size()) {
    fibits.resize(fibo.fibits.size());
  }
  for (size_t i = 0; i < fibo.fibits.size(); ++i) {
    fibits[i] |= fibo.fibits[i];
  }
  return normalize();
}

Fibo &Fibo::fiboXor(const Fibo &fibo) {
  if (fibo.fibits.size() > fibits.size()) {
    fibits.resize(fibo.fibits.size());
  }
  for (size_t i = 0; i < fibo.fibits.size(); ++i) {
    fibits[i] ^= fibo.fibits[i];
  }
  return normalize();
}

Fibo &Fibo::fiboLeftShift(size_t amnt) {
  fibits.resize(fibits.size() + amnt);
  fibits <<= amnt;
  return normalize();
}
// ===================== IMPLEMENTATION ========================================

//constuctors

Fibo::Fibo(const std::string &str) {
  const static std::regex pattern("1[01]*");
  assert(std::regex_match(str, pattern));

  fibits = boost::dynamic_bitset(str.size());
  std::string tempString = str;

  std::reverse(tempString.begin(), tempString.end());
  size_t strSize = tempString.size();

  for (size_t i = 0; i < strSize; i++) {
    if (tempString[i] == '1') {
      fibits[i] = true;
    }
  }

  normalize();
}

Fibo::Fibo(uint64_t n) {
  fibits = boost::dynamic_bitset(fibSequence.size());

  while (n > 0) {
    auto fibNumberIt = std::prev(
      std::upper_bound(fibSequence.begin(), fibSequence.end(), n));

    fibits[std::distance(fibSequence.begin(), fibNumberIt)] = true;
    n -= *fibNumberIt;
  }

  normalize();
}

Fibo::Fibo(int64_t n) {
  assert(n >= 0);

  *this = Fibo((uint64_t) n);
}

Fibo::Fibo(const char* cstr) {
  assert(cstr);

  *this = Fibo(std::string(cstr));
}


//methods
Fibo &Fibo::operator=(const Fibo &fibo) {
  fibits = boost::dynamic_bitset(fibo.fibits);

  return *this;
}

const Fibo operator+(const Fibo &fibo1, const Fibo &fibo2) {
  return Fibo(fibo1) += fibo2;
}

const Fibo operator&(const Fibo &fibo1, const Fibo &fibo2) {
  return Fibo(fibo1) &= fibo2;
}

const Fibo operator|(const Fibo &fibo1, const Fibo &fibo2) {
  return Fibo(fibo1) |= fibo2;
}

const Fibo operator^(const Fibo &fibo1, const Fibo &fibo2) {
  return Fibo(fibo1) ^= fibo2;
}

const Fibo Fibo::operator<<(size_t n) const {
  return Fibo(*this) <<= n;
}

Fibo &Fibo::operator+=(const Fibo &fibo) {
  return this->add(fibo);
}

Fibo &Fibo::operator&=(const Fibo &fibo) {
  return this->fiboAnd(fibo);
}

Fibo &Fibo::operator|=(const Fibo &fibo) {
  return this->fiboOr(fibo);
}

Fibo &Fibo::operator^=(const Fibo &fibo) {
  return this->fiboXor(fibo);
}

Fibo &Fibo::operator<<=(size_t n) {
  return this->fiboLeftShift(n);
}

bool operator==(const Fibo &fibo1, const Fibo &fibo2) {
  return fibo1.fibits == fibo2.fibits;
}

bool operator<(const Fibo &fibo1, const Fibo &fibo2) {
  if (fibo1.length() < fibo2.length()) {
    return true;
  }

  if (fibo1.length() > fibo2.length()) {
    return false;
  }

  for (auto i = (int64_t)fibo1.length() - 1; i >= 0; i--) {
    if (fibo1.fibits[i] < fibo2.fibits[i]) {
      return true;
    }

    if (fibo1.fibits[i] > fibo2.fibits[i]) {
      return false;
    }
  }

  return false;
}

bool operator<=(const Fibo &fibo1, const Fibo &fibo2) {
  return (fibo1 < fibo2) || (fibo1 == fibo2);
}

bool operator!=(const Fibo &fibo1, const Fibo &fibo2) {
  return !(fibo1 == fibo2);
}

bool operator>(const Fibo &fibo1, const Fibo &fibo2) {
  return !(fibo1 <= fibo2);
}

bool operator>=(const Fibo &fibo1, const Fibo &fibo2) {
  return (fibo1 > fibo2) || (fibo1 == fibo2);
}

size_t Fibo::length() const {
  return fibits.size();
}

std::ostream &operator<<(std::ostream& os, const Fibo &fibo) {
  os << fibo.fibits;

  return os;
}
const Fibo Zero() {
  static Fibo zero = Fibo(0);
  return zero;
}

const Fibo One() {
  static Fibo one = Fibo(1);
  return one;
}