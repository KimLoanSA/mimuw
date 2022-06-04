//
// Created by Marcin Abramowicz and Michał Szczęśniak
//

#ifndef JNPI_FIBO_FIBO_H
#define JNPI_FIBO_FIBO_H

#include <string>
#include <ostream>
#include <boost/dynamic_bitset.hpp>
#include <regex>
#include <algorithm>

class Fibo {

  private:
    boost::dynamic_bitset<> fibits;

    //methods
    void fixRecursively(size_t pos);
    Fibo& normalize();
    Fibo& add(const Fibo &fibo);
    Fibo& fiboAnd(const Fibo &fibo);
    Fibo& fiboOr(const Fibo &fibo);
    Fibo& fiboXor(const Fibo &fibo);
    Fibo& fiboLeftShift(size_t amnt);

  public:
    //constructors
    explicit Fibo(const std::string& str);
    Fibo(const char* cstr);
    Fibo(uint64_t n);
    Fibo(uint32_t n) : Fibo((uint64_t) n) {};
    Fibo(uint16_t n) : Fibo((uint64_t) n) {};
    Fibo(uint8_t n) : Fibo((uint64_t) n) {};
    Fibo(int64_t n);
    Fibo(int32_t n) : Fibo((int64_t) n) {};
    Fibo(int16_t n) : Fibo((int64_t) n) {};
    Fibo(int8_t n) : Fibo((int64_t) n) {};
    Fibo(const Fibo &fibo) : fibits(fibo.fibits) {};
    Fibo(): Fibo(0) {};

    //deleted constructors
    Fibo(bool n) = delete;
    Fibo(const char c) = delete;


    //methods
    Fibo &operator=(const Fibo &fibo);
    friend const Fibo operator+(const Fibo &fibo1, const Fibo &fibo2);
    friend const Fibo operator&(const Fibo &fibo1, const Fibo &fibo2);
    friend const Fibo operator|(const Fibo &fibo1, const Fibo &fibo2);
    friend const Fibo operator^(const Fibo &fibo1, const Fibo &fibo2);
    const Fibo operator<<(size_t n) const;

    Fibo &operator+=(const Fibo &fibo);
    Fibo &operator&=(const Fibo &fibo);
    Fibo &operator|=(const Fibo &fibo);
    Fibo &operator^=(const Fibo &fibo);
    Fibo &operator<<=(size_t n);

    friend bool operator==(const Fibo &fibo1, const Fibo &fibo2);
    friend bool operator<(const Fibo &fibo1, const Fibo &fibo2);
    friend bool operator<=(const Fibo &fibo1, const Fibo &fibo2);
    friend bool operator!=(const Fibo &fibo1, const Fibo &fibo2);
    friend bool operator>(const Fibo &fibo1, const Fibo &fibo2);
    friend bool operator>=(const Fibo &fibo1, const Fibo &fibo2);

    friend std::ostream &operator<<(std::ostream& os, const Fibo &fibo);

    size_t length() const;

};

const Fibo Zero();
const Fibo One();

#endif //JNPI_FIBO_FIBO_H
