#include "interfaces/request_resolver_test.h"

#include <stdlib.h>

void test_1();
void test_2();
void test_3();
void test_4();
void test_5();
void test_6();
void test_7();

int main(int argc, char *argv[]) {
  int test_id = argc > 1 ? atoi(argv[1]) : 0;

  switch (test_id) {
    case 1:
      test_1();
      break;
    case 2:
      test_2();
      break;
    case 3:
      test_3();
      break;
    case 4:
      test_4();
      break;
    case 5:
      test_5();
      break;
    case 6:
      test_6();
      break;
    case 7:
      test_7();
      break;
    default:
      break;
  }

  return 0;
}

void test_1() {
  send_request_test("http://www.mimuw.edu.pl:8080/plik", "src/test/resources/cookies/ciasteczka_test.txt");
}

void test_2() {
  send_request_test("http://www.mimuw.edu.pl:8080/plik", "src/test/resources/cookies/ciasteczka_test_2.txt");
}

void test_3() {
  send_request_test("http://www.mimuw.edu.pl/plik", "src/test/resources/cookies/ciasteczka_test.txt");
}

void test_4() {
  send_request_test("http://www.mimuw.edu.pl/", "src/test/resources/cookies/ciasteczka_test.txt");
}

void test_5() {
  send_request_test("http://www.mimuw.edu.pl:8080/plik", "src/test/resources/cookies/ciasteczka_test_3.txt");
}

void test_6() {
  send_request_test("http://www.mimuw.edu.pl:8080#plik", "src/test/resources/cookies/ciasteczka_test_3.txt");
}

void test_7() {
  send_request_test("http://www.mimuw.edu.pl:8080?plik=2137", "src/test/resources/cookies/ciasteczka_test_3.txt");
}