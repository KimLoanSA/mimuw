#include "interfaces/response_resolver_test.h"

#include <string.h>

void test_for_404_one_part_id_1();
void test_for_404_big_parts_id_2();
void test_for_404_small_parts_id_3();

void test_for_200_one_part_id_4();
void test_for_200_big_parts_id_5();
void test_for_200_one_part_id_6();
void test_for_200_one_part_id_7();

void test_for_200_one_part_chunked_id_8();
void test_for_200_big_parts_chunked_id_9();

void test_for_200_headers_id_10();
void test_for_200_chunked_tricky_header_id_11();


void run_test(const char **response_parts, size_t number_of_response_parts, size_t response_part_size);

int main(int argc, char *argv[]) {
  int test_id = argc > 1 ? atoi(argv[1]) : 0;

  switch (test_id) {
    case 1:
      test_for_404_one_part_id_1();
      break;
    case 2:
      test_for_404_big_parts_id_2();
      break;
    case 3:
      test_for_404_small_parts_id_3();
      break;
    case 4:
      test_for_200_one_part_id_4();
      break;
    case 5:
      test_for_200_big_parts_id_5();
      break;
    case 6:
      test_for_200_one_part_id_6();
      break;
    case 7:
      test_for_200_one_part_id_7();
      break;
    case 8:
      test_for_200_one_part_chunked_id_8();
      break;
    case 9:
      test_for_200_big_parts_chunked_id_9();
      break;
    case 10:
      test_for_200_headers_id_10();
      break;
    case 11:
      test_for_200_chunked_tricky_header_id_11();
      break;
    default:
      break;
  }

  return 0;
}

void test_for_404_one_part_id_1() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 404 Not Found\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Content-Length: 0\r\n"
    "Connection: Closed\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_404_big_parts_id_2() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 404 Not Found\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Ser", "ver: Apache/2.2.14 (Win32)\r\n"
    "Content-Length: 0\r\n"
    "Connection: Close", "d\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 3;
  const size_t response_part_size = 64;

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_404_small_parts_id_3() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.", "1 404 N", "ot Foun", "d\r\n"
    "Date", ": Sun, ", "18 Oct ", "2012 10", ":36:20 ", "GMT\r\n"
    "Se", "rver: A", "pache/2", ".2.14 (", "Win32)\r","\n"
    "Conten", "t-Lengt", "h: 0\r\n"
    "C", "onnecti", "on: Clo", "sed\r\n"
    "\r\n"
    };

  const size_t number_of_response_parts = 19;
  const size_t response_part_size = 7;

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_one_part_id_4() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.0 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37\r\n"
    "Set-Cookie: cookie18=22; cookie18=69\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_big_parts_id_5() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Ap", "ache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37\r\n"
    "Set-Cookie: cookie", "18=22; cookie18=69\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 3;
  const size_t response_part_size = 64;

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_one_part_id_6() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37; cookie69=Set-Cookie: 69\r\n"
    "Set-Cookie: cookie18=22; cookie18=69\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_one_part_id_7() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Content-Length: 3\r\n"
    "Connection: Closed\r\n"
    "\r\n"
    "m i m u w"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_one_part_chunked_id_8() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37\r\n"
    "Transfer-Encoding: chunked\r\n"
    "Set-Cookie: cookie18=22; cookie18=69\r\n"
    "\r\n"
    "4\r\n"
    "Wiki\r\n"
    "5\r\n"
    "pedia\r\n"
    "E\r\n"
    " in\r\n"
    "\r\n"
    "chunks.\r\n"
    "0\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_big_parts_chunked_id_9() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Ap", "ache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37\r\n"
    "Transfer-Encoding:", " chunked\r\n"
    "Set-Cookie: cookie18=22; cookie18=69\r\n"
    "\r\n"
    "4\r\n"
    "Wiki\r\n"
    "5\r\n"
    "pe", "dia\r\n"
    "E\r\n"
    " in\r\n"
    "\r\n"
    "chunks.\r\n"
    "0\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 4;
  const size_t response_part_size = 64;

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_headers_id_10() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "seT-cOoKie: cookie21=37\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void test_for_200_chunked_tricky_header_id_11() {
  const char *HTTP_RESPONSE[] = {
    "HTTP/1.1 200 OK\r\n"
    "Date: Sun, 18 Oct 2012 10:36:20 GMT\r\n"
    "Server: Apache/2.2.14 (Win32)\r\n"
    "Set-Cookie: cookie21=37\r\n"
    "transfer-encoding: gzip, chunked\r\n"
    "Set-Cookie: cookie18=22; cookie18=69\r\n"
    "\r\n"
    "4\r\n"
    "Wiki\r\n"
    "5\r\n"
    "pedia\r\n"
    "E\r\n"
    " in\r\n"
    "\r\n"
    "chunks.\r\n"
    "0\r\n"
    "\r\n"
  };

  const size_t number_of_response_parts = 1;
  const size_t response_part_size = strlen(HTTP_RESPONSE[0]);

  run_test(HTTP_RESPONSE, number_of_response_parts, response_part_size);
}

void run_test(const char **response_parts, size_t number_of_response_parts, size_t response_part_size) {
  report_for_response_test((char **) response_parts, number_of_response_parts, response_part_size);
}


