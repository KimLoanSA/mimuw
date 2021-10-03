#include <sys/socket.h>
#include <netdb.h>
#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <cstdlib>
#include <iostream>
#include <utility>

#include "tcpClient.h"


TcpClient::TcpClient(std::string host, std::string port, std::string resource, int timeout) :
  host(std::move(host)),
  port(std::move(port)),
  resource(std::move(resource)),
  timeout(timeout) {

  struct addrinfo *addrResult;
  struct addrinfo addrHints;
  memset(&addrHints, 0, sizeof(struct addrinfo));

  addrHints.ai_family = AF_INET;
  addrHints.ai_socktype = SOCK_STREAM;
  addrHints.ai_protocol = IPPROTO_TCP;

  if (getaddrinfo(this->host.c_str(), this->port.c_str(), &addrHints, &addrResult) != 0) {
    exit(1);
  }

  initSocket(addrResult);
  setTimeout();
  startConnection(addrResult);

  freeaddrinfo(addrResult);

  connectionFile = fdopen(socketId, "r+");
}

void TcpClient::initSocket(struct addrinfo *addrResult) {
  socketId = socket(addrResult->ai_family, addrResult->ai_socktype, addrResult->ai_protocol);
  if (socketId < 0) {
    exit(1);
  }
}

void TcpClient::setTimeout() {
  struct timeval timeval;
  timeval.tv_sec = timeout;
  timeval.tv_usec = 0;

  if (setsockopt (socketId, SOL_SOCKET, SO_RCVTIMEO, (char *)&timeval, sizeof(timeval)) < 0) {
    exit(1);
  }

  if (setsockopt (socketId, SOL_SOCKET, SO_SNDTIMEO, (char *)&timeval, sizeof(timeval)) < 0) {
    exit(1);
  }
}

void TcpClient::startConnection(struct addrinfo *addrResult) {
  if (connect(socketId, addrResult->ai_addr, addrResult->ai_addrlen) < 0) {
    exit(1);
  }
}


void TcpClient::sentRequest(bool metadataRequired) {
  int requestCode = 0;

  if (metadataRequired) {
    requestCode = sentRequestMetadata();
  } else {
    requestCode = sendRequestNoMetadata();
  }

  if (requestCode < 0) {
    exit(1);
  }
}

int TcpClient::sendRequestNoMetadata() {
  return  fprintf(connectionFile,
    "GET %s HTTP/1.1\r\n"
    "Host: %s:%s\r\n"
    "Accept: */*\r\n"
    "Connection: close\r\n"
    "\r\n", resource.c_str(), host.c_str(), port.c_str());
}

int TcpClient::sentRequestMetadata() {
  return  fprintf(connectionFile,
    "GET %s HTTP/1.1\r\n"
    "Host: %s:%s\r\n"
    "Accept: */*\r\n"
    "Icy-MetaData: 1\r\n"
    "Connection: close\r\n"
    "\r\n", resource.c_str(), host.c_str(), port.c_str());
}


std::string TcpClient::getResponseLine() {
  char *line = NULL;
  size_t len = 0;
  hasPreviousReadBeenInterruptedFlag = false;

  if (getline(&line, &len, connectionFile) < 0) {
    checkErrnoAndUpdateFlagOrExit();
  }

  return std::string(line);
}


std::string TcpClient::getResponseChunk(size_t chunkSize) {
  std::string result(chunkSize, '\0');
  hasPreviousReadBeenInterruptedFlag = false;

  if (fread(&result[0], 1, chunkSize, connectionFile) < chunkSize) {
    checkErrnoAndUpdateFlagOrExit();
  }

  return result;
}

bool TcpClient::hasPreviousReadBeenInterrupted() {
  return hasPreviousReadBeenInterruptedFlag;
}

TcpClient::~TcpClient() {
  fclose(connectionFile);
  close(socketId);
}

void TcpClient::checkErrnoAndUpdateFlagOrExit() {
  if (errno == EINTR) {
    hasPreviousReadBeenInterruptedFlag = true;
  } else {
    exit(1);
  }
}


