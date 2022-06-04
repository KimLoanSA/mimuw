#include <memory>
#include <thread>
#include <csignal>

#include "audio-stream-sinks/audioStreamSinkFactory.h"
#include "response-resolver/responseResolver.h"
#include "tcp-client/tcpClient.h"
#include "program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h"
#include "program-arguments-resolvers/udpProxyArgumentsResolver.h"
#include "workers/radioClientsConnectionWorker.h"
#include "workers/radioProxyWorker.h"

void catchSiging(int signal);
void handleSigint();
void workA(DefaultRadioProxyArgumentsResolver &defaultRadioProxyArgumentsResolver, char* programName);
void workB(DefaultRadioProxyArgumentsResolver &defaultRadioProxyArgumentsResolver, UdpProxyArgumentsResolver &udpProxyArgumentsResolver, char* programName);

std::unique_ptr<RadioProxyWorker> radioProxyWorker;
std::shared_ptr<RadioClientsConnectionWorker> radioClientsConnectionWorker;
bool isWokB;

int main(int argc, char *argv[]) {
  handleSigint();

  DefaultRadioProxyArgumentsResolver defaultRadioProxyArgumentsResolver(argc, argv);
  UdpProxyArgumentsResolver udpProxyArgumentsResolver(argc, argv);

  isWokB = udpProxyArgumentsResolver.isPortDefined();

  if (!isWokB) {
    workA(defaultRadioProxyArgumentsResolver, argv[0]);
  } else {
    workB(defaultRadioProxyArgumentsResolver, udpProxyArgumentsResolver, argv[0]);
  }

  return 0;
}

void catchSiging(__attribute__((unused))  int signal) {
  radioProxyWorker->interrupt();

  if (isWokB) {
    radioClientsConnectionWorker->interrupt();
  }
}

void handleSigint() {
  struct sigaction action;

  action.sa_handler = catchSiging;
  action.sa_flags = 0;

  if (sigaction(SIGINT, &action, nullptr) == -1) {
    exit(1);
  }
}

void workB(DefaultRadioProxyArgumentsResolver &defaultRadioProxyArgumentsResolver, UdpProxyArgumentsResolver &udpProxyArgumentsResolver, char* programName) {
  std::string host = defaultRadioProxyArgumentsResolver.getHost();
  std::string port = std::to_string(defaultRadioProxyArgumentsResolver.getPort());
  std::string resource = defaultRadioProxyArgumentsResolver.getResource();

  const int timeout = defaultRadioProxyArgumentsResolver.getTimeoutOrDefault();
  const bool metadata = defaultRadioProxyArgumentsResolver.getMetadataOrDefault();

  std::shared_ptr<UdpClientsStorage> udpClientsStorage = std::make_shared<UdpClientsStorage>(udpProxyArgumentsResolver.getTimeoutOrDefault());
  std::shared_ptr<UdpClient> udpClient
    = std::make_shared<UdpClient>(udpProxyArgumentsResolver.getPort(), udpProxyArgumentsResolver.getMulticastAddress(), udpProxyArgumentsResolver.isMulticastAddressDefined());

  std::shared_ptr<ResponseResolver> responseResolver = std::make_shared<ResponseResolver>(metadata, programName);
  std::shared_ptr<TcpClient> tcpClient = std::make_shared<TcpClient>(host, port, resource, timeout);
  std::shared_ptr<AudioStreamSink> audioStreamSink = AudioStreamSinkFactory::udpAudioStreamSink(udpClient, udpClientsStorage);

  radioClientsConnectionWorker = std::make_shared<RadioClientsConnectionWorker>(udpClient, udpClientsStorage);

  std::thread radioClientsConnectionWorkerThread([host, port, resource]{radioClientsConnectionWorker->work(host, port, resource);});

  radioProxyWorker = std::make_unique<RadioProxyWorker>(tcpClient, responseResolver, audioStreamSink);
  radioProxyWorker->work(metadata);

  radioClientsConnectionWorkerThread.join();
}


void workA(DefaultRadioProxyArgumentsResolver &defaultRadioProxyArgumentsResolver, char* programName) {
  std::string host = defaultRadioProxyArgumentsResolver.getHost();
  std::string port = std::to_string(defaultRadioProxyArgumentsResolver.getPort());
  std::string resource = defaultRadioProxyArgumentsResolver.getResource();

  const int timeout = defaultRadioProxyArgumentsResolver.getTimeoutOrDefault();
  const bool metadata = defaultRadioProxyArgumentsResolver.getMetadataOrDefault();

  std::shared_ptr<ResponseResolver> responseResolver = std::make_shared<ResponseResolver>(metadata, programName);
  std::shared_ptr<TcpClient> tcpClient = std::make_shared<TcpClient>(host, port, resource, timeout);
  std::shared_ptr<AudioStreamSink> audioStreamSink = AudioStreamSinkFactory::outputAudioStreamSink();

  radioProxyWorker = std::make_unique<RadioProxyWorker>(tcpClient, responseResolver, audioStreamSink);
  radioProxyWorker->work(metadata);
}
//./radio-proxy -h 178.32.107.151  -p 3639 -r /stream