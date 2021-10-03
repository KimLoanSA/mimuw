#ifndef MIMUW_S4_SIK_RADIO_RADIOPROXYWORKER_H
#define MIMUW_S4_SIK_RADIO_RADIOPROXYWORKER_H

#include <memory>
#include <atomic>

#include "../tcp-client/tcpClient.h"

class RadioProxyWorker {
public:
  RadioProxyWorker(std::shared_ptr<TcpClient> tcpClient, std::shared_ptr<ResponseResolver> responseResolver, std::shared_ptr<AudioStreamSink> audioStreamSink);

  void work(bool metadataRead);
  void interrupt();

private:
  std::shared_ptr<TcpClient> tcpClient;
  std::shared_ptr<ResponseResolver> responseResolver;
  std::shared_ptr<AudioStreamSink> audioStreamSink;

  std::atomic_bool interrupted;


  void readAndParseHeaders() const;
  void readAndParseData(size_t metadataInterval) const;
  void handleDataAndReadMetadataIfRequired(const std::string &dataChunk) const;
  void readAndParseMetadata() const;
  void handleMetadataSizeAndReadMetadata(std::string &metadataSizeChunk) const;
};


#endif //MIMUW_S4_SIK_RADIO_RADIOPROXYWORKER_H
