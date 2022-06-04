PROJECT(MIMUW-S4-SIK-Radio)

set(CMAKE_SOURCE_DIR src)
set (CMAKE_CXX_STANDARD 14)

# main
# proxy
add_executable(radio-proxy                                              src/main/cc/proxy/radioProxy.cc)
add_executable(radioClientsConnectionResolver                           src/main/cc/proxy/workers/radioClientsConnectionWorker.cc)
add_executable(radioProxyWorker                                         src/main/cc/proxy/workers/radioProxyWorker.cc)

add_executable(audioStreamSinkFactory                                   src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.cc)
add_executable(outputAudioStreamSink                                    src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.cc)
add_executable(udpAudioStreamSink                                       src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.cc)

add_executable(defaultRadioProxyArgumentsResolver                       src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.cc)
add_executable(udpProxyArgumentsResolver                                src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.cc)

add_executable(responseResolver                                         src/main/cc/proxy/response-resolver/responseResolver.cc)

add_executable(udpClientsStorage                                        src/main/cc/proxy/udp-client/udpClientsStorage.cc)
add_executable(udpClient                                                src/main/cc/proxy/udp-client/udpClient.cc)

add_executable(radioClientCommunicationParser                           src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.cc)

add_executable(tcpClient                                                src/main/cc/proxy/tcp-client/tcpClient.cc)

add_executable(programArgumentsParser                                   src/main/cc/utils/programArgumentsParser.cc)
add_executable(programUsagePrinter                                      src/main/cc/utils/programUsagePrinter.cc)


# test
add_executable(programArgumentsParserTest                               src/test/cc/programArgumentsParserTest.cc)
add_executable(defaultRadioProxyArgumentsResolverTest                   src/test/cc/defaultRadioProxyArgumentsResolverTest.cc)
add_executable(udpProxyArgumentsResolverTest                            src/test/cc/udpProxyArgumentsResolverTest.cc)
add_executable(responseResolverTest                                     src/test/cc/responseResolverTest.cc)
add_executable(udpClientsStorageTest                                    src/test/cc/udpClientsStorageTest.cc)
add_executable(radioClientCommunicationParserTest                       src/test/cc/radioClientCommunicationParserTest.cc)