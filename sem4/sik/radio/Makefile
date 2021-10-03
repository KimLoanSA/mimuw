CC = g++
CFLAGS = -Wall -Wextra -O2 -pthread -std=c++14
LDFLAGS = -Wall -Wextra -O2 -pthread -std=c++14

.PHONY: all tests clean


all: radio-proxy tests

radio-proxy: radioProxy.o audioStreamSinkFactory.o outputAudioStreamSink.o programUsagePrinter.o responseResolver.o tcpClient.o udpClientsStorage.o radioClientCommunicationParser.o udpProxyArgumentsResolver.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o udpClient.o radioClientCommunicationParser.o radioClientsConnectionWorker.o udpAudioStreamSink.o radioProxyWorker.o
	$(CC) $(LDFLAGS) -o $@ $^


radioProxy.o: src/main/cc/proxy/radioProxy.cc src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.h src/main/cc/proxy/audio-stream-sinks/audioStreamSink.h src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.h src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.h src/main/cc/utils/programUsagePrinter.h src/main/cc/proxy/response-resolver/responseResolver.h src/main/cc/proxy/tcp-client/tcpClient.h src/main/cc/utils/programUsagePrinter.h src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.h src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h src/main/cc/proxy/workers/radioClientsConnectionWorker.h src/main/cc/proxy/udp-client/udpClient.h src/main/cc/proxy/workers/radioProxyWorker.h
	$(CC) $(CFLAGS) -c $<


radioClientsConnectionWorker.o: src/main/cc/proxy/workers/radioClientsConnectionWorker.cc src/main/cc/proxy/workers/radioClientsConnectionWorker.h src/main/cc/proxy/udp-client/udpClientsStorage.h src/main/cc/proxy/udp-client/udpClient.h src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.h
	$(CC) $(CFLAGS) -c $<

radioProxyWorker.o: src/main/cc/proxy/workers/radioProxyWorker.cc src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.h src/main/cc/proxy/audio-stream-sinks/audioStreamSink.h src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.h src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.h src/main/cc/utils/programUsagePrinter.h src/main/cc/proxy/response-resolver/responseResolver.h src/main/cc/proxy/tcp-client/tcpClient.h src/main/cc/utils/programUsagePrinter.h src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.h src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h src/main/cc/proxy/workers/radioClientsConnectionWorker.h src/main/cc/proxy/udp-client/udpClient.h
	$(CC) $(CFLAGS) -c $<

audioStreamSinkFactory.o: src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.cc src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.h src/main/cc/proxy/audio-stream-sinks/audioStreamSink.h src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.h src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.h
	$(CC) $(CFLAGS) -c $<

outputAudioStreamSink.o: src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.cc src/main/cc/proxy/audio-stream-sinks/outputAudioStreamSink.h src/main/cc/proxy/audio-stream-sinks/audioStreamSink.h
	$(CC) $(CFLAGS) -c $<

udpAudioStreamSink.o: src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.cc src/main/cc/proxy/audio-stream-sinks/udpAudioStreamSink.h src/main/cc/proxy/audio-stream-sinks/audioStreamSink.h src/main/cc/proxy/udp-client/udpClient.h  src/main/cc/proxy/udp-client/udpClientsStorage.h
	$(CC) $(CFLAGS) -c $<


responseResolver.o: src/main/cc/proxy/response-resolver/responseResolver.cc src/main/cc/proxy/response-resolver/responseResolver.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolver.o: src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h src/main/cc/utils/programArgumentsParser.h src/main/cc/utils/programUsagePrinter.h
	$(CC) $(CFLAGS) -c $<

udpProxyArgumentsResolver.o: src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.cc src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.h src/main/cc/utils/programArgumentsParser.h src/main/cc/utils/programUsagePrinter.h src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.h
	$(CC) $(CFLAGS) -c $<


tcpClient.o: src/main/cc/proxy/tcp-client/tcpClient.cc src/main/cc/proxy/tcp-client/tcpClient.h src/main/cc/proxy/response-resolver/responseResolver.h
	$(CC) $(CFLAGS) -c $<


udpClient.o: src/main/cc/proxy/udp-client/udpClient.cc src/main/cc/proxy/udp-client/udpClient.h
	$(CC) $(CFLAGS) -c $<

udpClientsStorage.o: src/main/cc/proxy/udp-client/udpClientsStorage.cc src/main/cc/proxy/udp-client/udpClientsStorage.h
	$(CC) $(CFLAGS) -c $<

radioClientCommunicationParser.o: src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.cc src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.h
	$(CC) $(CFLAGS) -c $<



programArgumentsParser.o: src/main/cc/utils/programArgumentsParser.cc src/main/cc/utils/programArgumentsParser.h
	$(CC) $(CFLAGS) -c $<

programUsagePrinter.o: src/main/cc/utils/programUsagePrinter.cc src/main/cc/utils/programUsagePrinter.h
	$(CC) $(CFLAGS) -c $<



tests: programArgumentsParserTest defaultRadioProxyArgumentsResolverTest defaultRadioProxyArgumentsResolverMetadataInvalid defaultRadioProxyArgumentsResolverNoHostTest defaultRadioProxyArgumentsResolverNoPortTest defaultRadioProxyArgumentsResolverNoResourceTest defaultRadioProxyArgumentsResolverTimeout0Test responseResolverTest udpProxyArgumentsResolverTest udpClientsStorageTest radioClientCommunicationParserTest

programArgumentsParserTest: programArgumentsParserTest.o programArgumentsParser.o
	$(CC) $(LDFLAGS) -o $@ $^

programArgumentsParserTest.o: src/test/cc/programArgumentsParserTest.cc src/main/cc/utils/programArgumentsParser.h
	$(CC) $(CFLAGS) -c $<


responseResolverTest: responseResolverTest.o responseResolver.o programUsagePrinter.o audioStreamSinkFactory.o outputAudioStreamSink.o udpAudioStreamSink.o udpClient.o udpClientsStorage.o radioClientCommunicationParser.o
	$(CC) $(LDFLAGS) -o $@ $^

responseResolverTest.o: src/test/cc/responseResolverTest.cc src/main/cc/proxy/response-resolver/responseResolver.h src/main/cc/proxy/audio-stream-sinks/audioStreamSinkFactory.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverTest: defaultRadioProxyArgumentsResolverTest.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverTest.o: src/test/cc/defaultRadioProxyArgumentsResolverTest.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


udpProxyArgumentsResolverTest: udpProxyArgumentsResolverTest.o udpProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

udpProxyArgumentsResolverTest.o: src/test/cc/udpProxyArgumentsResolverTest.cc src/main/cc/proxy/program-arguments-resolvers/udpProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


udpClientsStorageTest: udpClientsStorageTest.o udpClientsStorage.o
	$(CC) $(LDFLAGS) -o $@ $^

udpClientsStorageTest.o: src/test/cc/udpClientsStorageTest.cc src/main/cc/proxy/udp-client/udpClientsStorage.h
	$(CC) $(CFLAGS) -c $<


radioClientCommunicationParserTest: radioClientCommunicationParserTest.o radioClientCommunicationParser.o
	$(CC) $(LDFLAGS) -o $@ $^

radioClientCommunicationParserTest.o: src/test/cc/radioClientCommunicationParserTest.cc src/main/cc/proxy/radio-client-communication-parser/radioClientCommunicationParser.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverMetadataInvalid: defaultRadioProxyArgumentsResolverMetadataInvalid.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverMetadataInvalid.o: src/test/cc/default-radio-proxy-arguments-resolver-tests/defaultRadioProxyArgumentsResolverMetadataInvalid.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverNoHostTest: defaultRadioProxyArgumentsResolverNoHostTest.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverNoHostTest.o: src/test/cc/default-radio-proxy-arguments-resolver-tests/defaultRadioProxyArgumentsResolverNoHostTest.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverNoPortTest: defaultRadioProxyArgumentsResolverNoPortTest.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverNoPortTest.o: src/test/cc/default-radio-proxy-arguments-resolver-tests/defaultRadioProxyArgumentsResolverNoPortTest.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverNoResourceTest: defaultRadioProxyArgumentsResolverNoResourceTest.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverNoResourceTest.o: src/test/cc/default-radio-proxy-arguments-resolver-tests/defaultRadioProxyArgumentsResolverNoResourceTest.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<


defaultRadioProxyArgumentsResolverTimeout0Test: defaultRadioProxyArgumentsResolverTimeout0Test.o defaultRadioProxyArgumentsResolver.o programArgumentsParser.o programUsagePrinter.o
	$(CC) $(LDFLAGS) -o $@ $^

defaultRadioProxyArgumentsResolverTimeout0Test.o: src/test/cc/default-radio-proxy-arguments-resolver-tests/defaultRadioProxyArgumentsResolverTimeout0Test.cc src/main/cc/proxy/program-arguments-resolvers/defaultRadioProxyArgumentsResolver.h
	$(CC) $(CFLAGS) -c $<

clean:
	rm -f *.o radio-proxy programArgumentsParserTest defaultRadioProxyArgumentsResolverTest defaultRadioProxyArgumentsResolverMetadataInvalid defaultRadioProxyArgumentsResolverNoResourceTest defaultRadioProxyArgumentsResolverNoHostTest defaultRadioProxyArgumentsResolverNoPortTest defaultRadioProxyArgumentsResolverNoResourceTest defaultRadioProxyArgumentsResolverTimeout0Test responseResolverTest udpProxyArgumentsResolverTest udpClientsStorageTest radioClientCommunicationParserTest