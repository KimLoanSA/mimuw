PROJECT(MIMUW-S4-SIK-TestHttp)

set(CMAKE_SOURCE_DIR solve/src)

# solve
add_executable(testhttp_raw                         solve/src/main/c/testhttp_raw.c)
add_executable(program_arguments_validator          solve/src/main/c/program_arguments_validator.c)
add_executable(response_resolver                    solve/src/main/c/response_resolver.c)
add_executable(logger                               solve/src/main/c/logger.c)
add_executable(safe_memory                          solve/src/main/c/safe_memory.c)
add_executable(buffered_reader                      solve/src/main/c/buffered_reader.c)
add_executable(string_buffer                        solve/src/main/c/string_buffer.c)
add_executable(tcp_client                           solve/src/main/c/tcp_client.c)
add_executable(file_reader                          solve/src/main/c/file_reader.c)
add_executable(request_sender                       solve/src/main/c/request_sender.c)
add_executable(address_resolver                     solve/src/main/c/address_resolver.c)

# solve tests
add_executable(response_resolver_test               solve/src/test/c/response_resolver_test.c)
add_executable(string_buffer_test                   solve/src/test/c/string_buffer_test.c)
add_executable(buffered_reader_test                 solve/src/test/c/buffered_reader_test.c)
add_executable(tcp_client_test                      solve/src/test/c/tcp_client_test.c)
add_executable(file_reader_test                     solve/src/test/c/file_reader_test.c)
add_executable(address_resolver_test                solve/src/test/c/address_resolver_test.c)
add_executable(request_resolver_test                solve/src/test/c/request_resolver_test.c)

# docker tests
add_executable(response_resolver_test_impl          docker-test/src/test/c/response_resolver_test_impl.c)
add_executable(request_resolver_test_impl           docker-test/src/test/c/request_resolver_test_impl.c)