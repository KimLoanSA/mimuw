#ifndef MIMUW_S4_SIK_TESTHTTP_ADDRESS_RESOLVER_H
#define MIMUW_S4_SIK_TESTHTTP_ADDRESS_RESOLVER_H

const char *get_port(const char *address);
long get_port_long(const char *address);
const char *get_host(const char *address);
const char *get_host_without_port(const char *address);
const char *get_resource(const char *address);

#endif //MIMUW_S4_SIK_TESTHTTP_ADDRESS_RESOLVER_H
