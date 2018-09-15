#ifndef INCLUDED_GET_NETWORK_SUBNET_H
#define INCLUDED_GET_NETWORK_SUBNET_H
#if __STDC_VERSION__ >= 199901L || defined(__linux__)
#   include <stdbool.h>
#else
#   define bool _Bool
#   define true 1
#   define false 0
#endif
#include <arpa/inet.h>
#include <string.h>
#include <stdlib.h>
#include <ifaddrs.h>
#include <sys/socket.h>
#include <netinet/in.h>

int get_subnet_from_nic(const char*, u_int32_t*);
#endif
