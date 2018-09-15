#include "get_network_subnet.h"

int get_subnet_from_nic(const char* nicname, u_int32_t* in)
{
    if (!in) return EXIT_FAILURE;

    struct ifaddrs* ifal = NULL;
    if (getifaddrs(&ifal)) {
        return EXIT_FAILURE;
    }
    bool found = false;
    for (struct ifaddrs* ifa = ifal; ifa != NULL; ifa = ifa->ifa_next) {
        if (!strcmp(ifa->ifa_name, nicname) && ifa->ifa_addr->sa_family == AF_INET) {
            union sockaddru {
                struct sockaddr ifa_;
                struct sockaddr_in ifa_i;
            } acc;
            acc.ifa_ = *ifa->ifa_netmask;
            *in = acc.ifa_i.sin_addr.s_addr;
            found = true;
            break;
        }
    }
    freeifaddrs(ifal);
    if (!found) return EXIT_FAILURE;
    return EXIT_SUCCESS;
}
