# network-basal

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/80780d7eaab14e6eb3e6134bf53e77ca)](https://www.codacy.com/app/falgon/network-basal_2?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=falgon/network-basal&amp;utm_campaign=Badge_Grade)
[![MIT License](http://img.shields.io/badge/license-MIT-blue.svg?style=flat)](LICENSE)

A low layer utility about network packets implemented by haskell (Currently Linux only).
This project is not for practical because it was implemented for learning, so there is currently no plan to upload it to hackage.

This includes the implementation below.

* ARP
* ICMP
    * Link Layer (Normally it is not necessary to send from the link layer, but I decided to create all of the Ether header, IP header and ICMP data for learning.)
    * Network Layer
* Wake-On-Lan
* Subnet calculation, derivation of default route gateway, etc.


## build

```sh
$ stack build
```

## tests

This project has been tested and passed in the vagrant environment in the [testenv](https://github.com/falgon/network-basal/tree/master/testenv) directory.

```sh
$ git clone https://github.com/falgon/network-basal.git
$ mv network-basal/testenv . && mv network-basal testenv && cd testenv
$ vagrant up
$ vagrant ssh -c 'sudo stack test --allow-different-user --stack-yaml /vagrant/network-basal/stack.yaml' node1
```

## run apps

In this project, several simple applications are implemented.

### ping-exe

Send an ICMP Echo Request using the PF\_INET socket.

```sh
$ sudo stack --allow-different-user exec ping-exe -- --help
usage: ping-exe [-c count] [-t timeout] [-i wait] host
$ sudo stack --allow-different-user exec ping-exe -- -c 1 8.8.8.8
PING 8.8.8.8: 56 data bytes
64 bytes from google-public-dns-a.google.com: icmp_seq=1 ttl=63 time=9.465934s

--- ping statics ---
1 packets transmitted, 1 received, 0% packet loss
```

### ping-exe2

Builds an Ethernet packet on its own and issues an ICMP Echo request.

```sh
sudo stack --allow-different-user exec ping-exe2 -- -c 1 8.8.8.8
PING 8.8.8.8: 56 data bytes
64 bytes from google-public-dns-a.google.com: icmp_seq=1 ttl=63 time=11.432482s

--- ping statics ---
1 packets transmitted, 1 received, 0% packet loss
```

### arp-exe

Sends an ARP request and gets the MAC address corresponding to the IP address passed in the argument.

```sh
$ sudo stack --allow-different-user exec arp-exe -- eth2 192.168.33.12
Just 08:00:27:8b:b4:ae
```

## References

### ARP
* RFC 826 - An Ethernet Address Resolution Protocol: Or Converting Network Protocol Addresses to 48.bit Ethernet Address for Transmission on Ethernet Hardware
* RFC 5227 - IPv4 Address Conflict Detection
* RFC 5494 - IANA Allocation Guidelines for the Address Resolution Protocol (ARP)
* [Address Resolution Protocol (ARP) Parameters](https://www.iana.org/assignments/arp-parameters/arp-parameters.xhtml)

### ICMP
* RFC 792 - INTERNET CONTROL MESSAGE PROTOCOL
* RFC 1812 - Requirements for IP Version 4 Routers
* RFC 4884 - Extended ICMP to Support Multi-Part Messages
* [Internet Control Message Protocol (ICMP) Parameters](https://www.iana.org/assignments/icmp-parameters/icmp-parameters.xhtml)

### WOL
* [Magic Packet Technology (PDF)](https://support.amd.com/TechDocs/20213.pdf)
* [Wake-On-Lan - Wikipedia](https://en.wikipedia.org/wiki/Wake-on-LAN)
