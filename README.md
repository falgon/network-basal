# network-basal

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
