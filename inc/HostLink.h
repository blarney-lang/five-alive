#ifndef _HOSTLINK_H_
#define _HOSTLINK_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <HostLink/JTAGUARTBuffer.h>

#ifdef SIMULATE
#define IsSimulation 1
#else
#define IsSimulation 0
#endif

class HostLink {

 public:

  // Access to SoC via UART
  JTAGUARTBuffer* uart;

  // Constructor
  HostLink() {
    uart = new JTAGUARTBuffer;
  }

  // Destructor
  ~HostLink() {
    delete uart;
  }

  // Dumping data from the UART
  // --------------------------

  // Receive bytes from SoC and display on stdout
  // Returns when an '\0' is observed
  void dump() {
     while (1) {
       uint8_t b = uart->getByte();
       if (b == '\0') break;
       printf("%c", b);
     }
  }
};

#endif
