#ifndef _CSRS_UART_H_
#define _CSRS_UART_H_

#define INLINE inline __attribute__((always_inline))

// Control/status registers
#define CSR_UARTRead  "0x800"
#define CSR_UARTWrite "0x801"

// Can write to UART?
INLINE int UARTCanPut()
{
  int x;
  asm volatile("csrr %0, " CSR_UARTWrite : "=r"(x));
  return x;
}

// Write to UART; assumes UARTCanPut() is true
INLINE void UARTPut(char c)
{
  asm volatile("csrw " CSR_UARTWrite ", %0\n" : : "r"(c));
}

// Receive from UART
// Bits [7:0] contain payload
// Bit [8] is valid bit
INLINE int UARTGet()
{
  int x;
  asm volatile ("csrr %0, " CSR_UARTRead : "=r"(x));
  return x;
}

#endif
