#include <CSRs/UART.h>

int putchar(int c)
{
  while (! UARTCanPut());
  UARTPut(c);
  return c;
}

int puts(const char* s)
{
  int count = 0;
  while (*s) { putchar(*s); s++; count++; }
  putchar('\n');
  return count;
}

int main()
{
  puts("Hello, World!");
  putchar('\0');
  return 0;
}
