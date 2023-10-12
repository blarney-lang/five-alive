#include <HostLink.h>

int main()
{
  HostLink hostLink;
  uint8_t result = hostLink.uart->getByte();
  if (result == 1)
    printf("ok\n");
  else 
    printf("failed(%d)\n", result >> 1);
  return 0;
}
