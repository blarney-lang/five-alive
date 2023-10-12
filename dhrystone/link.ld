OUTPUT_ARCH( "riscv" )

MEMORY
{
  code : ORIGIN = 0, LENGTH = 1 << (12 + 2)
  data : ORIGIN = 1 << (12 + 2), LENGTH = 1 << (12 + 2)
}

SECTIONS
{
  .text   : { *.o(.text*)   }           > code
  .bss    : { *.o(.bss*)    }           > data = 0
  .rodata : { *.o(.rodata*) }           > data
  .sdata  : { *.o(.sdata*)  }           > data
  .data   : { *.o(.data*)   }           > data
}
