# Compiler details
RV_ARCH    = rv32i
RV_ABI     = ilp32
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy
CFLAGS     = -mabi=$(RV_ABI) -march=$(RV_ARCH) -O2 \
  -I $(FIVEALIVE_ROOT)/inc -nostdlib -fno-builtin \
  -ffreestanding -DTIME -DRISCV
LDFLAGS    = -melf32lriscv -G 0

# Size and base of instruction and data memories
IMEM_BASE      = 0
LOG_IMEM_BYTES = $$((12 + 2))
IMEM_BYTES     = $$((1 << $(LOG_IMEM_BYTES)))
DMEM_BASE      = $(IMEM_BYTES)
LOG_DMEM_BYTES = $(LOG_IMEM_BYTES)
DMEM_BYTES     = $(IMEM_BYTES)
