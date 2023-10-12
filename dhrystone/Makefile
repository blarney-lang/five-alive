FIVEALIVE_ROOT ?= $(realpath ../)
  
# Common settings
include $(FIVEALIVE_ROOT)/software/common.mk

# Location of simulator
SIM = $(FIVEALIVE_ROOT)/sim

# Location of FPGA project
FPGA = $(FIVEALIVE_ROOT)/de10-pro-e

# Script to handle hex files
IHEX_CONV = $(FIVEALIVE_ROOT)/software/ihex-to-img.py

OBJS = start.o dhry_1.o dhry_2.o stdlib.o

.PHONY: all
all: $(SIM)/imem.hex  \
     $(SIM)/dmem.hex  \
     $(FPGA)/imem.mif \
     $(FPGA)/dmem.mif

dhry.elf: $(OBJS) link.ld
	$(RV_CC) $(CFLAGS) \
    -Wl,-Bstatic,-T,link.ld,-Map,dhry.map,--strip-debug \
    -o $@ $(OBJS) -lgcc

%.o: %.c
	$(RV_CC) -c $(CFLAGS) $<

%.o: %.S
	$(RV_CC) -c $(CFLAGS) $<

dhry_1.o dhry_2.o: CFLAGS += -Wno-implicit-int -Wno-implicit-function-declaration

imem.ihex: dhry.elf
	$(RV_OBJCOPY) --only-section=.text -O ihex dhry.elf imem.ihex

dmem.ihex: dhry.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents dhry.elf dmem.ihex

$(SIM)/imem.hex: imem.ihex
	$(IHEX_CONV) imem.ihex hex $(IMEM_BASE) 4 $(IMEM_BYTES) 1 \
    > $(SIM)/imem.hex

$(SIM)/dmem.hex: dmem.ihex
	$(IHEX_CONV) dmem.ihex hex $(DMEM_BASE) 4 $(DMEM_BYTES) 1 \
    > $(SIM)/dmem.hex

$(FPGA)/imem.mif: imem.ihex
	$(IHEX_CONV) imem.ihex mif $(IMEM_BASE) 4 $(IMEM_BYTES) 1 \
    > $(FPGA)/imem.mif

$(FPGA)/dmem.mif: dmem.ihex
	$(IHEX_CONV) dmem.ihex mif $(DMEM_BASE) 4 $(DMEM_BYTES) 1 \
    > $(FPGA)/dmem.mif

.PHONY: clean
clean:
	rm -rf *.o dhry.elf dhry.map dhry.hex
