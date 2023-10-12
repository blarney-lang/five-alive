FIVEALIVE_ROOT ?= $(realpath ../)

# Common settings
include $(FIVEALIVE_ROOT)/software/common.mk

# Location of simulator
SIM = $(FIVEALIVE_ROOT)/sim

# Location of FPGA project
FPGA = $(FIVEALIVE_ROOT)/de10-pro-e

# C files to compile
CFILES = boot.c
OFILES = $(patsubst %.c,%.o,$(CFILES))

.PHONY: all
all: $(SIM)/imem.hex  \
     $(SIM)/dmem.hex  \
     $(FPGA)/imem.mif \
     $(FPGA)/dmem.mif

$(SIM)/imem.hex: imem.ihex
	./ihex-to-img.py imem.ihex hex $(IMEM_BASE) 4 $(IMEM_BYTES) 1 \
    > $(SIM)/imem.hex

$(SIM)/dmem.hex: dmem.ihex
	./ihex-to-img.py dmem.ihex hex $(DMEM_BASE) 4 $(DMEM_BYTES) 1 \
    > $(SIM)/dmem.hex

imem.ihex: out.elf
	$(RV_OBJCOPY) --only-section=.text -O ihex out.elf imem.ihex

dmem.ihex: out.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents out.elf dmem.ihex

out.elf: entry.S $(OFILES) link.ld
	$(RV_CC) $(CFLAGS) -Wall -c -o entry.o entry.S
	$(RV_LD) $(LDFLAGS) -T link.ld -o out.elf entry.o $(OFILES)

%.o: %.c $(HFILES)
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

$(FPGA)/imem.mif: imem.ihex
	./ihex-to-img.py imem.ihex mif $(IMEM_BASE) 4 $(IMEM_BYTES) 1 \
    > $(FPGA)/imem.mif

$(FPGA)/dmem.mif: dmem.ihex
	./ihex-to-img.py dmem.ihex mif $(DMEM_BASE) 4 $(DMEM_BYTES) 1 \
    > $(FPGA)/dmem.mif

hl-dump-sim: dump.cpp
	@g++ -DSIMULATE -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o hl-dump-sim dump.cpp

hl-dump-fpga: checkenv dump.cpp
	@g++ -std=c++11 -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o hl-dump-fpga dump.cpp \
    -fno-exceptions -ljtag_atlantic -lpthread \
    -Wl,--no-as-needed -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

dump-sim: hl-dump-sim
	./hl-dump-sim

dump-fpga: hl-dump-fpga
	./hl-dump-fpga

.PHONY: clean
clean:
	rm -f *.o *.elf *.ihex *.mif hl-dump-fpga hl-dump-sim

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
