FIVEALIVE_ROOT ?= $(realpath ../)
IHEX_CONV = $(FIVEALIVE_ROOT)/software/ihex-to-img.py

# Common settings
include $(FIVEALIVE_ROOT)/software/common.mk

# RISC-V subset
RV_ARCH = rv32i
RV_ABI = ilp32

# Compiler setup
CFLAGS += -I./inc

# Helper function
hex-files-for = $(patsubst %.S,%.$(1).code.hex,$(wildcard $(2)/*.S)) \
                $(patsubst %.S,%.$(1).data.hex,$(wildcard $(2)/*.S))

.PHONY: all
all: hex-files getresult

.PHONY: hex-files
hex-files: $(call hex-files-for,cpu,I)

%.code.hex: %.code.ihex
	$(IHEX_CONV) $< hex $(IMEM_BASE) 4 $(IMEM_BYTES) 1 > $@

%.data.hex: %.data.ihex
	$(IHEX_CONV) $< hex $(DMEM_BASE) 4 $(DMEM_BYTES) 1 > $@

%.code.ihex: %.elf
	@$(RV_OBJCOPY) -O ihex --only-section=.text $< $@

%.data.ihex: %.elf
	@$(RV_OBJCOPY) -O ihex --remove-section=.text \
                 --set-section-flags .bss=alloc,load,contents $< $@

.PRECIOUS: %.cpu.elf
%.cpu.elf: %.cpu.o link.ld
	@$(RV_LD) $(LDFLAGS) -T link.ld -o $@ $<

.PRECIOUS: %.cpu.o
%.cpu.o: %.S
	@$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

getresult: getresult.cpp
	@g++ -DSIMULATE -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o getresult getresult.cpp

.PHONY: clean
clean:
	@rm -f *.o I/*.o I/*.elf I/*.hex *.mif sim getresult
