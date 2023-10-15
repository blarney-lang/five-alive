include $(FIVEALIVE_ROOT)/software/common/common.mk

# Object files
OFILES = entry.o $(patsubst %.c,%.o,$(CFILES))

.PHONY: all
all: sim-files/imem.mif  \
     sim-files/dmem.mif  \
     fpga-files/imem.mif \
     fpga-files/dmem.mif

sim-files/imem.mif: imem.ihex
	@mkdir -p sim-files
	@$(IHEX_CONV) $< hex $(IMEM_BASE) 4 $(IMEM_BYTES) 1 > $@

sim-files/dmem.mif: dmem.ihex
	@mkdir -p sim-files
	@$(IHEX_CONV) $< hex $(DMEM_BASE) 4 $(DMEM_BYTES) 1 > $@

fpga-files/imem.mif: imem.ihex
	@mkdir -p fpga-files
	@$(IHEX_CONV) $< mif $(IMEM_BASE) 4 $(IMEM_BYTES) 1 > $@

fpga-files/dmem.mif: dmem.ihex
	@mkdir -p fpga-files
	@$(IHEX_CONV) $< mif $(DMEM_BASE) 4 $(DMEM_BYTES) 1 > $@

imem.ihex: out.elf
	@$(RV_OBJCOPY) --only-section=.text -O ihex out.elf imem.ihex

dmem.ihex: out.elf
	@$(RV_OBJCOPY) -O ihex --remove-section=.text \
    --set-section-flags .bss=alloc,load,contents out.elf dmem.ihex

out.elf: $(OFILES) $(LINK_LD)
	$(RV_CC) $(CFLAGS) \
    -Wl,-Bstatic,-T,$(LINK_LD),--strip-debug \
    -o $@ $(OFILES) -lgcc

%.o: %.c
	$(RV_CC) $(CFLAGS) -c $<

entry.o: $(ENTRY_S)
	$(RV_CC) $(CFLAGS) -c $(ENTRY_S)

hl-dump-sim: $(DUMP_CPP)
	@g++ -DSIMULATE -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o hl-dump-sim $(DUMP_CPP)

hl-dump-fpga: checkenv $(DUMP_CPP)
	@g++ -std=c++11 -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o hl-dump-fpga $(DUMP_CPP) \
    -fno-exceptions -ljtag_atlantic -lpthread \
    -Wl,--no-as-needed -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

.PHONY: run
run: all hl-dump-sim
	@(killall five-alive-sim || true) 2> /dev/null
	@make -s -C $(FIVEALIVE_ROOT)/sim
	@cd sim-files && $(FIVEALIVE_ROOT)/sim/five-alive-sim &
	@./hl-dump-sim
	@(killall five-alive-sim || true) 2> /dev/null

.PHONY: clean
clean:
	rm -f *.o *.elf *.ihex hl-dump-fpga hl-dump-sim
	rm -rf sim-files fpga-files

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
