FIVEALIVE_ROOT ?= $(realpath ..)
FIRMWARE ?= $(FIVEALIVE_ROOT)/software/hello
NUM_SEEDS ?= 8
HOSTLINK_CABLE_ID ?= 1
QPF = DE10_Pro.qpf
DUMP_CPP = $(FIVEALIVE_ROOT)/software/common/dump.cpp
SOF = output_files/DE10_Pro.sof

.PHONY: one
one: checkenv ../FiveAlive.v imem.mif dmem.mif
	time quartus_sh --flow compile $(QPF)

.PHONY: many
many: checkenv ../FiveAlive.v imem.mif dmem.mif
	quartus_dse $(QPF)             \
    --num-seeds $$(($(NUM_SEEDS) - 1)) \
    --launcher local                   \
    --num-concurrent 4
	quartus_dse $(QPF) --report utilization
	quartus_dse $(QPF) --report fmax_summary

../FiveAlive.v: checkenv
	make -C $(FIVEALIVE_ROOT)

imem.mif:
	make -C $(FIRMWARE)
	cp $(FIRMWARE)/fpga-files/imem.mif .

dmem.mif:
	make -C $(FIRMWARE)
	cp $(FIRMWARE)/fpga-files/dmem.mif .

.PHONY: report
report: checkenv
	quartus_dse $(QPF) --report utilization
	quartus_dse $(QPF) --report fmax_summary

.PHONY: update-mif
update-mif: checkenv
	quartus_cdb --update_mif DE10_Pro.qpf
	quartus_asm DE10_Pro.qpf

.PHONY: download-sof
download-sof: checkenv
	env -u LM_LICENSE_FILE quartus_pgm \
    -m jtag -o "p;$(SOF)" -c ${HOSTLINK_CABLE_ID}

$(SOF):
	make one

hl-dump-fpga: checkenv $(DUMP_CPP)
	@g++ -std=c++11 -O2 -I $(FIVEALIVE_ROOT)/inc \
    -o hl-dump-fpga $(DUMP_CPP) \
    -fno-exceptions -ljtag_atlantic -lpthread \
    -Wl,--no-as-needed -ljtag_client \
    -L $(QUARTUS_ROOTDIR)/linux64/ -Wl,-rpath,$(QUARTUS_ROOTDIR)/linux64

.PHONY: run
run: hl-dump-fpga $(SOF)
	make download-sof
	./hl-dump-fpga

.PHONY: clean
clean:
	rm -rf tmp-clearbox synth_dumps qdb output_files
	rm -rf DE10_Pro_QSYS reset_release db dse*
	rm -f DE10_Pro.qws *.rpt *.csv *.mif hl-dump-fpga
	ls ip/DE10_Pro_QSYS/ | grep -v -E '\.ip$$' \
    | xargs -i rm -rf ip/DE10_Pro_QSYS/{}

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
