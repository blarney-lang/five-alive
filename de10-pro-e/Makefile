FIVEALIVE_ROOT ?= $(realpath ..)
SOFTWARE ?= $(FIVEALIVE_ROOT)/software/hello
NUM_SEEDS ?= 8
HOSTLINK_CABLE_ID ?= 1
QPF = DE10_Pro.qpf

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
	make -C $(SOFTWARE)
	cp $(SOFTWARE)/fpga-files/imem.mif .

dmem.mif:
	make -C $(SOFTWARE)
	cp $(SOFTWARE)/fpga-files/dmem.mif .

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
    -m jtag -o "p;output_files/DE10_Pro.sof" -c ${HOSTLINK_CABLE_ID}

.PHONY: clean
clean:
	rm -rf tmp-clearbox synth_dumps qdb output_files
	rm -rf DE10_Pro_QSYS reset_release db dse*
	rm -f DE10_Pro.qws *.rpt *.csv *.mif
	ls ip/DE10_Pro_QSYS/ | grep -v -E '\.ip$$' \
    | xargs -i rm -rf ip/DE10_Pro_QSYS/{}

# Raise error if QUARTUS_ROOTDIR not set
.PHONY: checkenv
checkenv:
	$(if $(value QUARTUS_ROOTDIR), , $(error Please set QUARTUS_ROOTDIR))
