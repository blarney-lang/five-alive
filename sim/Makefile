FIVEALIVE_ROOT = $(realpath ../)
BOOT = $(FIVEALIVE_ROOT)/software
SIM_CFLAGS = -I $(FIVEALIVE_ROOT)/inc \
             -I $(FIVEALIVE_ROOT)/sim \
             -fmax-errors=1

sim: Main.cpp ../FiveAlive.v imem.hex dmem.hex
	verilator -cc ../FiveAlive.v -exe Main.cpp -o sim \
    -Wno-UNSIGNED -Wno-CMPCONST -y $(FIVEALIVE_ROOT)/src/ \
    --x-assign unique --x-initial unique \
    -CFLAGS "$(SIM_CFLAGS)"
	make -C obj_dir -j -f VFiveAlive.mk sim
	cp obj_dir/sim .
	rm -rf obj_dir
	cp imem.hex imem.mif
	cp dmem.hex dmem.mif

../FiveAlive.v:
	cd $(FIVEALIVE_ROOT) && cabal run --verbose=0

imem.hex:
	make -C $(BOOT)

dmem.hex:
	make -C $(BOOT)

.PHONY: run
run: sim
	@./sim &
	@make -s -C $(FIVEALIVE_ROOT)/software dump-sim
	@killall sim
	

.PHONY: clean 
clean:
	rm -rf sim *.hex *.mif obj_dir
