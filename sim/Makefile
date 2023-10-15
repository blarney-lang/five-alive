FIVEALIVE_ROOT = $(realpath ../)
BOOT = $(FIVEALIVE_ROOT)/software
SIM_CFLAGS = -I $(FIVEALIVE_ROOT)/inc \
             -I $(FIVEALIVE_ROOT)/sim \
             -fmax-errors=1

sim: Main.cpp ../FiveAlive.v
	verilator -cc ../FiveAlive.v -exe Main.cpp -o sim \
    -Wno-UNSIGNED -Wno-CMPCONST -y $(FIVEALIVE_ROOT)/src/ \
    --x-assign unique --x-initial unique \
    -CFLAGS "$(SIM_CFLAGS)"
	make -C obj_dir -j -f VFiveAlive.mk sim
	cp obj_dir/sim .
	rm -rf obj_dir

../FiveAlive.v:
	cd $(FIVEALIVE_ROOT) && cabal run --verbose=0

.PHONY: clean 
clean:
	rm -rf sim obj_dir
