#!/bin/bash

SIM_DIR=../../sim

# On CTRL-C, call quit()
trap quit INT
function quit() {
  echo
  echo "Caught CTRL-C. Exiting."
  kill $PID 2> /dev/null
}

# Build tests
echo -n "Building tests... "
make -s
echo "done"

# Build and copy simulator to current directory
echo -n "Building simulator... "
make -s -C $SIM_DIR > /dev/null
cp $SIM_DIR/five-alive-sim .
echo done

# For each test
killall five-alive-sim 2> /dev/null
for FILE in I/*.S; do
  TEST=$(basename $FILE .S)
  echo -n "Testing $TEST... "
  cp I/$TEST.cpu.code.hex imem.mif
  cp I/$TEST.cpu.data.hex dmem.mif
  ./five-alive-sim &
  ./getresult
  killall five-alive-sim 2> /dev/null
done
