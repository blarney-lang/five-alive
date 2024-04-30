# FiveAlive

FiveAlive is a 32-bit microcontroller implemented in Haskell using the
[Blarney](https://github.com/blarney-lang/blarney) library.  It is
primarily intended as a demonstration of the generic
[Five](https://github.com/blarney-lang/five) processor pipeline.  It
features:

  * RISC-V instruction set (RV32I) 
  * Formally verified five-stage pipeline
  * Optional register forwarding and branch target prediction
  * Instruction throughput up to 1.23 DMIPS/MHz
  * Clock frequency above 200MHz on Intel Stratix 10 FPGA
  * Area requirement under 1000 ALMs

## Dependencies

First, download the repo:

```sh
git clone --recursive https://github.com/blarney-lang/five-alive
```

We'll need Verilator, a RISC-V compiler, and GHC (version 9.4.5 known
working).

On Ubuntu 22.04, we can do:
```sh
sudo apt install verilator
sudo apt install gcc-riscv64-unknown-elf
sudo apt install libgmp-dev
```

For GHC 9.4.5, [ghcup](https://www.haskell.org/ghcup/) can be used.

If you have trouble meeting any of the above dependencies, and are
working only in simulation (not on FPGA), you can simply enter a
docker shell:

```sh
make shell
```

## Usage

Inside the repo, there are various things to try.  For example, to
generate the `FiveAlive.v` synthesisable verilog:

```sh
cabal run
```

To run "hello world" in simulation:

```sh
cd software/hello
make run
```

To run the Dhrystone benchmark in simulation:

```sh
cd software/dhrystone
make run
```

To run the test suite in simulation:

```sh
cd software/tests
./test.sh
```

To run the Dhrystone benchmark on the [DE10-Pro
revE](http://de10-pro.terasic.com) board:

```sh
cd de10-pro-e
make FIRMWARE=../software/dhrystone # Assumes quartus in PATH
make download-sof                   # Assumes DE10-Pro revE connected via USB
make run                            # Dump output from FPGA
```

Or, if you are feeling brave, simply:

```sh
cd de10-pro-e
make FIRMWARE=../software/dhrystone run
```

## Acknowledgements

FiveAlive was developed on the [CAPcelerate
project](https://gow.epsrc.ukri.org/NGBOViewGrant.aspx?GrantRef=EP/V000381/1),
part of the UKRI's Digital Security by Design programme.
