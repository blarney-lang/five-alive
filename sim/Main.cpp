#include <stdint.h>
#include <verilated.h>
#include <JTAGUART.h>
#include "VFiveAlive.h"

VFiveAlive *top;
vluint64_t main_time = 0;

// Called by $time in Verilog
double sc_time_stamp () {
  return main_time;
}

int main(int argc, char** argv) {
  Verilated::commandArgs(argc, argv);

  // Instantiate SoC
  top = new VFiveAlive;
  
  // JTAG UART simulator
  JTAGUART uart;
  uart.ifc.readdata = &top->in0_avl_jtaguart_readdata;
  uart.ifc.waitrequest = &top->in0_avl_jtaguart_waitrequest;
  uart.ifc.writedata = &top->out_avl_jtaguart_writedata;
  uart.ifc.address = &top->out_avl_jtaguart_address;
  uart.ifc.read = &top->out_avl_jtaguart_read;
  uart.ifc.write = &top->out_avl_jtaguart_write;

  while (!Verilated::gotFinish()) {
    int reset = top->reset = main_time < 1;
    if (reset == 0) {
      uart.tick();
    }
    top->clock = 0; top->eval();
    top->clock = 1; top->eval();
    main_time++;
  }

  top->final();
  delete top;

  return 0;
}
