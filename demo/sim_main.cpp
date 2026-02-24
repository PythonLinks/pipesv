#include "Vtestbench.h"
#include "verilated.h"
#include "verilated_vcd_c.h"

int main(int argc, char** argv) {
    VerilatedContext* contextp = new VerilatedContext;
    contextp->commandArgs(argc, argv);

    Vtestbench* top = new Vtestbench{contextp};

    contextp->traceEverOn(true);
    VerilatedVcdC* tfp = new VerilatedVcdC;
    top->trace(tfp, 99);
    tfp->open("output.vcd");

    top->clock = 0;
    top->eval();
    tfp->dump(contextp->time());

    while (!contextp->gotFinish()) {
        contextp->timeInc(5);
        top->clock = !top->clock;
        top->eval();
        tfp->dump(contextp->time());
    }

    tfp->close();
    delete top;
    delete contextp;
    return 0;
}
