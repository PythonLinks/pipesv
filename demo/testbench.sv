`include "define.sv"

module testbench (
    input logic clock
);

    integer cycle;
    Pixel pixel;
   wire  result;
 
    PipeLine pipeline (
        .clock(clock),
        .pixel(pixel),		        
        .result(result)		       
    );

initial begin
  repeat (256*256) @(posedge clock);
  $finish;
end


endmodule
