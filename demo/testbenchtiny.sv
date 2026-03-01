
module testbench (
    input logic clock
);

    integer cycle;

    PipeLine pipeline (
        .clock(clock)
    );

initial begin
  repeat (256*256) @(posedge clock);
  $finish;
end


endmodule
