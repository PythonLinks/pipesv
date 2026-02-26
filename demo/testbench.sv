
module testbench (
    input logic clock
);
    integer cycle;
    EdgeDetector detectorInstance (
        .clock(clock)
    );

initial begin
  repeat (256*256) @(posedge clock);
  $finish;
end


endmodule
