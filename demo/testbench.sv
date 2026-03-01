
module testbench (
    input logic clock
);
    Pixel pixel;
    logic result;
    integer cycle;

    PipeLine pipeline (
        .clock(clock),
        .pixel(pixel),
        .result(result)
    );

    initial begin

        cycle = 0;

        $display("Pixels");
        $display("cycle | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d",
                cycle,
                pipeline.pixels_init[0].red,
                pipeline.pixels_init[0].green,
                pipeline.pixels_init[0].blue
            );
        end

        $display("");
        $display("Pixels With Edge");
        $display("cycle | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d",
                cycle,
                pipeline.pixels_createEdge[0].red,
                pipeline.pixels_createEdge[0].green,
                pipeline.pixels_createEdge[0].blue
            );
        end

        $display("");
        $display("Pixels With Noise");
        $display("cycle | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d",
                cycle,
                pipeline.pixels_addNoise[0].red,
                pipeline.pixels_addNoise[0].green,
                pipeline.pixels_addNoise[0].blue
            );
        end

        $finish;
    end

endmodule
