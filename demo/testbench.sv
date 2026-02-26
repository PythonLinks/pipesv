
module testbench (
    input logic clock
);

    integer cycle;

    EdgeDetector detectorInstance (
        .clock(clock)
    );

    initial begin

        cycle = 0;

        $display("Pixels");
        $display("cycle | red green blue | red green blue | red green blue | red green blue | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d",
                cycle,
                detectorInstance.pixels[0].red, detectorInstance.pixels[0].green, detectorInstance.pixels[0].blue,
                detectorInstance.pixels[1].red, detectorInstance.pixels[1].green, detectorInstance.pixels[1].blue,
                detectorInstance.pixels[2].red, detectorInstance.pixels[2].green, detectorInstance.pixels[2].blue,
                detectorInstance.pixels[3].red, detectorInstance.pixels[3].green, detectorInstance.pixels[3].blue,
                detectorInstance.pixels[4].red, detectorInstance.pixels[4].green, detectorInstance.pixels[4].blue
            );
        end

        $display("");
        $display("Pixels With Edge");
        $display("cycle | red green blue | red green blue | red green blue | red green blue | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d",
                cycle,
                detectorInstance.pixels_createEdge[0].red, detectorInstance.pixels_createEdge[0].green, detectorInstance.pixels_createEdge[0].blue,
                detectorInstance.pixels_createEdge[1].red, detectorInstance.pixels_createEdge[1].green, detectorInstance.pixels_createEdge[1].blue,
                detectorInstance.pixels_createEdge[2].red, detectorInstance.pixels_createEdge[2].green, detectorInstance.pixels_createEdge[2].blue,
                detectorInstance.pixels_createEdge[3].red, detectorInstance.pixels_createEdge[3].green, detectorInstance.pixels_createEdge[3].blue,
                detectorInstance.pixels_createEdge[4].red, detectorInstance.pixels_createEdge[4].green, detectorInstance.pixels_createEdge[4].blue
            );
        end

        $display("");
        $display("Pixels With Noise");
        $display("cycle | red green blue | red green blue | red green blue | red green blue | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d",
                cycle,
                detectorInstance.pixels_addNoise[0].red, detectorInstance.pixels_addNoise[0].green, detectorInstance.pixels_addNoise[0].blue,
                detectorInstance.pixels_addNoise[1].red, detectorInstance.pixels_addNoise[1].green, detectorInstance.pixels_addNoise[1].blue,
                detectorInstance.pixels_addNoise[2].red, detectorInstance.pixels_addNoise[2].green, detectorInstance.pixels_addNoise[2].blue,
                detectorInstance.pixels_addNoise[3].red, detectorInstance.pixels_addNoise[3].green, detectorInstance.pixels_addNoise[3].blue,
                detectorInstance.pixels_addNoise[4].red, detectorInstance.pixels_addNoise[4].green, detectorInstance.pixels_addNoise[4].blue
            );
        end

        $finish;
    end

endmodule
