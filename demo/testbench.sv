`include "definitions.sv"

module testbench (
    input logic clock
);

    integer cycle;

    detector detectorInstance (
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
                detectorInstance.pixelsWithEdge[0].red, detectorInstance.pixelsWithEdge[0].green, detectorInstance.pixelsWithEdge[0].blue,
                detectorInstance.pixelsWithEdge[1].red, detectorInstance.pixelsWithEdge[1].green, detectorInstance.pixelsWithEdge[1].blue,
                detectorInstance.pixelsWithEdge[2].red, detectorInstance.pixelsWithEdge[2].green, detectorInstance.pixelsWithEdge[2].blue,
                detectorInstance.pixelsWithEdge[3].red, detectorInstance.pixelsWithEdge[3].green, detectorInstance.pixelsWithEdge[3].blue,
                detectorInstance.pixelsWithEdge[4].red, detectorInstance.pixelsWithEdge[4].green, detectorInstance.pixelsWithEdge[4].blue
            );
        end

        $display("");
        $display("Pixels With Noise");
        $display("cycle | red green blue | red green blue | red green blue | red green blue | red green blue");
        repeat (30) begin
            @(posedge clock); cycle = cycle + 1;
            $display("%5d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d | %3d %5d %4d",
                cycle,
                detectorInstance.pixelsWithNoise[0].red, detectorInstance.pixelsWithNoise[0].green, detectorInstance.pixelsWithNoise[0].blue,
                detectorInstance.pixelsWithNoise[1].red, detectorInstance.pixelsWithNoise[1].green, detectorInstance.pixelsWithNoise[1].blue,
                detectorInstance.pixelsWithNoise[2].red, detectorInstance.pixelsWithNoise[2].green, detectorInstance.pixelsWithNoise[2].blue,
                detectorInstance.pixelsWithNoise[3].red, detectorInstance.pixelsWithNoise[3].green, detectorInstance.pixelsWithNoise[3].blue,
                detectorInstance.pixelsWithNoise[4].red, detectorInstance.pixelsWithNoise[4].green, detectorInstance.pixelsWithNoise[4].blue
            );
        end

        $finish;
    end

endmodule
