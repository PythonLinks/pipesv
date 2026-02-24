`include "definitions.sv"

module detector (
    input logic clock
);

    Pixel pixels [5];

    initial pixels = initializePixels();

    always @(posedge clock) begin
        pixels <= incrementPixels(pixels);
    end

    logic [7:0] counter = 0;
    Pixel pixelsWithEdge [5];

    initial pixelsWithEdge = initializePixels();

    always @(posedge clock) begin
        counter        <= counter + 1;
        pixelsWithEdge <= createEdge(pixels, counter);
    end

    PixelArray pixelsWithNoise;

    initial pixelsWithNoise = initializePixels();

    always @(posedge clock) begin
        pixelsWithNoise <= noise(pixels);
    end

endmodule
