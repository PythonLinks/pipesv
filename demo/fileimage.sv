// Module: fileImage
// Description: A memory module that outputs a pixel every clock cycle
// 
// Parameters:
//   pixelWidth     - Width of each pixel in bits
//   numberOfPixels - Depth of the memory array
//
// Features:
//   - Internal memory array of size [numberOfPixels-1:0][pixelWidth-1:0]
//   - Internal counter of size $clog2(numberOfPixels) bits
//   - Counter starts at 0 and wraps around when reaching numberOfPixels-1
//   - On each clock cycle, outputs memory[addressCounter]
//   - Memory initialized from file specified by `IMAGE_FILE_NAME define
//
// Required defines (from "defines.sv"):
//   `IMAGE_FILE_NAME - Path to file containing pixel data in hex format

`include "defines.sv"

module FileImage #(
    parameter int pixelWidth = 8,
    parameter int numberOfPixels = 256
) (
    input  logic                     clock,
    output logic [pixelWidth-1:0]    pixelOut
);

    // Calculate address width (clog2 of numberOfPixels)
    localparam int addrWidth = $clog2(NumberOfPixels);
    
    // Internal memory array
    logic [pixelWidth-1:0] memory [0:NumberOfPixels-1];
    
    // Address counter
    logic [addrWidth-1:0] addressCounter;
    
    // Initialize memory with values from image file
    initial begin
        $readmemh(`IMAGE_FILE_NAME, memory);
    end
    
    // Address counter logic
    always_ff @(posedge clock) begin
        if (addressCounter == numberOfPixels - 1)
            addressCounter <= '0;
        else
            addressCounter <= addressCounter + 1;
    end
    
    // Output pixel data
    always_comb begin
        pixelOut = memory[addressCounter];
    end
    
endmodule
