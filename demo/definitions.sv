`ifndef DEFINITIONS_SV
`define DEFINITIONS_SV

typedef struct packed {
    logic [7:0] red;
    logic [7:0] green;
    logic [7:0] blue;
} Pixel;

typedef Pixel PixelArray [5];

function automatic PixelArray incrementPixels(PixelArray pixels);
    for (int index = 0; index < 5; index++) begin
        pixels[index].red   = pixels[index].red   + 1;
        pixels[index].green = pixels[index].green + 1;
        pixels[index].blue  = pixels[index].blue  + 1;
    end
    return pixels;
endfunction

function automatic PixelArray createEdge(PixelArray pixels, logic [7:0] counter);
    for (int index = 0; index < 5; index++) begin
        if (counter > 20) begin
            pixels[index].red   = pixels[index].red   + 10;
            pixels[index].green = pixels[index].green + 10;
            pixels[index].blue  = pixels[index].blue  + 10;
        end
    end
    return pixels;
endfunction

function automatic PixelArray noise(PixelArray pixels);
    for (int index = 0; index < 5; index++) begin
        logic signed [9:0] noisyRed, noisyGreen, noisyBlue;

        noisyRed   = 10'(pixels[index].red)   + 10'(5'($urandom_range(0, 31))) - 16;
        noisyGreen = 10'(pixels[index].green) + 10'(5'($urandom_range(0, 31))) - 16;
        noisyBlue  = 10'(pixels[index].blue)  + 10'(5'($urandom_range(0, 31))) - 16;

        pixels[index].red   = (noisyRed   < 0) ? 8'd0 : (noisyRed   > 255) ? 8'd255 : 8'(noisyRed);
        pixels[index].green = (noisyGreen < 0) ? 8'd0 : (noisyGreen > 255) ? 8'd255 : 8'(noisyGreen);
        pixels[index].blue  = (noisyBlue  < 0) ? 8'd0 : (noisyBlue  > 255) ? 8'd255 : 8'(noisyBlue);
    end
    return pixels;
endfunction

function automatic PixelArray initializePixels();
    PixelArray pixels;
    for (int index = 0; index < 5; index++)
        pixels[index] = 0;
    return pixels;
endfunction

`endif
