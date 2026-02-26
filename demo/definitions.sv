`ifndef DEFINITIONS_SV
`define DEFINITIONS_SV

parameter PixelHeight = 5;

  
typedef struct packed {
    logic [7:0] red;
    logic [7:0] green;
    logic [7:0] blue;
} Pixel;

typedef Pixel  PixelArray [PixelHeight];

typedef logic [17:0] DistanceArray [PixelHeight];

function automatic PixelArray incrementColor(PixelArray pixels, logic [7:0] counter);
    for (int index = 0; index < 5; index++) begin
        pixels[index].red   = pixels[index].red   + 1'b1;
        if (counter == 0)
             pixels[index].green = pixels[index].green - 1'b1;
        pixels[index].blue  = pixels[index].blue  - 1'b1;
    end
    return pixels;
endfunction

    
function automatic PixelArray createEdge(PixelArray pixels, input logic [7:0] counter);
    logic [7:0] delta;  // Changed from parameter to logic
    logic [7:0] red;
    logic [7:0] green;
    logic [7:0] blue;   
    
    for (int index = 0; index < 5; index++) begin
        red   = pixels[index].red;
        green = pixels[index].green;	   
        blue  = pixels[index].blue;
        
        // Set delta based on counter (moved outside pixel assignment)
        delta = (counter > 120) ? 8'd0 : 8'd40;
        
        // Apply changes
        //if (counter == 8'd128)
        //    pixels[index].red   =  0;
        //else 
        if (counter < 8'd128) 
             pixels[index].red   =  red + delta;
        else
             pixels[index].red   =  red;

        //if (counter == 8'd128) 
        //    pixels[index].blue   =  0;
        //else 
        if (counter < 8'd128) 
             pixels[index].blue   =  red + delta;
        else
             pixels[index].blue   =  red;

        //if (counter == 8'd128) 
        //    pixels[index].green   =  0;
        //else
            pixels[index].green = green;
    end
    
    return pixels;
endfunction // createEdge

function automatic PixelArray addNoise(PixelArray pixels);
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

function automatic PixelArray initializePixels(pixels);
    PixelArray pixels;
    for (int index = 0; index < 5; index++)
        pixels[index] = 0;
    return pixels;
endfunction

function automatic PixelArray average(PixelArray a, PixelArray b);
    PixelArray result;
    for (int index = 0; index < PixelHeight; index++) begin
        logic [8:0] red, green, blue;
        red   = 9'(a[index].red)   + 9'(b[index].red);
        green = 9'(a[index].green) + 9'(b[index].green);
        blue  = 9'(a[index].blue)  + 9'(b[index].blue);
        result[index].red   = 8'(red   >> 1);
        result[index].green = 8'(green >> 1);
        result[index].blue  = 8'(blue  >> 1);
    end
    return result;
endfunction

function automatic PixelArray difference(PixelArray a, PixelArray b);
    PixelArray result;
    for (int index = 0; index < PixelHeight; index++) begin
        result[index].red   = (a[index].red   >= b[index].red)
                              ? a[index].red   - b[index].red
                              : b[index].red   - a[index].red;
        result[index].green = (a[index].green >= b[index].green)
                              ? a[index].green - b[index].green
                              : b[index].green - a[index].green;
        result[index].blue  = (a[index].blue  >= b[index].blue)
                              ? a[index].blue  - b[index].blue
                              : b[index].blue  - a[index].blue;
    end
    return result;
endfunction

function automatic DistanceArray squaredDistance(PixelArray diff);
    DistanceArray result;
    for (int index = 0; index < PixelHeight; index++) begin
        result[index] = (18'(diff[index].red)   * 18'(diff[index].red))
                      + (18'(diff[index].green) * 18'(diff[index].green))
                      + (18'(diff[index].blue)  * 18'(diff[index].blue));
    end
    return result;
endfunction

`endif
