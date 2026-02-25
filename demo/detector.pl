`default_nettype none

module EdgeDetector (
    input logic clock//,    output Pixel resultOut
);
    var PixelArray pixels;
    //initial pixels = initializePixels(pixels);

    //Not dependent on the previous stage
    always @(posedge clock) 
    pixels <= incrementPixels(pixels);

    pipeline
        stage (createEdge)
            logic [7:0] counter = 0;
            counter <= counter + 1;
            pixels <= createEdge(pixels, counter);
        stage (addNoise)
            pixels <= noise(pixels);
        stage (delay2_0)
            pixels <= pixels;
        stage (delay2_1)
            pixels <= pixels;    
        stage(averagePixels)
 	    pixels <= pixels;
            var PixelArray average2 ;
            average2  <= average(pixels#{-1}, pixels#{-0});
        stage (delay)
            average2 <= average2;
        stage(average4)
            var PixelArray average4;
            average4 <= average(average2#{-2},average2#{-1});
        stage (delay4_1)
            average4 <= average4;
        stage (delay4_2)
            average4 <= average4;
        stage (delay4_3)
            average4 <= average4;     
        stage(delta)    
            var PixelArray delta;
            delta <= difference (average4#{-1}, average4#{-4});
        stage(distanceSquared) 
            reg [17:0] distanceSquared[PixelHeight];
            distanceSquared <= squaredDistance(delta);
        stage(detector)
            logic [0:0] result [PixelHeight];
            always @(posedge clock) begin
                for (int ii = 0; ii < 5; ii++) begin
                    if (distanceSquared[ii] > 8'd100)
                        result[ii] <= 1'b1;
                    else
                        result[ii] <= 1'b0;
                end
            end
    endpipeline
    wire resultOut[PixelHeight];
    assign resultOut = result;      
endmodule

