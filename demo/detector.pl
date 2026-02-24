`default_nettype none

module EdgeDetector (
    input logic clock//,    output Pixel resultOut
);
    var PixelArray pixels;
    initial pixels <= initializePixels();

    //Not dependent on the previous stage
    always @(posedge clock) 
        pixels <= incrementPixels(pixels);

    pipeline
        stage (createEdge)
            logic [7:0] counter = 0;
            counter <= counter + 1;
            pixels <= createEdge(pixels, counter);
        stage (addNoise)
            pixels <= addNoise(pixels);
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
        stage(averageFour)
            var PixelArray averageFour;
            average4 <= average(averageTwo#{-3},averageTwo#{-1}); 	     
        stage (delay4_1)
            average2 <= average2;
        stage (delay4_2)
            average2 <= average2;
        stage (delay4_3)
            average2 <= average2;     
        stage(delta)    
            reg  delta[PixelHeight];
            delta <= difference (average4#{-1}, average4#{-4});
        stage(distanceSquared) 
            reg [17:0] distanceSquared[PixelHeight];
            distanceSquared <= distance (delta);
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
    assign edgeOut = result;      
endmodule

