`default_nettype none
//`include "functions.sv"
//`include "define.sv"    

module PipeLine (
    input logic clock
);
    logic [7:0] counter = 0;
    var PixelArray pixels;
    initial pixels = initializePixels(pixels);

    always @(posedge clock) begin
        // Incrementing is best done
        // outside of a pipeline.
        counter <= counter + 1;
        pixels <= incrementColor(pixels,counter);
        end
    integer imageFile;
    initial imageFile = $fopen("image.raw", "wb");
            always @(posedge clock)
                $fwrite(imageFile,
                "%c%c%c",
                pixels_createEdge[0].red,
                pixels_createEdge[0].green,
                pixels_createEdge[0].blue);

    pipeline
        stage #{createEdge}
            pixels <= createEdge(pixels, counter);
        stage #{addNoise}
            pixels <= addNoise(pixels);
        stage #{delay2_0}
            pixels <= pixels;
        stage #{delay2_1}
            pixels <= pixels;    
        stage #{averagePixels}
 	    pixels <= pixels;
            var PixelArray average2 ;
            average2  <= average(pixels#{-1}, pixels#{-0});
        stage #{delay}
            average2 <= average2;
        stage #{average4}
            var PixelArray average4;
            average4 <= average(average2#{-2},average2#{-1});
        stage #{delay4_1}
            average4 <= average4;
        stage #{delay4_2}
            average4 <= average4;
        stage #{delay4_3}
            average4 <= average4;     
        stage #{delta}    
            var PixelArray delta;
            delta <= difference (average4#{-1}, average4#{-4});
        stage #{distanceSquared}
            reg [17:0] distanceSquared[PipelineHeight];
            distanceSquared <= squaredDistance(delta);
        stage #{detector}
            reg result[PipelineHeight];
            always @(posedge clock) begin
                for (int ii = 0; ii < 5; ii++) begin
                    if (distanceSquared[ii] > 100)
                        result[ii] <= 1'b1;
                    else
                        result[ii] <= 1'b0;
                end
            end
    endpipeline
    wire result2[PipelineHeight];
    // Be careful to use the right name for pipeline variables 
    assign result2 = result_detector;      
endmodule

