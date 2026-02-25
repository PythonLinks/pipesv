`default_nettype none
module EdgeDetector (clock);
	input logic clock;
	PixelArray pixels;
	always @(posedge clock)
		pixels <= incrementPixels(pixels);
	// pipeline
	// stage (createEdge)
		logic [7:0] counter_createEdge = 0;
		PixelArray pixels_createEdge;
		always @(posedge clock) begin
			counter_createEdge <= counter_createEdge + 1;
			pixels_createEdge <= createEdge(pixels, counter_createEdge);
			end
	// stage (addNoise)
		PixelArray pixels_addNoise;
		always @(posedge clock)
			pixels_addNoise <= noise(pixels_createEdge);
	// stage (delay2_0)
		PixelArray pixels_delay2_0;
		always @(posedge clock)
			pixels_delay2_0 <= pixels_addNoise;
	// stage (delay2_1)
		PixelArray pixels_delay2_1;
		always @(posedge clock)
			pixels_delay2_1 <= pixels_delay2_0;
	// stage (averagePixels)
		PixelArray average2_averagePixels;
		PixelArray pixels_averagePixels;
		always @(posedge clock) begin
			pixels_averagePixels <= pixels_delay2_1;
			average2_averagePixels <= average(pixels_delay2_1, pixels_averagePixels);
			end
	// stage (delay)
		PixelArray average2_delay;
		always @(posedge clock)
			average2_delay <= average2_averagePixels;
	// stage (average4)
		PixelArray average4_average4;
		always @(posedge clock)
			average4_average4 <= average(average2_averagePixels, average2_delay);
	// stage (delay4_1)
		PixelArray average4_delay4_1;
		always @(posedge clock)
			average4_delay4_1 <= average4_average4;
	// stage (delay4_2)
		PixelArray average4_delay4_2;
		always @(posedge clock)
			average4_delay4_2 <= average4_delay4_1;
	// stage (delay4_3)
		PixelArray average4_delay4_3;
		always @(posedge clock)
			average4_delay4_3 <= average4_delay4_2;
	// stage (delta)
		PixelArray delta_delta;
		always @(posedge clock)
			delta_delta <= difference(average4_delay4_3, average4_average4);
	// stage (distanceSquared)
		reg [17:0] distanceSquared_distanceSquared [0:PixelHeight - 1];
		always @(posedge clock)
			distanceSquared_distanceSquared <= squaredDistance(delta_delta);
	// stage (detector)
		logic [0:0] result_detector [0:PixelHeight - 1];
		always @(posedge clock)
			begin
				int ii;
				for (ii = 0; ii < 5; ii += 1)
					begin
						if (distanceSquared_distanceSquared[ii] > 100)
							result_detector[ii] <= 1'b1;
						else
							result_detector[ii] <= 1'b0;
						end
				end
	// endpipeline
	wire resultOut [0:PixelHeight - 1];
	assign resultOut = result_detector;
endmodule
