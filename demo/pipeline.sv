`default_nettype none
module PipeLine (clock);
	input logic clock;
	always @(posedge clock) begin
		end
	integer imageFile;
	initial begin
		imageFile = $fopen("image.raw", "wb");
		end
	always @(posedge clock)
		$fwrite(imageFile, "%c%c%c", pixels_createEdge[0].red, pixels_createEdge[0].green, pixels_createEdge[0].blue);
	// pipeline @(posedge clock)
	// stage #{init}
		logic [7:0] counter_init = 0;
		PixelArray pixels_init;
		initial begin
			pixels_init = initializePixels(pixels_init);
			end
		always @(posedge clock) begin
			counter_init <= counter_init + 1;
			pixels_init <= incrementColor(pixels_init, counter_init);
			end
	// stage #{createEdge}
		PixelArray pixels_createEdge;
		always @(posedge clock)
			pixels_createEdge <= createEdge(pixels_init, counter_init);
	// stage #{addNoise}
		PixelArray pixels_addNoise;
		always @(posedge clock)
			pixels_addNoise <= addNoise(pixels_createEdge);
	// stage #{delay2_0}
		PixelArray pixels_delay2_0;
		always @(posedge clock)
			pixels_delay2_0 <= pixels_addNoise;
	// stage #{delay2_1}
		PixelArray pixels_delay2_1;
		always @(posedge clock)
			pixels_delay2_1 <= pixels_delay2_0;
	// stage #{averagePixels}
		PixelArray average2_averagePixels;
		PixelArray pixels_averagePixels;
		always @(posedge clock) begin
			pixels_averagePixels <= pixels_delay2_1;
			average2_averagePixels <= average(pixels_delay2_1, pixels_averagePixels);
			end
	// stage #{delay}
		PixelArray average2_delay;
		always @(posedge clock)
			average2_delay <= average2_averagePixels;
	// stage #{average4}
		PixelArray average4_average4;
		always @(posedge clock)
			average4_average4 <= average(average2_averagePixels, average2_delay);
	// stage #{delay4_1}
		PixelArray average4_delay4_1;
		always @(posedge clock)
			average4_delay4_1 <= average4_average4;
	// stage #{delay4_2}
		PixelArray average4_delay4_2;
		always @(posedge clock)
			average4_delay4_2 <= average4_delay4_1;
	// stage #{delay4_3}
		PixelArray average4_delay4_3;
		always @(posedge clock)
			average4_delay4_3 <= average4_delay4_2;
	// stage #{delta}
		PixelArray delta_delta;
		always @(posedge clock)
			delta_delta <= difference(average4_delay4_3, average4_average4);
	// stage #{distanceSquared}
		reg [17:0] distanceSquared_distanceSquared [0:PipelineHeight - 1];
		always @(posedge clock)
			distanceSquared_distanceSquared <= squaredDistance(delta_delta);
	// stage #{detector}
		int ii_detector;
		reg result_detector [0:PipelineHeight - 1];
		always @(posedge clock)
			for (ii_detector = 0; ii_detector < 5; ii_detector += 1)
			begin
				if (distanceSquared_distanceSquared[ii_detector] > 100)
					result_detector[ii_detector] <= 1'b1;
				else
					result_detector[ii_detector] <= 1'b0;
				end
	// endpipeline
	wire result2 [0:PipelineHeight - 1];
	assign result2 = result_detector;
endmodule
