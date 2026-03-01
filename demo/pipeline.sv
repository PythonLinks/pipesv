// Trace: pipeline.pl:1:1
`default_nettype none
module PipeLine (
	clock,
	pixel,
	result
);
	// Trace: pipeline.pl:6:5
	input logic clock;
	// Trace: pipeline.pl:7:5
	input Pixel pixel;
	// Trace: pipeline.pl:8:5
	output wire result;
	// Trace: pipeline.pl:11:5
	integer imageFile;
	// Trace: pipeline.pl:12:5
	initial begin
		imageFile = $fopen("image.raw", "wb");
		end
	// Trace: pipeline.pl:13:13
	always @(posedge clock)
		$fwrite(imageFile, "%c%c%c", pixels_createEdge[0].red, pixels_createEdge[0].green, pixels_createEdge[0].blue);
	// Trace: pipeline.pl:20:5
	// pipeline @(posedge clock)
	// stage #{init}
		// Trace: pipeline.pl:22:13
		logic [7:0] counter_init = 0;
		// Trace: pipeline.pl:23:13
		PixelArray pixels_init;
		// Trace: pipeline.pl:24:13
		// Trace: pipeline.pl:25:13
		// Trace: pipeline.pl:26:13
		initial begin
			pixels_init = initializePixels(pixels_init);
			end
		always @(posedge clock) begin
			counter_init <= counter_init + 1;
			pixels_init <= incrementColor(pixels_init, counter_init);
			end
	// stage #{createEdge}
		// Trace: pipeline.pl:28:13
		PixelArray pixels_createEdge;
		always @(posedge clock)
			pixels_createEdge <= createEdge(pixels_init, counter_init);
	// stage #{addNoise}
		// Trace: pipeline.pl:30:13
		PixelArray pixels_addNoise;
		always @(posedge clock)
			pixels_addNoise <= addNoise(pixels_createEdge);
	// stage #{delay2_0}
		// Trace: pipeline.pl:32:13
		PixelArray pixels_delay2_0;
		always @(posedge clock)
			pixels_delay2_0 <= pixels_addNoise;
	// stage #{delay2_1}
		// Trace: pipeline.pl:34:13
		PixelArray pixels_delay2_1;
		always @(posedge clock)
			pixels_delay2_1 <= pixels_delay2_0;
	// stage #{averagePixels}
		// Trace: pipeline.pl:36:7
		// Trace: pipeline.pl:37:13
		PixelArray average2_averagePixels;
		// Trace: pipeline.pl:38:13
		PixelArray pixels_averagePixels;
		always @(posedge clock) begin
			pixels_averagePixels <= pixels_delay2_1;
			average2_averagePixels <= average(pixels_delay2_1, pixels_averagePixels);
			end
	// stage #{delay}
		// Trace: pipeline.pl:40:13
		PixelArray average2_delay;
		always @(posedge clock)
			average2_delay <= average2_averagePixels;
	// stage #{average4}
		// Trace: pipeline.pl:42:13
		PixelArray average4_average4;
		// Trace: pipeline.pl:43:13
		always @(posedge clock)
			average4_average4 <= average(average2_averagePixels, average2_delay);
	// stage #{delay4_1}
		// Trace: pipeline.pl:45:13
		PixelArray average4_delay4_1;
		always @(posedge clock)
			average4_delay4_1 <= average4_average4;
	// stage #{delay4_2}
		// Trace: pipeline.pl:47:13
		PixelArray average4_delay4_2;
		always @(posedge clock)
			average4_delay4_2 <= average4_delay4_1;
	// stage #{delay4_3}
		// Trace: pipeline.pl:49:13
		PixelArray average4_delay4_3;
		always @(posedge clock)
			average4_delay4_3 <= average4_delay4_2;
	// stage #{delta}
		// Trace: pipeline.pl:51:13
		PixelArray delta_delta;
		// Trace: pipeline.pl:52:13
		always @(posedge clock)
			delta_delta <= difference(average4_delay4_3, average4_average4);
	// stage #{distanceSquared}
		// Trace: pipeline.pl:54:13
		reg [17:0] distanceSquared_distanceSquared [0:PipelineHeight - 1];
		// Trace: pipeline.pl:55:13
		always @(posedge clock)
			distanceSquared_distanceSquared <= squaredDistance(delta_delta);
	// stage #{detector}
		// Trace: pipeline.pl:57:13
		int ii_detector;
		// Trace: pipeline.pl:58:13
		logic conclusion_detector [4:0];
		// Trace: pipeline.pl:59:17
		always @(posedge clock)
			for (ii_detector = 0; ii_detector < 5; ii_detector += 1)
			begin
				if (distanceSquared_distanceSquared[ii_detector] > 100)
					conclusion_detector[ii_detector] <= 1'b1;
				else
					conclusion_detector[ii_detector] <= 1'b0;
				end
	// endpipeline
	// Trace: pipeline.pl:67:5
	assign result = conclusion_detector[0];
endmodule
