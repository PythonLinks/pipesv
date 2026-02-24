`default_nettype none
module EdgeDetector(
   input wire [23:0] pixels[5],
   output	     resultOut[5]		    
);
  pipeline 
  stage (first)
      pixels <= pixels;
  stage (second)
      pixels <= pixels;
  stage (third)
      pixels <= pixels;   
  stage (average)
      reg [23:0] beforeEdge;
      reg [23:0] afterEdge;
      beforeEdge <= average (pixels#{-3}, pixels#{-2});
      afterEdge  <= average (pixels#{-1}, pixels#{-0});
  stage(difference)
      reg [23:0] difference;
      difference <= difference(beforeEdge,afterEdge);
  stage (distance)
      reg [17:0] distanceSquared;      
      distance <= ditanceSquared(difference);
  stage (result)
      result = edgeOrNot(distance);
  endpipeline
  assign resultOutput = result;
endmodule   


  
