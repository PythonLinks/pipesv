`default_nettype none
module EdgeDetector(
   input wire [23:0] pixels[5],
   output result		    
);
  reg previous;
   
  stage (first)
      pixels <= pixels;
  endstage

  stage (second)
      always @ (negedge clock)
          pixels <= pixels;
  endstage

  stage (third)
      reg x;
      pixels <= pixels;
      wire y;
      assign y = x;
  endstage    

  stage (encode)    
      wire x[7];
      reg [15:0] y;
      x <= y;
      pixels <= pixels#{-1};
      pixels <= average (pixels#{-1},      pixels#{last});                             
      pixels <= average (pixels#{second}[5],   pixels#{-3}[4]);                          
      pixels <= average (pixels#{+1}[5:2], pixels#{+1}[5:2]);                
  endstage
  stage (last)
      pixels <= pixels;
  endstage
    
     assign result = pixels;
endmodule   


  
