`ifndef DEFINE_SV
`define  DEFINE_SV
parameter PipelineHeight = 1;

//For girl.png and girl.raw
parameter ImageWidth = 706;
parameter ImageHeight = 467;
parameter NumberOfPixels  = ImageHeight * ImageWidth;

typedef  logic [17:0] DistanceSquared [PipelineHeight];  
typedef struct packed {
    logic [7:0] red;
    logic [7:0] green;
    logic [7:0] blue;
} Pixel;

typedef Pixel  PixelArray [PipelineHeight];

`endif
