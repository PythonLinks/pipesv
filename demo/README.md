# EDGE DETECTOR DEMO

This is a simple demo of a pipelined edge detector. The array of RGB888 pixels
pass through a series of stages.   
- `stage(increment)`, all of the colors are incremented, and wrap.
- `stage(edge)`. a  color edge is added to the pixels
- `stage(noise)`, noise is added to the pixels`
- `stage(delay1)`, `stage(delay2)`and `stage(delay3)` allow access multiple pixels at a time.
- `stage(average2)` averages two pairs of pixels
- `stage(average4)` averages those two pairs
- `stage(difference) calculates the difference between averages
- `stage(distanceSquared)` calculates the square of the distance between pixels.
- `stage(edgeDetector)` spots the edges
- `stage(result)` displays a bit  map with edges.

## Running

./make compiles the Verilator code
./run just runs it without compiling. 