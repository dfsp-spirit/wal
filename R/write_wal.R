
## 24 bit JPEG/PNG colors to indexed WAL colors:
# convert RGB colors to LAB space (colorscience::rgb2x)
# use deltaE metric to compute distances (colorscience::deltaE2000())
# pick color with

## mimap issue:
# find out storage order
# find out width and height in pixels of the 3 smaller mipmap levels (depends on 1st, I guess)
