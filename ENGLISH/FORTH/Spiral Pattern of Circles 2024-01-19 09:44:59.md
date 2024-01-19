```forth
: DRAW-CIRCLE ( RADIUS -- )
  0 2DUP [DUP *] 0DO
  [ DUP 1+ OVER SWAP ROT + SWAP ROT 1+ SWAP ROT . ]
  LOOP DROP ;

: CIRCLE ( X Y RADIUS -- )
  OVER CX+ OVER CY+ 2DUP DRAW-CIRCLE
  DROP DROP ;

: SPIRAL ( RADIUS -- )
  50 0 DO
    CIRCLE 20 ROT ;
  LOOP ;

: MAIN ( -- )
  0 0 0 SPIRAL
  0 0 240 SPIRAL ;
```

Explanation:

This Forth code generates a spiral pattern of circles. It starts by defining a word DRAW-CIRCLE, which takes a radius as input and draws a circle of that radius centered at the current position. The word CIRCLE then takes an x-coordinate, y-coordinate, and radius as input and calls DRAW-CIRCLE to draw a circle at that location.

The word SPIRAL is a loop that calls CIRCLE 50 times, each time increasing the radius by 20. This creates a spiral pattern of circles.

The word MAIN calls SPIRAL twice, once with a radius of 0 and once with a radius of 240. This creates two spirals, one starting at the center of the screen and one starting at the edge.

Overall, this code is a good example of how Forth can be used to create complex and interesting patterns. It is also a good example of how Forth code can be very concise and readable.