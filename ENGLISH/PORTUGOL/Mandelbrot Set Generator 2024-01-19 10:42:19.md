```portugol
PROGRAM MANDELBROT;
VAR
    X, Y          : REAL;
    COLOUR        : INTEGER;
    XRANGE, YRANGE: REAL;
    ITER          : INTEGER;
    COMPLEX       : REAL;
    UP, DOWN, LEFT, RIGHT: REAL;
    WIDTH, HEIGHT : INTEGER;
    XSCALE, YSCALE: REAL;

PROCEDURE PLOT (X, Y, COLOUR);
VAR
    I: INTEGER;
BEGIN
    FOR I:=0 TO WIDTH-1 DO
        BEGIN
            PUTPIXEL (I, HEIGHT-Y, COLOUR);
        END;
END PLOT;

BEGIN
    WIDTH:=640;
    HEIGHT:=480;
    XSCALE:=3.0;
    YSCALE:=2.0;
    XRANGE:=-2.0;
    YRANGE:=-1.0;
    LEFT:=XRANGE;
    RIGHT:=XRANGE+XSCALE;
    UP:=YRANGE+YSCALE;
    DOWN:=YRANGE;
    ITER:=20;
    COMPLEX:=ITER*ITER/4.0;

    FOR Y:=0 TO HEIGHT-1 DO
        BEGIN
            X:=LEFT;
            FOR X:=0 TO WIDTH-1 DO
                BEGIN
                    COLOUR:=0;
                    REALPART:=X*XSCALE/WIDTH;
                    IMAGPART:=Y*YSCALE/HEIGHT;
                    ITERATION:=0;
                    WHILE (ITERATION<ITER) AND (REALPART*REALPART+IMAGPART*IMAGPART)<COMPLEX DO
                        BEGIN
                            TEMP:=REALPART*REALPART-IMAGPART*IMAGPART+X;
                            IMAGPART:=2.0*REALPART*IMAGPART+Y;
                            REALPART:=TEMP;
                            INC(ITERATION);
                        END;
                    PLOT (X, Y, ITERATION*6);
                END;
        END;
    PUTIMAGE (0, 0, WIDTH, HEIGHT, 'MANDELBROT.BMP');
END MANDELBROT.
```

Explanation:

1. `PROGRAM MANDELBROT`: This line declares the start of the program and gives it the name "MANDELBROT."

2. `VAR`: This line declares the variables used in the program.

 * `X, Y`: Real variables used to store the coordinates of a point in the complex plane.
 * `COLOUR`: Integer variable used to store the color of a pixel.
 * `XRANGE, YRANGE`: Real variables used to specify the range of values for the real and imaginary parts of the complex plane.
 * `ITER`: Integer variable used to specify the maximum number of iterations for the Mandelbrot calculation.
 * `COMPLEX`: Real variable used to store a constant value.
 * `UP, DOWN, LEFT, RIGHT`: Real variables used to specify the boundaries of the region of the complex plane being examined.
 * `WIDTH, HEIGHT`: Integer variables used to specify the dimensions of the image being generated.
 * `XSCALE, YSCALE`: Real variables used to specify the scaling factors for the complex plane.

3. `PROCEDURE PLOT (X, Y, COLOUR)`: This procedure is used to plot a single pixel on the screen.

 * It takes three parameters: the x and y coordinates of the pixel and the color to be used.
 * It uses a FOR loop to iterate through all the pixels in the image and set their colors accordingly.

4. `BEGIN`: This line marks the start of the main program block.

5. `WIDTH:=640;`: This line sets the width of the image to 640 pixels.

6. `HEIGHT:=480;`: This line sets the height of the image to 480 pixels.

7. `XSCALE:=3.0;`: This line sets the scaling factor for the complex plane in the x-direction to 3.0.

8. `YSCALE:=2.0;`: This line sets the scaling factor for the complex plane in the y-direction to 2.0.

9. `XRANGE:=-2.0;`: This line sets the left boundary of the region of the complex plane being examined to -2.0.

10. `YRANGE:=-1.0;`: This line sets the bottom boundary of the region of the complex plane being examined to -1.0.

11. `LEFT:=XRANGE;`: This line sets the left boundary of the region of the complex plane being examined to the value of `XRANGE`.

12. `RIGHT:=XRANGE+XSCALE;`: This line sets the right boundary of the region of the complex plane being examined to the value of `XRANGE` plus `XSCALE`.

13. `UP:=YRANGE+YSCALE;`: This line sets the top boundary of the region of the complex plane being examined to the value of `YRANGE` plus `YSCALE`.

14. `DOWN:=YRANGE;`: This line sets the bottom boundary of the region of the complex plane being examined to the value of `YRANGE`.

15. `ITER:=20;`: This line sets the maximum number of iterations for the Mandelbrot calculation to 20.

16. `COMPLEX:=ITER*ITER/4.0;`: This line sets the value of the `COMPLEX` variable to the square of `ITER` divided by 4.0.

17. `FOR Y:=0 TO HEIGHT-1 DO`: This line starts a FOR loop that iterates through all the rows of pixels in the image.

18. `BEGIN`: This line marks the start of the loop body.

19. `X:=LEFT;`: This line sets the x-coordinate of the current pixel to the left boundary of the region of the complex plane being examined.

20. `FOR X:=0 TO WIDTH-1 DO`: This line starts a FOR loop that iterates through all the columns of pixels in the current row.

21. `BEGIN`: This line marks the start of the loop body.

22. `COLOUR:=0;`: This line sets the color of the current pixel to black.

23. `REALPART:=X*XSCALE/WIDTH;`: This line calculates the real part of the complex number corresponding to the current pixel.

24. `IMAGPART:=Y*YSCALE/HEIGHT;`: This line calculates the imaginary part of the complex number corresponding to the current pixel.

25. `ITERATION:=0;`: This line sets the iteration counter to 0.

26. `WHILE (ITERATION<ITER) AND (REALPART*REALPART+IMAGPART*IMAGPART)<COMPLEX DO`: This line starts a WHILE loop that continues until either the maximum number of iterations has been reached or the complex number corresponding to the current pixel escapes from the Mandelbrot set.

27. `BEGIN`: This line marks the start of the loop body.

28. `TEMP:=REALPART*REALPART-IMAGPART*IMAGPART+X;`: This line calculates the temporary variable `TEMP`, which is used in the Mandelbrot calculation.

29. `IMAGPART:=2.0*REALPART*IMAGPART+Y;`: This line calculates the new value of the imaginary part of the complex number.

30. `REALPART:=TEMP;`: This line calculates the new value of the real part of the complex number.

31. `INC(ITERATION);`: This line increments the iteration counter.

32. `END;`: This line marks the end of the WHILE loop.

33. `PLOT (X, Y, ITERATION*6);`: This line calls the `PLOT` procedure to plot the current pixel using the appropriate color.

34. `END;`: This line marks the end of the loop body for the FOR loop that iterates through the columns of pixels in the current row.

35. `END;`: This line marks the end of the loop body for the FOR loop that iterates through the rows of pixels in the image.

36. `PUTIMAGE (0, 0, WIDTH, HEIGHT, 'MANDELBROT.BMP');`: This line saves the image to a file named "MANDELBROT.BMP."

37. `END MANDELBROT.`: This line marks the end of the program.