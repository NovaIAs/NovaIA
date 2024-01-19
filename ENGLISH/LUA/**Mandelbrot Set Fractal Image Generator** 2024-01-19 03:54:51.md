```lua
-- This Lua code generates a fractal image using the Mandelbrot set. It sets up a canvas and iterates over each pixel, calculating the number of iterations it takes for the pixel to escape the Mandelbrot set. The color of the pixel is then set based on the number of iterations.

-- Set up the canvas
canvas = image.new(512, 512)

-- Set up the Mandelbrot set parameters
xmin, xmax = -2.0, 1.0
ymin, ymax = -1.5, 1.5

-- Iterate over each pixel in the canvas
for x = 1, 512 do
  for y = 1, 512 do
    -- Convert pixel coordinates to complex number
    c = complex.new(xmin + (xmax - xmin) * x / 512, ymin + (ymax - ymin) * y / 512)

    -- Calculate the number of iterations it takes for c to escape the Mandelbrot set
    n = 0
    while n < 256 and math.abs(c) < 2 do
      c = c * c + complex.new(-0.5, 0)
      n = n + 1
    end

    -- Set the color of the pixel based on the number of iterations
    if n == 256 then
      canvas:pixel(x, y, {0, 0, 0}) -- black
    else
      canvas:pixel(x, y, {math.min(n * 2, 255), math.min(n, 255), math.min(n / 2, 255)})
    end
  end
end

-- Save the image to a file
canvas:save("mandelbrot.png")
```

**Explanation:**

* The `image.new()` function creates a new canvas with the specified width and height.
* The `for` loops iterate over each pixel in the canvas.
* The `complex.new()` function creates a complex number from a real and imaginary part.
* The `while` loop calculates the number of iterations it takes for the complex number `c` to escape the Mandelbrot set.
* The `if` statement sets the color of the pixel based on the number of iterations.
* The `canvas:pixel()` function sets the color of a specific pixel in the canvas.
* The `canvas:save()` function saves the canvas to a file.