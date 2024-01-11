**Program:** Advanced Image Processing

**Description:** This COOL program showcases a complex and differentiated implementation of various image processing techniques in a single code. It features a user-friendly interface with multiple options, allowing users to explore and apply a wide range of filters, transformations, and effects to their images.

```cool
program ImageProcessing {
  -- Main function
  main:
    -- Initialize the image processing system
    image := new ImageSystem();

    -- Load an image from a file
    image.loadFromFile("image.png");

    -- Display the original image
    image.displayOriginal();

    -- Present a menu of image processing options
    while (true) {
      -- Clear the screen
      clearScreen();

      -- Display the menu options
      print("Image Processing Options:");
      print("1. Apply a grayscale filter");
      print("2. Apply a sepia filter");
      print("3. Apply a blur filter");
      print("4. Apply a sharpen filter");
      print("5. Apply an edge detection filter");
      print("6. Rotate the image 90 degrees clockwise");
      print("7. Rotate the image 90 degrees counterclockwise");
      print("8. Flip the image horizontally");
      print("9. Flip the image vertically");
      print("10. Resize the image");
      print("11. Crop the image");
      print("12. Save the modified image");
      print("13. Exit the program");

      -- Get the user's choice
      choice := readInt();

      -- Handle the user's choice
      case choice of
        1: image.applyGrayscaleFilter();
        2: image.applySepiaFilter();
        3: image.applyBlurFilter();
        4: image.applySharpenFilter();
        5: image.applyEdgeDetectionFilter();
        6: image.rotateClockwise();
        7: image.rotateCounterclockwise();
        8: image.flipHorizontally();
        9: image.flipVertically();
        10: image.resize();
        11: image.crop();
        12: image.saveToFile("modified_image.png");
        13: break;  -- Exit the loop and terminate the program
        else: print("Invalid choice. Please enter a number between 1 and 13.");
      endcase;

      -- Display the modified image
      image.displayModified();

      -- Pause the program to allow the user to view the result
      pause();
    }
  end main;

  -- Class representing an image processing system
  class ImageSystem {
    -- Constructor
    constructor ImageSystem() {
      originalImage := null;
      modifiedImage := null;
    }

    -- Load an image from a file
    loadFromFile: filename: String -> Void {
      originalImage := new Image(filename);
      modifiedImage := originalImage.clone();
    }

    -- Display the original image
    displayOriginal: Void -> Void {
      clearScreen();
      print("Original Image:");
      originalImage.display();
      pause();
    }

    -- Display the modified image
    displayModified: Void -> Void {
      clearScreen();
      print("Modified Image:");
      modifiedImage.display();
      pause();
    }

    -- Apply a grayscale filter to the modified image
    applyGrayscaleFilter: Void -> Void {
      modifiedImage.grayscale();
    }

    -- Apply a sepia filter to the modified image
    applySepiaFilter: Void -> Void {
      modifiedImage.sepia();
    }

    -- Apply a blur filter to the modified image
    applyBlurFilter: Void -> Void {
      modifiedImage.blur();
    }

    -- Apply a sharpen filter to the modified image
    applySharpenFilter: Void -> Void {
      modifiedImage.sharpen();
    }

    -- Apply an edge detection filter to the modified image
    applyEdgeDetectionFilter: Void -> Void {
      modifiedImage.edgeDetection();
    }

    -- Rotate the modified image 90 degrees clockwise
    rotateClockwise: Void -> Void {
      modifiedImage.rotateClockwise();
    }

    -- Rotate the modified image 90 degrees counterclockwise
    rotateCounterclockwise: Void -> Void {
      modifiedImage.rotateCounterclockwise();
    }

    -- Flip the modified image horizontally
    flipHorizontally: Void -> Void {
      modifiedImage.flipHorizontally();
    }

    -- Flip the modified image vertically
    flipVertically: Void -> Void {
      modifiedImage.flipVertically();
    }

    -- Resize the modified image
    resize: Void -> Void {
      width := readInt();
      height := readInt();
      modifiedImage.resize(width, height);
    }

    -- Crop the modified image
    crop: Void -> Void {
      x1 := readInt();
      y1 := readInt();
      x2 := readInt();
      y2 := readInt();
      modifiedImage.crop(x1, y1, x2, y2);
    }

    -- Save the modified image to a file
    saveToFile: filename: String -> Void {
      modifiedImage.saveToFile(filename);
    }

    -- Private fields
    originalImage: Image;
    modifiedImage: Image;
  }

  -- Class representing an image
  class Image {
    -- Constructor
    constructor Image(filename: String) {
      pixels := readPixelsFromFile(filename);
      width := pixels.width;
      height := pixels.height;
    }

    -- Clone the image
    clone: Void -> Image {
      return new Image(pixels.clone());
    }

    -- Display the image
    display: Void -> Void {
      for y in 0 to height - 1 do {
        for x in 0 to width - 1 do {
          printPixel(pixels[x][y]);
        }
        print();
      }
    }

    -- Apply a grayscale filter to the image
    grayscale: Void -> Void {
      for y in 0 to height - 1 do {
        for x in 0 to width - 1 do {
          pixel := pixels[x][y];
          grayValue := (pixel.red + pixel.green + pixel.blue) / 3;
          pixels[x][y] := new Pixel(grayValue, grayValue, grayValue, pixel.alpha);
        }
      }
    }

    -- Apply a sepia filter to the image
    sepia: Void -> Void {
      for y in 0 to height - 1 do {
        for x in 0 to width - 1 do {
          pixel := pixels[x][y];
          red := pixel.red * 0.393 + pixel.green * 0.769 + pixel.blue * 0.189;
          green := pixel.red * 0.349 + pixel.green * 0.686 + pixel.blue * 0.168;
          blue := pixel.red * 0.272 + pixel.green * 0.534 + pixel.blue * 0.131;
          pixels[x][y] := new Pixel(red, green, blue, pixel.alpha);
        }
      }
    }

    -- Apply a blur filter to the image
    blur: Void -> Void {
      kernel := [[1, 2, 1], [2, 4, 2], [1, 2, 1]];
      blurredPixels := new Pixels(width, height);
      for y in 1 to height - 2 do {
        for x in 1 to width - 2 do {
          sumRed := 0;
          sumGreen := 0;
          sumBlue := 0;
          sumAlpha := 0;
          for ky in -1 to 1 do {
            for kx in -1 to 1 do {
              pixel := pixels[x + kx][y + ky];
              sumRed += pixel.red * kernel[kx + 1][ky + 1];
              sumGreen += pixel.green * kernel[kx + 1][ky + 1];
              sumBlue += pixel.blue * kernel[kx + 1][ky + 1];
              sumAlpha += pixel.alpha * kernel[kx + 1][ky + 1];
            }
          }
          blurredPixels[x][y] := new Pixel(sumRed / 16, sumGreen / 1