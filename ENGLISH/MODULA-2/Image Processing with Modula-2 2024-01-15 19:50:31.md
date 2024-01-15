```modula-2
MODULE ImageProcessing;
FROM Terminal IMPORT WriteString, WriteLn;
FROM TextIO IMPORT WriteCard;

PROCEDURE ProcessImage(image: ARRAY OF ARRAY OF INTEGER);
VAR
  i, j: INTEGER;
BEGIN
  FOR i := 0 TO HIGH(image) DO
    FOR j := 0 TO HIGH(image[i]) DO
      image[i][j] := image[i][j] * 2;
    END;
  END;
END ProcessImage;

PROCEDURE DisplayImage(image: ARRAY OF ARRAY OF INTEGER);
VAR
  i, j: INTEGER;
BEGIN
  FOR i := 0 TO HIGH(image) DO
    FOR j := 0 TO HIGH(image[i]) DO
      WriteCard(image[i][j], 3);
    END;
    WriteLn;
  END;
END DisplayImage;

VAR
  image: ARRAY[0..255, 0..255] OF INTEGER;
BEGIN
  WriteString("Enter the image data (256x256 pixels, 0-255): ");
  FOR i := 0 TO 255 DO
    FOR j := 0 TO 255 DO
      ReadCard(image[i][j]);
    END;
  END;

  ProcessImage(image);

  WriteString("Displaying the processed image: ");
  DisplayImage(image);
END ImageProcessing.
```

This code is a simple image processing program written in Modula-2. It takes a 256x256 pixel image as input, doubles the value of each pixel, and then displays the processed image.

The program is divided into three modules:

* **ImageProcessing:** This module contains the main program and the procedures for processing and displaying the image.
* **Terminal:** This module contains the procedures for writing text to the terminal.
* **TextIO:** This module contains the procedures for reading and writing text from and to the terminal.

The main program in the **ImageProcessing** module first prompts the user to enter the image data. It then calls the **ProcessImage** procedure to process the image, and then calls the **DisplayImage** procedure to display the processed image.

The **ProcessImage** procedure loops through each pixel in the image and doubles its value.

The **DisplayImage** procedure loops through each pixel in the image and displays its value on the terminal.

The program uses the **Terminal** and **TextIO** modules to write text to and read text from the terminal.