```forth
: WINDOW ( x y width height title -- )
  BEGIN
    3-DUP @ OVER @ FLUSH-KEY "New Window" MOVE
    (WIN32) X-Y-WIDTH-HEIGHT SWAP DROP  DUP OVER
    @ NOP XOR 4 +BL SWAP 4 +BL PUT
    3-DUP OVER @ OVER @ SWAP +BL PUT SWAP
    3-DUP OVER @ OVER @ SWAP -BL PUT SWAP -BL PUT
  END ;

: BUTTON ( x y width height text -- )
  BEGIN
    3-DUP @ OVER @ FLUSH-KEY MOVE
    (WIN32) X-Y-WIDTH-HEIGHT SWAP DROP  DUP SWAP
    +BL BL PUT SWAP +BL BL PUT 4 +BL BL PUT @ NOP XOR BL PUT
    3-DUP OVER @ OVER @ SWAP +BL SWAP BL PUT SWAP -BL PUT
    3-DUP OVER @ OVER @ SWAP -BL SWAP BL PUT SWAP -BL PUT
  END ;

: PRINT ( x y text -- )
  BEGIN
    3-DUP @ FLUSH-KEY MOVE 1+ SWAP BL PUT
    (WIN32) X-Y-TEXT SWAP 1- MTEXT SWAP BL PUT
  END ;

: CLOSE-WINDOW ( window -- )
  BEGIN
    1 CELLS @ (WIN32) CLOSE-WINDOW SWAP  CELLS @ BL PUT
  END ;



: MAIN ( -- )
  BEGIN
    0 WINDOW "Window 1" FLUSH-KEY
    1 WINDOW "Window 2" FLUSH-KEY
    20 20 120 20 "Button 1" BUTTON FLUSH-KEY
    80 80 80 20 "Button 2" BUTTON FLUSH-KEY
    20 100 "Button 3" PRINT FLUSH-KEY
    40 100 "Button 4" PRINT FLUSH-KEY
    CLOSE-WINDOW 0 FLUSH-KEY
    CLOSE-WINDOW 1 FLUSH-KEY
    KEY? FLUSH-KEY
  END ;

MAIN
```

Explanation:

- The code defines several Forth words using the `:` definition operator. These words, or functions, perform various tasks such as creating windows, buttons, and printing text.

- The `WINDOW` word creates a new window with a specified position, size, and title. It uses the `X-Y-WIDTH-HEIGHT` function to set the window's properties.

- The `BUTTON` word creates a new button within a window. It uses the `X-Y-WIDTH-HEIGHT` function to set the button's properties and the `MTEXT` function to set the button's text.

- The `PRINT` word prints text at a specified position within a window. It uses the `X-Y-TEXT` function to set the text's position.

- The `CLOSE-WINDOW` word closes a specified window. It uses the `CLOSE-WINDOW` function to close the window.

- The `MAIN` word is the entry point for the program. It creates two windows, four buttons, and some printed text. It then waits for a key to be pressed before closing the windows.

- The program uses the `FLUSH-KEY` word to clear the keyboard buffer before performing any input operations. This prevents unwanted keystrokes from affecting the program's behavior.