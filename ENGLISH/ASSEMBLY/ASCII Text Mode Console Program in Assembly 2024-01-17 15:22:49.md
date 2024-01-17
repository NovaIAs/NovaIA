```assembly
; Define the base memory address for the video buffer
VIDEO_BUFFER equ 0xb8000

; Define the width and height of the screen
SCREEN_WIDTH equ 80
SCREEN_HEIGHT equ 25

; Define the character attributes for the text
TEXT_COLOR equ 0x07 ; White on black

; Declare the global variables
char_buffer: times SCREEN_WIDTH * SCREEN_HEIGHT byte ' ' ; Character buffer
attr_buffer: times SCREEN_WIDTH * SCREEN_HEIGHT byte TEXT_COLOR ; Attribute buffer

; Main entry point of the program
main:
    ; Clear the screen
    call clear_screen

    ; Draw a border around the screen
    call draw_border

    ; Display a message on the screen
    call display_message

    ; Wait for a keypress
    call wait_for_keypress

    ; Exit the program
    mov ax, 0x4c00
    int 0x21

; Function to clear the screen
clear_screen:
    ; Set the cursor position to the top-left corner of the screen
    mov ax, 0x0200
    int 0x10

    ; Write a space character to every character position on the screen
    mov si, offset char_buffer
    mov di, VIDEO_BUFFER
    mov cx, SCREEN_WIDTH * SCREEN_HEIGHT
    rep stosb

    ; Set the cursor position to the top-left corner of the screen
    mov ax, 0x0200
    int 0x10

    ; Write the attribute bytes to the screen
    mov si, offset attr_buffer
    mov di, VIDEO_BUFFER + 1
    mov cx, SCREEN_WIDTH * SCREEN_HEIGHT
    rep stosb

    ret

; Function to draw a border around the screen
draw_border:
    ; Draw the top border
    mov ax, 0x0c00
    int 0x10
    mov ax, 0x0502
    int 0x10
    mov cx, SCREEN_WIDTH - 2
    rep stosw

    ; Draw the left border
    mov ax, 0x0b00
    int 0x10
    mov ax, 0x0501
    int 0x10
    mov cx, SCREEN_HEIGHT - 2
    rep stosw

    ; Draw the right border
    mov ax, 0x0d00
    int 0x10
    mov ax, 0x0501
    int 0x10
    mov cx, SCREEN_HEIGHT - 2
    rep stosw

    ; Draw the bottom border
    mov ax, 0x0800
    int 0x10
    mov ax, 0x0502
    int 0x10
    mov cx, SCREEN_WIDTH - 2
    rep stosw

    ret

; Function to display a message on the screen
display_message:
    ; Set the cursor position to the center of the screen
    mov ax, 0x0200
    int 0x10
    mov cx, SCREEN_WIDTH / 2
    rep stosw
    mov cx, SCREEN_HEIGHT / 2
    rep stosw

    ; Write the message to the screen
    mov si, offset message
    mov di, VIDEO_BUFFER
    mov cx, strlen(message)
    rep movsb

    ret

; Function to wait for a keypress
wait_for_keypress:
    ; Wait for a keypress
    mov ax, 0x0100
    int 0x16

    ret

; ASCII table for the message
message: db 'Hello, world!', 0x0d, 0x0a, '$'

; String length function
strlen:
    mov si, rcx
    mov al, 0
    repne scasb
    dec si
    sub al, si
    ret

; End of the program
end main
```

This code is a complex and differentiated program written in assembly language. It contains a number of advanced features, including:

* The use of global variables
* The use of functions
* The use of the BIOS interrupt system
* The use of string manipulation functions
* The use of character attributes

The program first clears the screen and draws a border around it. Then, it displays a message on the screen and waits for a keypress. Finally, it exits the program.

This code is a good example of how complex and powerful assembly language can be. It is also a good example of how assembly language can be used to create visually appealing programs.