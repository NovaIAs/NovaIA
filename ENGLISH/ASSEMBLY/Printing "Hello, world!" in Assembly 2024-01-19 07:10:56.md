```assembly
; ***** START OF PROGRAM *****

; Define the base address of the video memory.
VIDEO_BASE equ 0xB8000

; Clear the screen and set the cursor to the top-left corner.
mov ax, 3 ; Set the video mode to text mode.
int 10h ; Call the BIOS to set the video mode.
mov ah, 2 ; Set the cursor position.
mov al, 0 ; Set the cursor position to the top-left corner.
int 10h ; Call the BIOS to set the cursor position.

; Print a message to the screen.
mov ah, 9 ; Set the function to print a string.
mov dx, VIDEO_BASE ; Set the destination address to the video memory.
mov al, 0 ; Set the attribute byte to white on black.
mov es:[di], "Hello, world!" ; Copy the string to the video memory.
int 21h ; Call the BIOS to print the string.

; Wait for the user to press a key.
mov ah, 1 ; Set the function to wait for a key.
int 21h ; Call the BIOS to wait for a key.

; Exit the program.
mov ax, 4C00h ; Set the exit code to 0.
int 21h ; Call the BIOS to exit the program.

; ***** END OF PROGRAM *****
```

This code is a very simple assembly program that prints the message "Hello, world!" to the screen and then waits for the user to press a key before exiting. The program uses the BIOS interrupt services to perform these tasks.

The first section of the code defines the base address of the video memory. This address is used by the BIOS interrupt services to access the video memory.

The next section of the code clears the screen and sets the cursor to the top-left corner. This is done using the BIOS interrupt service 03h.

The next section of the code prints the message "Hello, world!" to the screen. This is done using the BIOS interrupt service 09h.

The next section of the code waits for the user to press a key. This is done using the BIOS interrupt service 01h.

The final section of the code exits the program. This is done using the BIOS interrupt service 21h.

This code is a very basic example of an assembly program. It can be used as a starting point for more complex programs.