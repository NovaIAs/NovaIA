```
.data
msg: .asciiz "Hello world!"
len: .equ $ - msg

.text
.global main
main:
    # Print message
    mov eax, 4
    mov ebx, 1
    mov ecx, msg
    mov edx, len
    int 0x80

    # Exit program
    mov eax, 1
    mov ebx, 0
    int 0x80
```

Explanation:

* The `.data` section defines a variable named `msg` which contains the message "Hello world!" and a variable named `len` which is the length of the message.
* The `.text` section defines the main function.
* The `main:` label marks the beginning of the main function.
* The `mov` instructions are used to load values into registers.
* The `int` instruction is used to make a system call.
* The first system call prints a message to the console.
* The second system call exits the program.