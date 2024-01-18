```assembly
.data
phrase: .asciiz "Hello, world!"
length: .int 13

.text
.global main
main:
    # Move the address of the string into the %rdi register
    movq $phrase, %rdi

    # Move the length of the string into the %rsi register
    movq $length, %rsi

    # Call the write system call to print the string
    callq write

    # Exit the program
    xorq %rax, %rax
    ret

.section .rodata
.asciz "Hello, world!"
```

This code is a simple program that prints the string "Hello, world!" to the console. It uses the write system call to print the string. The program is divided into three sections: the data section, the text section, and the rodata section.

The data section contains the string "Hello, world!" and the length of the string. The text section contains the main function, which is the entry point of the program. The rodata section contains the string "Hello, world!" in a read-only format.

The main function first moves the address of the string into the %rdi register. This register is used by the write system call to specify the address of the data to be written. Next, the main function moves the length of the string into the %rsi register. This register is used by the write system call to specify the length of the data to be written.

Finally, the main function calls the write system call to print the string. The write system call takes two arguments: the address of the data to be written and the length of the data to be written. After the write system call returns, the main function exits the program by returning 0.