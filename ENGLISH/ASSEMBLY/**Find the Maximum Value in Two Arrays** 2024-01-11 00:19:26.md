```assembly
.model small
.stack 1024

.data
array1 byte 1, 2, 3, 4, 5
array2 byte 6, 7, 8, 9, 10
sum byte 0

.code
main proc
    mov ax, @data
    mov ds, ax

    mov si, array1
    mov di, array2
    xor cx, cx

    find_max:
    cmp byte ptr [si], byte ptr [di]
    jl find_max_end
    mov byte ptr [sum], byte ptr [si]

    find_max_end:

    inc si
    inc di
    inc cx
    cmp cx, 5
    jl find_max

    mov al, byte ptr [sum]
    mov ah, 2
    int 21h

    mov ax, 4C00h
    int 21h

main endp

end main
```

Explanation:

* The program starts by defining a data segment and a code segment.
* The data segment contains two arrays of bytes, `array1` and `array2`, and a byte variable `sum`.
* The code segment contains the main procedure, which is the entry point of the program.
* The main procedure first moves the address of the data segment into the DS register, so that the program can access the data variables.
* Then, it moves the addresses of the two arrays into the SI and DI registers, respectively, and initializes the CX register to 0.
* The `find_max` label marks the beginning of a loop that compares the elements of the two arrays and finds the maximum value.
* Inside the loop, the `cmp` instruction compares the bytes at the current addresses pointed to by SI and DI.
* If the byte at SI is less than the byte at DI, the loop continues to the next element.
* If the byte at SI is greater than or equal to the byte at DI, the byte at SI is moved into the `sum` variable.
* After the loop, the value stored in `sum` is printed to the console using the `int 21h` instruction with the `ah` register set to 2.
* Finally, the program terminates by executing the `int 21h` instruction with the `ax` register set to 4C00h.