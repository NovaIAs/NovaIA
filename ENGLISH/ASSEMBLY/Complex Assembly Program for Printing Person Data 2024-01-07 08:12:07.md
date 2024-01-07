```assembly
; This code is a complex and differentiated program in assembly language.
; It is highly unlikely to be repeated again, and it showcases the power and versatility of the assembly language.

; The program first defines a structure called "person" which contains
; the following fields:
; - name: a string of characters up to 20 bytes long
; - age: an integer representing the person's age
; - height: a floating-point number representing the person's height in meters
person:
    db    20 dup (?) ; Name, up to 20 characters
    dw    ? ; Age
    dd    ? ; Height
; The program then defines an array of 10 structures of type "person".
people:
    times 10 person ; Array of 10 people

; The program then defines a function called "print_person" which takes a
; pointer to a "person" structure as an argument and prints the person's
; name, age, and height to the console.
print_person:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get pointer to person structure
    mov     esi, [eax] ; Get person's name
    call    print_string ; Print the person's name
    mov     eax, [eax + 4] ; Get person's age
    call    print_integer ; Print the person's age
    mov     eax, [eax + 8] ; Get person's height
    call    print_float ; Print the person's height
    pop     ebp
    ret

; The program then defines a function called "main" which is the entry point
; of the program.
main:
    push    ebp
    mov     ebp, esp

    ; Initialize the array of people with some sample data.
    mov     eax, people
    mov     ebx, 0
    mov     ecx, 10
    loop:
        mov     [eax], "John Doe"
        mov     [eax + 4], 20
        mov     [eax + 8], 1.80
        add     eax, 24
        inc     ebx
        cmp     ebx, ecx
        jne     loop

    ; Call the "print_person" function for each person in the array.
    mov     eax, people
    mov     ebx, 0
    mov     ecx, 10
    loop2:
        call    print_person
        add     eax, 24
        inc     ebx
        cmp     ebx, ecx
        jne     loop2

    ; Exit the program.
    mov     eax, 0
    leave
    ret

; The program then defines the functions "print_string", "print_integer",
; and "print_float" which are used to print strings, integers, and floating-point
; numbers to the console.

print_string:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get pointer to string
    mov     esi, eax
    call    strlen ; Get length of string
    mov     ecx, eax
    rep     movsb ; Copy string to buffer
    call    print_buffer ; Print buffer
    pop     ebp
    ret

print_integer:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get integer to print
    call    itoa ; Convert integer to string
    mov     esi, eax
    call    strlen ; Get length of string
    mov     ecx, eax
    rep     movsb ; Copy string to buffer
    call    print_buffer ; Print buffer
    pop     ebp
    ret

print_float:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get floating-point number to print
    call    ftoa ; Convert floating-point number to string
    mov     esi, eax
    call    strlen ; Get length of string
    mov     ecx, eax
    rep     movsb ; Copy string to buffer
    call    print_buffer ; Print buffer
    pop     ebp
    ret

; The program then defines the functions "strlen", "itoa", "ftoa", and
; "print_buffer" which are used to calculate the length of a string,
; convert an integer to a string, convert a floating-point number to a
; string, and print a buffer to the console.

strlen:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get pointer to string
    xor     ecx, ecx
    mov     al, [eax + ecx]
    cmp     al, 0
    jz      done
    inc     ecx
    jmp     strlen

done:
    mov     eax, ecx
    leave
    ret

itoa:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get integer to convert
    mov     ebx, 10 ; Base 10
    xor     ecx, ecx ; Counter
    xor     edx, edx ; Quotient
    div     ebx
    mov     [ebp - 4], edx ; Store quotient
    inc     ecx
    cmp     eax, 0
    jnz     itoa_loop

    ; Convert the quotient to a string
    mov     eax, [ebp - 4]
    mov     esi, [ebp - 12] ; Buffer pointer
    call    itoa_convert

    ; Convert the remaining digits to a string
    mov     eax, ecx
    mov     esi, [ebp - 12] ; Buffer pointer
    call    itoa_convert

    ; Return the buffer pointer
    mov     eax, [ebp - 12]
    leave
    ret

itoa_loop:
    mov     edx, eax
    div     ebx
    mov     [ebp - 4], edx ; Store quotient
    inc     ecx
    cmp     eax, 0
    jnz     itoa_loop

    ; Convert the quotient to a string
    mov     eax, [ebp - 4]
    mov     esi, [ebp - 12] ; Buffer pointer
    call    itoa_convert

    ; Convert the remaining digits to a string
    mov     eax, ecx
    mov     esi, [ebp - 12] ; Buffer pointer
    call    itoa_convert

    ; Return the buffer pointer
    mov     eax, [ebp - 12]
    leave
    ret

itoa_convert:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get number to convert
    mov     ebx, [ebp + 12] ; Get buffer pointer
    xor     edx, edx
    mov     dl, 48 ; '0'
    add     dl, al
    mov     [ebx + ecx], dl
    inc     ecx
    cmp     eax, 0
    jnz     itoa_convert

    ; Add null terminator to the string
    mov     [ebx + ecx], 0
    pop     ebp
    ret

ftoa:
    push    ebp
    mov     ebp, esp
    mov     eax, [ebp + 8] ; Get floating-point number to convert
    fistp   [ebp - 8] ; Convert floating-point number to integer
    mov     edx, [ebp - 8] ; Get integer part
    mov     eax, [ebp + 8] ; Get floating-point number again
    fld     dword [ebp - 8] ; Load integer part into floating-point register
    fsubp   st(0), st(1) ; Subtract integer part from floating-point number
    fistp   [ebp - 12] ; Convert fractional part to integer
    mov     ebx, 10 ; Base 10
    xor     ecx, ecx ; Counter
    xor     edx, edx ; Quotient
    div     ebx
    mov     [ebp - 4], edx ; Store quotient
    inc     ecx
    cmp     eax, 0
    jnz     ftoa_loop

    ; Convert the quotient to a string
    mov     eax, [ebp - 4]
    mov     esi, [ebp - 16] ; Buffer pointer
    call    itoa_convert

    ; Convert the remaining digits to a string
    mov     eax, ecx
    mov     esi, [ebp - 16] ; Buffer pointer
    call    itoa_convert

    ; Add decimal point to the string
    mov     [ebp - 16 + ecx], '.'
    inc     ecx

    ; Convert the fractional part to a string
    mov     eax, [ebp - 12]
    mov     esi, [ebp - 16 + ecx] ; Buffer pointer
    call    itoa_convert

    ; Add null terminator to the string
    mov     [ebp - 16 + ecx], 0
    pop     ebp
    ret