```assembly
; This program simulates a simple calculator that can perform arithmetic operations on two numbers.

; Define the segment where the code will be located
segment .text

; Define the entry point of the program
global _start

; Declare the main function
_start:

    ; Declare the variables that will store the two numbers and the result
    mov eax, 0            ; eax will store the first number
    mov ebx, 0            ; ebx will store the second number
    mov ecx, 0            ; ecx will store the result

    ; Prompt the user to enter the first number
    mov eax, 4
    mov ebx, 1
    mov ecx, message1
    mov edx, len1
    int 0x80

    ; Read the first number from the user
    mov eax, 3
    mov ebx, 0
    mov ecx, buffer
    mov edx, 10
    int 0x80

    ; Convert the string representation of the first number to an integer
    call atoi

    ; Store the first number in eax
    mov eax, [buffer]

    ; Prompt the user to enter the second number
    mov eax, 4
    mov ebx, 1
    mov ecx, message2
    mov edx, len2
    int 0x80

    ; Read the second number from the user
    mov eax, 3
    mov ebx, 0
    mov ecx, buffer
    mov edx, 10
    int 0x80

    ; Convert the string representation of the second number to an integer
    call atoi

    ; Store the second number in ebx
    mov ebx, [buffer]

    ; Determine the operation to be performed based on the user's input
    mov eax, 4
    mov ebx, 1
    mov ecx, prompt
    mov edx, len3
    int 0x80

    ; Read the operation from the user
    mov eax, 3
    mov ebx, 0
    mov ecx, buffer
    mov edx, 10
    int 0x80

    ; Convert the string representation of the operation to an integer
    call atoi

    ; Store the operation in eax
    mov eax, [buffer]

    ; Perform the appropriate operation based on the user's input
    cmp eax, 1             ; Check if the operation is addition
    je add_numbers         ; If so, jump to the add_numbers label
    cmp eax, 2             ; Check if the operation is subtraction
    je subtract_numbers    ; If so, jump to the subtract_numbers label
    cmp eax, 3             ; Check if the operation is multiplication
    je multiply_numbers    ; If so, jump to the multiply_numbers label
    cmp eax, 4             ; Check if the operation is division
    je divide_numbers      ; If so, jump to the divide_numbers label

    ; If the operation is not valid, print an error message and exit the program
    mov eax, 4
    mov ebx, 1
    mov ecx, error_message
    mov edx, len4
    int 0x80

    ; Exit the program with an error code
    mov eax, 1
    mov ebx, 0
    int 0x80

; Define the function to add two numbers
add_numbers:

    ; Add the two numbers in eax and ebx
    add eax, ebx

    ; Store the result in ecx
    mov ecx, eax

    ; Jump to the print_result label
    jmp print_result

; Define the function to subtract two numbers
subtract_numbers:

    ; Subtract the second number from the first number in eax and ebx
    sub eax, ebx

    ; Store the result in ecx
    mov ecx, eax

    ; Jump to the print_result label
    jmp print_result

; Define the function to multiply two numbers
multiply_numbers:

    ; Multiply the two numbers in eax and ebx
    mul ebx

    ; Store the result in ecx
    mov ecx, eax

    ; Jump to the print_result label
    jmp print_result

; Define the function to divide two numbers
divide_numbers:

    ; Check if the second number is zero
    cmp ebx, 0

    ; If the second number is zero, print an error message and exit the program
    jz divide_by_zero

    ; Divide the first number by the second number in eax and ebx
    div ebx

    ; Store the result in ecx
    mov ecx, eax

    ; Jump to the print_result label
    jmp print_result

; Define the function to print the result
print_result:

    ; Convert the result in ecx to a string
    call itoa

    ; Print the result to the console
    mov eax, 4
    mov ebx, 1
    mov ecx, buffer
    mov edx, len5
    int 0x80

    ; Exit the program with a success code
    mov eax, 0
    mov ebx, 0
    int 0x80

; Define the function to convert a string to an integer
atoi:

    ; Save the current value of ecx on the stack
    push ecx

    ; Initialize the result to zero
    mov eax, 0

    ; Iterate over the string character by character
    mov ecx, [esp]
1:
    ; Get the current character from the string
    mov al, [ecx]

    ; Check if the current character is a digit
    cmp al, '0'
    jl not_a_digit
    cmp al, '9'
    jg not_a_digit

    ; Convert the current character to a digit
    sub al, '0'

    ; Multiply the result by 10 to make room for the next digit
    mul 10

    ; Add the current digit to the result
    add eax, al

    ; Move to the next character in the string
    inc ecx

    ; Check if the current character is not a digit
    cmp al, 0
    jne 1b

;not_a_digit:

    ; Restore the original value of ecx from the stack
    pop ecx

    ; Return the result
    ret

; Define the function to convert an integer to a string
itoa:

    ; Save the current value of ecx on the stack
    push ecx

    ; Initialize the buffer pointer and the index of the current digit
    mov ecx, buffer
    mov edi, 0

    ; Check if the number is negative
    cmp eax, 0
    jl negative

    ; Convert the number to a string of digits
    mov ebx, 10
1:
    ; Divide the number by 10 and store the remainder in edx
    div ebx

    ; Convert the remainder to a digit
    add edx, '0'

    ; Store the digit in the buffer
    mov [ecx + edi], edx

    ; Increment the index of the current digit
    inc edi

    ; Check if the number is zero
    cmp eax, 0
    jne 1b

;negative:

    ; If the number is negative, prepend a minus sign to the string
    cmp eax, 0
    jl negative_sign

    ; Restore the original value of ecx from the stack
    pop ecx

    ; Return the pointer to the buffer
    ret

;negative_sign:

    ; Store a minus sign in the buffer
    mov [ecx + edi], '-'

    ; Increment the index of the current digit
    inc edi

    ; Restore the original value of ecx from the stack
    pop ecx

    ; Return the pointer to the buffer
    ret

; Define the data segment
segment .data

; Define the messages that will be displayed to the user
message1:      db 'Enter the first number: '
len1:          equ $ - message1
message2:      db 'Enter the second number: '
len2:          equ $ - message2
prompt:        db 'Enter the operation (+, -, *, /): '
len3:          equ $ - prompt
error_message: db 'Invalid operation'
len4:          equ $ - error_message

; Define the buffer for storing the user's input
buffer:        times 10 db 0

; Define the array of digits for converting numbers to strings
digits:        db '0123456789'
len5:          equ $ - digits
```

**Explanation:**

This assembly program simulates a simple calculator that can perform arithmetic operations on two numbers. The program first prompts the user to enter the two numbers and the operation to be performed. It then performs the appropriate operation and prints the result to the console.

The program is divided into several sections:

* The `.text` segment contains the code for the program.
* The `.data` segment contains the data that is used by the program, such as the messages that are displayed to the user and the buffer for storing the user's input.

The main function of the program is `_start`. This function first prompts the user to enter the two numbers and the operation to be performed. It then calls the appropriate function to perform the operation and print the result to the console.

The program uses a number of functions to perform its tasks. These functions include:

* `atoi`: This function converts a string representation of a number to an integer.
* `itoa`: This function converts an integer to a string representation.
* `add_numbers`: This function adds two numbers together.
* `subtract_numbers`: This function subtracts the second number from the first number.
* `multiply_numbers`: This function multiplies two numbers together.
* `divide_numbers`: This function divides the first number by the second number.
* `print_result`: This function prints the result of the operation to the console.

The program uses a number of constants to define the messages that are displayed to the user and the buffer for storing the user's input. These constants include:

* `message1`: This constant contains the message that is displayed to the user when prompting for the first number.
* `len1`: This constant contains the length of the `message1` string.
* `message2`: This constant contains the message that is displayed to the user when prompting for the second number.
* `len2`: This constant contains the length of the `message2` string.
* `prompt`: This constant contains the message that is displayed to the user when prompting for the operation to be performed.
* `len3`: This constant contains the length of the `prompt` string.
* `error_message`: This constant contains the error message that is displayed if the user enters an invalid operation.
* `len4`: This constant contains the length of the `error_message` string.
* `buffer`: This constant defines the buffer for storing the user's input.
* `digits`: This constant contains the array of digits that is used to convert numbers to strings.
* `len5`: This constant contains the length of the `digits` array.