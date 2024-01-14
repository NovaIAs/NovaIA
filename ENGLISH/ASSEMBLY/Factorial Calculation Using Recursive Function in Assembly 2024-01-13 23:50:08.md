```assembly
; This program calculates the factorial of a number using a recursive function.

.data
; Declare a variable to store the number to calculate the factorial of.
number: .word 5

; Declare a variable to store the result of the factorial calculation.
factorial: .word 0

.text
; Define the main function.
main:
    ; Push the number to calculate the factorial of onto the stack.
    pushl number

    ; Call the factorial function to calculate the factorial of the number.
    call factorial

    ; Pop the result of the factorial calculation off the stack.
    popl factorial

    ; Print the result of the factorial calculation to the console.
    printf "The factorial of %d is %d\n", number, factorial

    ; Return from the main function.
    ret

; Define the factorial function.
factorial:
    ; Push the base case value, 1, onto the stack.
    pushl 1

    ; Check if the number to calculate the factorial of is equal to 1.
    cmpl number, 1
    je done

    ; Decrement the number to calculate the factorial of by 1.
    decl number

    ; Push the number to calculate the factorial of onto the stack.
    pushl number

    ; Call the factorial function recursively to calculate the factorial of the number.
    call factorial

    ; Pop the result of the factorial calculation off the stack.
    popl eax

    ; Multiply the result of the factorial calculation by the number to calculate the factorial of.
    imull eax, number

    ; Pop the base case value, 1, off the stack.
    popl eax

    ; Return the result of the factorial calculation.
    ret

; Define the done label.
done:
    ; Return the result of the factorial calculation.
    ret
```

This program calculates the factorial of a number using a recursive function. The factorial of a number is the product of all the positive integers less than or equal to that number. For example, the factorial of 5 is 5 * 4 * 3 * 2 * 1 = 120.

The program first declares a variable to store the number to calculate the factorial of and a variable to store the result of the factorial calculation. The main function then pushes the number to calculate the factorial of onto the stack and calls the factorial function to calculate the factorial of the number. The factorial function first checks if the number to calculate the factorial of is equal to 1. If it is, then the function returns 1. Otherwise, the function decrements the number to calculate the factorial of by 1, pushes the number to calculate the factorial of onto the stack, and calls the factorial function recursively to calculate the factorial of the number. The function then pops the result of the factorial calculation off the stack, multiplies the result of the factorial calculation by the number to calculate the factorial of, and pops the base case value, 1, off the stack. The function then returns the result of the factorial calculation.

The main function then pops the result of the factorial calculation off the stack and prints the result of the factorial calculation to the console.