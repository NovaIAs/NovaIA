Program Prime_Factorization

Implicit none

Integer :: Number, i, j
Logical :: Found

Write(*,*) "Enter a Number: "
Read (*,*) Number

Write(*,*) "Prime Factors of ", Number, " are: "

i = 2
Do While (i <= Number)
    If (Number Mod i == 0) Then
        Number = Number / i
        Write(*,*) i
        Found = .True.
    Else
        Found = .False.
        i = i + 1
    End If
End Do

If (.Not. Found) Then
    Write(*,*) Number, " is a Prime Number"
End If

End Program Prime_Factorization

Explanation:

This code is a program in FORTRAN that performs prime factorization of a number. Prime factorization is the process of expressing a number as a product of its prime factors.

The program starts by declaring the necessary variables:

Number: This is the number that we want to factor.
i: This is a loop variable.
j: This is a loop variable.
Found: This is a logical variable that indicates whether a prime factor has been found.
The program then prompts the user to enter a number, and reads the input.

Next, the program initializes the loop variable i to 2.

The program then enters a while loop, which continues until i is greater than the number.

Inside the loop, the program checks if the number is divisible by i. If it is, then i is a prime factor of the number, and the program prints out i and divides the number by i.

If the number is not divisible by i, then the program increments i by 1 and continues to the next iteration of the loop.

After the loop, the program checks if the number is still greater than 1. If it is, then the number is a prime number, and the program prints out the number.

Finally, the program ends.