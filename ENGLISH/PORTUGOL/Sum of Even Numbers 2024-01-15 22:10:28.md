```portuguol
programa "Soma de números pares"
funcao soma_par(n)
    soma <- 0
    enquanto n > 0 faca
        se n % 2 = 0 entao
            soma <- soma + n
        fim_se
        n <- n - 1
    fim_enquanto
    retorne soma
fim_funcao

inicio
    n <- leia_inteiro("Digite um número inteiro:")
    soma <- soma_par(n)
    escreva_real("A soma dos números pares de 1 a ", n, " é ", soma)
fim

```

This code calculates the sum of all even numbers from 1 to a given integer `n`.

The program defines a function called `soma_par` which takes an integer as input and returns the sum of all even numbers from 1 to that integer.

The function uses a while loop to iterate from 1 to `n`, and inside the loop, it checks if the current number is even. If it is, the current number is added to the sum.

After the loop, the function returns the sum of all the even numbers.

The main program then prompts the user to enter an integer, calls the `soma_par` function to calculate the sum of all even numbers from 1 to that integer, and prints the result to the console.