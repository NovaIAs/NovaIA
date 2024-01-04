```portugol
program Fibonacci
    real i, j, soma, n
    escreva("Digite um número inteiro positivo: ")
    leia(n)
    i := 0
    j := 1
    soma := 0
    enquanto i <= n faca
        soma := soma + i
        k := i
        i := j
        j := k + i
    fim_enquanto
    escreva("A soma dos ", n, " primeiros números da sequência de Fibonacci é ", soma)
fim_programa
```

This program calculates the sum of the first n numbers of the Fibonacci sequence. The Fibonacci sequence is a series of numbers in which each number is the sum of the two preceding ones, starting from 0 and 1.

The program first asks the user to enter a positive integer n. Then, it initializes three variables: i, j, and soma. i and j are used to keep track of the current and previous numbers in the sequence, respectively. soma is used to store the sum of the numbers in the sequence.

The program then enters a while loop that continues until i is greater than n. Inside the loop, the program adds the current number i to the sum soma. Then, it updates i and j to the next two numbers in the sequence. This is done by setting k to the current value of i, setting i to the current value of j, and setting j to the sum of k and i.

After the loop, the program prints the sum of the first n numbers of the Fibonacci sequence.