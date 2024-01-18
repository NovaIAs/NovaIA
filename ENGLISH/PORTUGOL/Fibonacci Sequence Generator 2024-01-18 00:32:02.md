```portugol
/* This program calculates the Fibonacci sequence up to a specified number of terms. */

inicio

    inteiro n, i, j, k;
    real f;

    escreva("Digite o número de termos da sequência de Fibonacci: ");
    leia(n);

    /* Initialize the first two terms of the sequence. */
    i := 0;
    j := 1;

    /* Calculate the remaining terms of the sequence. */
    para k de 2 ate n faca
        f := i + j;
        i := j;
        j := f;
    fim_para

    /* Print the Fibonacci sequence. */
    escreva("A sequência de Fibonacci é: ");
    escreva(i);
    escreva(", ");
    escreva(j);
    para k de 3 ate n faca
        escreva(", ");
        escreva(f);
        i := j;
        j := f;
        f := i + j;
    fim_para

fim

```

Explanation:

* The `inicio` statement starts the program.
* The `inteiro` statement declares the integer variables `n`, `i`, `j`, and `k`.
* The `real` statement declares the real variable `f`.
* The `escreva` statement prompts the user to enter the number of terms of the Fibonacci sequence.
* The `leia` statement reads the user's input and stores it in the variable `n`.
* The `para` statement initializes a loop that will run from 2 to `n`.
* The `f := i + j;` statement calculates the next term of the Fibonacci sequence.
* The `i := j;` and `j := f;` statements update the values of `i` and `j` for the next iteration of the loop.
* The `escreva` statement prints the current term of the Fibonacci sequence.
* The `fim_para` statement ends the loop.
* The `fim` statement ends the program.