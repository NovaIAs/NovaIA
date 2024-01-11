```portuguol
program ordenação_bubble_sort;

real vetor[10];

real função bubble_sort(real a[], int tam);
    int i, j;
    real aux;

    para (i <- 0 até tam - 2 passo 1) faça
        para (j <- 0 até tam - i - 2 passo 1) faça
            se (a[j] > a[j + 1]) então
                aux <- a[j];
                a[j] <- a[j + 1];
                a[j + 1] <- aux;
            fim-se;
        fim-para;
    fim-para;

    retorna a;
fim-função;

void main()
    int i;
    real aux;

    para (i <- 0 até 9 passo 1) faça
        escreva("Digite o valor ", i + 1, ":");
        leia(aux);
        vetor[i] <- aux;
    fim-para;

    vetor <- bubble_sort(vetor, 10);

    escreva("Vetor ordenado:");
    para (i <- 0 até 9 passo 1) faça
        escreva(vetor[i], " ");
    fim-para;
fim-programa;
```

This program sorts an array of 10 real numbers using the bubble sort algorithm. The bubble sort algorithm works by repeatedly comparing adjacent elements in the array and swapping them if they are out of order. The algorithm continues until no more swaps are needed.

The program first declares an array of 10 real numbers and a function called bubble_sort. The bubble_sort function takes an array and its size as arguments and returns the sorted array.

The bubble_sort function uses two nested loops to compare adjacent elements in the array. The outer loop iterates over the entire array, and the inner loop iterates over the remaining elements in the array. If two adjacent elements are out of order, they are swapped.

After the bubble_sort function has been called, the sorted array is printed to the console.

Here is an example of how the program would run:

```
Digite o valor 1: 5
Digite o valor 2: 3
Digite o valor 3: 1
Digite o valor 4: 2
Digite o valor 5: 4
Digite o valor 6: 8
Digite o valor 7: 6
Digite o valor 8: 7
Digite o valor 9: 9
Digite o valor 10: 10
Vetor ordenado: 1 2 3 4 5 6 7 8 9 10
```