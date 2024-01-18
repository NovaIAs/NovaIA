```portuguol
programa teste {
    funcao inteiro valorAbsoluto(inteiro x) {
        se (x < 0) {
            retorna -x;
        }
        senao {
            retorna x;
        }
    }

    funcao inteiro maiorPrimo(inteiro n) {
        inteiro i = 2;
        boolean ehPrimo = falso;
        enquanto (ehPrimo == falso) {
            inteiro div = 0;
            para (inteiro j = 1; j <= i; j++) {
                se (i % j == 0) {
                    div = div + 1;
                }
            }
            se (div == 2) {
                ehPrimo = verdadeiro;
            }
            senao {
                i = i + 1;
            }
        }
        retorna i;
    }

    funcao cadeia caractere inverter(cadeia caractere str) {
        inteiro tamanho = comprimento(str);
        cadeia caractere strInvertida = "";
        para (inteiro i = tamanho - 1; i >= 0; i--) {
            strInvertida = strInvertida + str[i];
        }
        retorna strInvertida;
    }

    funcao real calcularMedia(vetor real notas, inteiro tamanho) {
        real soma = 0;
        para (inteiro i = 0; i < tamanho; i++) {
            soma = soma + notas[i];
        }
        real media = soma / tamanho;
        retorna media;
    }

    inteiro[] vetorInteiros = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    cadeia caractere[] vetorCadeias = {"um", "dois", "tres", "quatro", "cinco"};

    escreva("Valor absoluto de -5:", valorAbsoluto(-5));
    escreva("Maior primo menor que 100:", maiorPrimo(100));
    escreva("Inverso da palavra 'teste':", inverter("teste"));
    escreva("Media das notas: [10, 9, 8, 7, 6]", calcularMedia([10, 9, 8, 7, 6], 5));
    escreva("Elementos do vetor de inteiros:");
    para (inteiro x in vetorInteiros) {
        escreva(x);
    }
    escreva("Elementos do vetor de cadeias:");
    para (cadeia caractere str in vetorCadeias) {
        escreva(str);
    }
}
```

This program contains various functions and demonstrates their usage. Here's an explanation of the code:

- Function `valorAbsoluto`: This function takes an integer `x` as input and returns its absolute value.

- Function `maiorPrimo`: This function takes an integer `n` and finds the largest prime number less than or equal to `n`. It iterates through numbers starting from 2 and checks if each number is prime. A number is prime if it is only divisible by 1 and itself.

- Function `inverter`: This function takes a string `str` as input and returns the reverse of that string. It uses a loop to traverse the string in reverse order and concatenate the characters to form the reversed string.

- Function `calcularMedia`: This function takes an array of real numbers `notas` and calculates the average of those numbers. It iterates through the array, sums up the numbers, and then divides the sum by the length of the array to get the average.

- The program then declares two arrays: `vetorInteiros` which contains integers from 1 to 10, and `vetorCadeias` which contains string values from "um" to "cinco".

- The program uses `escreva` statements to display the results of the functions and to print the elements of the arrays.

This code demonstrates how to define and use functions in Portugol, and showcases different operations such as finding the absolute value, finding the largest prime number, reversing a string, calculating the average of numbers, and working with arrays.