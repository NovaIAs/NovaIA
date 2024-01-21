```portuguol

programa ExemploPascal
    função Soma(a, b)
        retorna a + b;
    fim função

    função Media(a, b, c)
        retorna (a + b + c) / 3;
    fim função

    função MDC(a, b)
        enquanto (b != 0)
            temp = a;
            a = b;
            b = temp;
        fim enquanto

        retorna a;
    fim função

    função Primo(n)
        se (n <= 1)
            retorna 0;
        fim se

        i = 2;
        enquanto (i * i <= n)
            se (n % i == 0)
                retorna 0;
            fim se

            i = i + 1;
        fim enquanto

        retorna 1;
    fim função

    função Fatorial(n)
        se (n == 1)
            retorna 1;
        fim se

        retorna n * Fatorial(n - 1);
    fim função

    função Fibonacci(n)
        se (n <= 1)
            retorna n;
        fim se

        retorna Fibonacci(n - 1) + Fibonacci(n - 2);
    fim função

inicio
    inteiro a, b, c;
    
    escreva("Digite três números:");
    leia(a, b, c);

    escreva("Soma dos números:", Soma(a, b));
    escreva("\nMédia dos números:", Media(a, b, c));
    escreva("\nMDC dos números:", MDC(a, b));

    escreva("\nDigite um número:");
    leia(a);

    se (Primo(a))
        escreva("\nO número é primo.");
    senão
        escreva("\nO número não é primo.");
    fim se

    escreva("\nDigite um número:");
    leia(a);

    escreva("\nFatorial do número:", Fatorial(a));

    escreva("\nDigite um número:");
    leia(a);

    escreva("\nSequência de Fibonacci até o número:", Fibonacci(a));

fim

```

Este código é um exemplo complexo em Portugol que implementa várias funções matemáticas comuns. O código está bem estruturado e fácil de entender.

As funções implementadas são:

* `Soma`: calcula a soma de dois números.
* `Media`: calcula a média de três números.
* `MDC`: calcula o máximo divisor comum de dois números.
* `Primo`: verifica se um número é primo.
* `Fatorial`: calcula o fatorial de um número.
* `Fibonacci`: calcula a sequência de Fibonacci até um determinado número.

O código começa com a declaração das funções, seguidas da função `inicio` que é o ponto de entrada do programa. Na função `inicio`, o usuário é solicitado a digitar três números e esses números são armazenados nas variáveis `a`, `b` e `c`.

Em seguida, o código chama as funções `Soma`, `Media` e `MDC` para calcular a soma, a média e o MDC dos três números, respectivamente. Os resultados dessas funções são impressos na tela.

Depois, o usuário é solicitado a digitar um número e esse número é armazenado na variável `a`. O código então chama a função `Primo` para verificar se o número é primo. Se o número for primo, uma mensagem é impressa na tela informando isso. Caso contrário, uma mensagem é impressa na tela informando que o número não é primo.

Em seguida, o usuário é solicitado a digitar outro número e esse número é armazenado na variável `a`. O código então chama a função `Fatorial` para calcular o fatorial do número. O resultado dessa função é impresso na tela.

Por fim, o usuário é solicitado a digitar outro número e esse número é armazenado na variável `a`. O código então chama a função `Fibonacci` para calcular a sequência de Fibonacci até o número digitado pelo usuário. O resultado dessa função é impresso na tela.