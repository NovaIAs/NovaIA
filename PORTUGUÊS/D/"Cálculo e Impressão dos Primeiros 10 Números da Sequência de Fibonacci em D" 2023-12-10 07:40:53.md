Claro! Aqui está um exemplo de um código complexo em D, explicando passo a passo:

```D
import std.stdio;

void main()
{
    int fibonacci(int n)
    {
        if (n <= 1)
            return n;
        else
            return fibonacci(n - 1) + fibonacci(n - 2);
    }

    void imprimirFibonacci(int quantidade)
    {
        for (int i = 0; i < quantidade; i++)
        {
            writef("Fibonacci(%d) = %d\n", i, fibonacci(i));
        }
    }

    int quantidadeFibonacci = 10;

    imprimirFibonacci(quantidadeFibonacci);
}
```

Este código calcula e imprime os primeiros 10 números da sequência de Fibonacci.

Explicação:

1. A primeira linha importa o módulo `std.stdio`, que é necessário para usar a função `writef` para imprimir os resultados.

2. Em seguida, temos a função `main`, que é o ponto de entrada do programa.

3. Dentro da função `main`, temos a definição da função `fibonacci`, que recebe um número `n` como argumento e retorna o `n`-ésimo número da sequência de Fibonacci. Esta função é implementada de forma recursiva. Se `n` for menor ou igual a 1, ela retorna `n`. Caso contrário, ela retorna a soma dos dois números anteriores da sequência.

4. Logo após, temos a definição da função `imprimirFibonacci`, que recebe um argumento `quantidade` e imprime os primeiros `quantidade` números da sequência de Fibonacci. Esta função utiliza um loop `for` para iterar de 0 até `quantidade - 1` e imprime o resultado de `fibonacci(i)` para cada valor de `i`.

5. Em seguida, temos a declaração da variável `quantidadeFibonacci`, que define o valor 10.

6. Por fim, chamamos a função `imprimirFibonacci` passando `quantidadeFibonacci` como argumento, o que resultará na impressão dos primeiros 10 números da sequência de Fibonacci.