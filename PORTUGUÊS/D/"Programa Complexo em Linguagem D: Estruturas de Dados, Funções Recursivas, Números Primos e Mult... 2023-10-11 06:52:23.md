Claro! Aqui está um exemplo de um programa complexo escrito em linguagem D:

```d
import std.stdio;

void main()
{
    writeln("Bem-vindo ao programa complexo em D!");

    // Definindo uma estrutura de dados personalizada
    struct Pessoa
    {
        string nome;
        int idade;
    }

    // Criando um array de pessoas
    Pessoa[] pessoas;

    // Preenchendo o array com algumas pessoas
    pessoas ~= Pessoa("João", 25);
    pessoas ~= Pessoa("Maria", 30);
    pessoas ~= Pessoa("Pedro", 40);
    pessoas ~= Pessoa("Ana", 35);

    // Exibindo as informações das pessoas
    writeln("Informações das pessoas:");
    foreach (pessoa; pessoas)
    {
        writeln("Nome: ", pessoa.nome);
        writeln("Idade: ", pessoa.idade);
        writeln();
    }

    // Criando uma função recursiva para calcular o fatorial de um número
    ulong fatorial(ulong num)
    {
        if (num == 0)
            return 1;
        else
            return num * fatorial(num - 1);
    }

    // Calculando o fatorial de alguns números
    writeln("Fatorial de 5: ", fatorial(5));
    writeln("Fatorial de 7: ", fatorial(7));
    writeln("Fatorial de 10: ", fatorial(10));

    // Gerando números primos em um determinado intervalo
    int inicio = 1;
    int fim = 20;
    writeln("Números primos entre ", inicio, " e ", fim, ":");
    for (int i = inicio; i <= fim; i++)
    {
        bool ehPrimo = true;
        for (int j = 2; j < i; j++)
        {
            if (i % j == 0)
            {
                ehPrimo = false;
                break;
            }
        }
        if (ehPrimo)
            write(i, " ");
    }
    writeln();

    // Realizando uma operação de multiplicação de matrizes
    int[][] matrizA = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    int[][] matrizB = [[9, 8, 7], [6, 5, 4], [3, 2, 1]];
    int[][] matrizResultado;

    writeln("Multiplicação de matrizes:");
    for (int i = 0; i < matrizA.length; i++)
    {
        for (int j = 0; j < matrizB[0].length; j++)
        {
            int soma = 0;
            for (int k = 0; k < matrizA[0].length; k++)
            {
                soma += matrizA[i][k] * matrizB[k][j];
            }
            matrizResultado[i][j] = soma;
        }
    }

    for (int i = 0; i < matrizResultado.length; i++)
    {
        for (int j = 0; j < matrizResultado[0].length; j++)
        {
            write(matrizResultado[i][j], " ");
        }
        writeln();
    }
}
```

Este programa complexo em linguagem D possui várias funcionalidades. Aqui está uma explicação de cada uma delas:

1. Definindo uma estrutura de dados personalizada chamada `Pessoa` com os campos `nome` e `idade`.
2. Criando um array de `Pessoa` chamado `pessoas` e preenchendo-o com algumas instâncias de `Pessoa`.
3. Exibindo as informações das pessoas no array usando um loop `foreach`.
4. Criando uma função recursiva chamada `fatorial` para calcular o fatorial de um número.
5. Calculando o fatorial de alguns números usando a função `fatorial`.
6. Gerando e exibindo os números primos em um determinado intervalo usando dois loops `for`.
7. Realizando uma operação de multiplicação de matrizes e exibindo a matriz resultante.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição.