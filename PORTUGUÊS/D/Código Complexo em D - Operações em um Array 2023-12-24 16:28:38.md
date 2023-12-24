Claro! Aqui está um código complexo em linguagem D, explicado detalhadamente:

```d
import std.stdio;

void main()
{
    writeln("Bem-vindo ao código complexo em D!");

    // Definindo e preenchendo um array de inteiros
    int[] array = [1, 2, 3, 4, 5];

    // Imprimindo o array
    writeln("Array original:");
    foreach (element; array)
    {
        writeln(element);
    }

    // Calculando a soma dos elementos do array
    int sum = 0;
    foreach (element; array)
    {
        sum += element;
    }

    // Imprimindo a soma
    writeln("Soma dos elementos do array: ", sum);

    // Encontrando o maior elemento do array
    int max = array[0];
    foreach (element; array[1..$])
    {
        if (element > max)
        {
            max = element;
        }
    }

    // Imprimindo o maior elemento
    writeln("Maior elemento do array: ", max);

    // Ordenando o array em ordem decrescente usando bubble sort
    for (int i = 0; i < array.length - 1; i++)
    {
        for (int j = 0; j < array.length - i - 1; j++)
        {
            if (array[j] < array[j + 1])
            {
                int temp = array[j];
                array[j] = array[j + 1];
                array[j + 1] = temp;
            }
        }
    }

    // Imprimindo o array ordenado
    writeln("Array ordenado em ordem decrescente:");
    foreach (element; array)
    {
        writeln(element);
    }

    writeln("Código complexo em D concluído!");
}
```

Este código complexo em D executa várias operações em um array de inteiros. Ele começa definindo e preenchendo um array com os números de 1 a 5. Em seguida, imprime o array original.

Depois, calcula a soma de todos os elementos do array e imprime o resultado. Em seguida, encontra o maior elemento do array e o imprime.

Em seguida, usa o algoritmo bubble sort para ordenar o array em ordem decrescente. Por fim, imprime o array ordenado.

O código é escrito em português e contém comentários explicativos para facilitar o entendimento. Ele utiliza estruturas de controle de fluxo, operações aritméticas e manipulação de arrays.