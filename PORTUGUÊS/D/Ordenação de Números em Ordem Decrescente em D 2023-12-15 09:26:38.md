Claro! Aqui está um exemplo de código complexo em D, escrito em português:

```d
import std.stdio;

void main()
{
    // Inicialização do array com números de 1 a 100
    int[] numeros = new int[100];
    for (int i = 0; i < 100; i++)
    {
        numeros[i] = i + 1;
    }

    // Ordenação do array em ordem decrescente usando bubble sort
    for (int i = 0; i < numeros.length; i++)
    {
        for (int j = 0; j < numeros.length - i - 1; j++)
        {
            if (numeros[j] < numeros[j + 1])
            {
                int temp = numeros[j];
                numeros[j] = numeros[j + 1];
                numeros[j + 1] = temp;
            }
        }
    }

    // Impressão dos números ordenados
    writeln("Números ordenados em ordem decrescente:");
    foreach (num in numeros)
    {
        writeln(num);
    }
}
```

Nesse código, começamos inicializando um array chamado "numeros" com números de 1 a 100. Em seguida, utilizamos o algoritmo bubble sort para ordenar o array em ordem decrescente. O bubble sort é um algoritmo de ordenação simples, mas que requer várias iterações sobre o array para garantir que todos os elementos estejam na posição correta.

No loop externo, iteramos sobre o array numeros, enquanto no loop interno, comparamos cada elemento com o próximo. Se o elemento atual for menor que o próximo, realizamos uma troca. Ao final desse processo, teremos o array ordenado em ordem decrescente.

Por fim, imprimimos na tela os números ordenados em ordem decrescente. Utilizamos o comando writeln para imprimir cada número em uma nova linha.

Esse é apenas um exemplo de um código complexo em D, mas existem muitas outras possibilidades de implementação. Se tiver alguma necessidade específica ou algum objetivo específico em mente, ficarei feliz em ajudar a criar um código que atenda às suas necessidades.