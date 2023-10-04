Claro! Aqui está um exemplo de código complexo em D, que envolve manipulação de dados, estruturas de controle e algoritmos avançados:

```D
import std.stdio;
import std.algorithm;
import std.range;
import std.string;
import std.conv;

void main()
{
    // Definir uma lista de números inteiros
    auto numeros = [5, 3, 1, 4, 2];

    // Ordenar a lista em ordem crescente usando o algoritmo Bubble Sort
    for (int i = 0; i < numeros.length - 1; i++)
    {
        for (int j = 0; j < numeros.length - i - 1; j++)
        {
            if (numeros[j] > numeros[j + 1])
            {
                swap(numeros[j], numeros[j + 1]);
            }
        }
    }

    // Converter a lista de números em uma string separada por vírgulas
    auto numerosString = numeros.map!(to!string).join(",");

    // Imprimir a string resultante na tela
    writeln("Lista ordenada: ", numerosString);

    // Calcular a média dos números na lista
    auto media = numeros.reduce!((a, b) => a + b) / numeros.length;

    // Imprimir a média na tela
    writeln("Média: ", media);

    // Verificar se algum número na lista é maior que a média
    bool algumMaiorQueMedia = numeros.any!(n => n > media);

    // Imprimir o resultado na tela
    if (algumMaiorQueMedia)
    {
        writeln("Algum número é maior que a média.");
    }
    else
    {
        writeln("Nenhum número é maior que a média.");
    }
}
```

Este código em D realiza as seguintes tarefas:

1. Define uma lista de números inteiros.
2. Ordena a lista em ordem crescente usando o algoritmo Bubble Sort.
3. Converte a lista de números em uma string separada por vírgulas.
4. Imprime a string resultante na tela.
5. Calcula a média dos números na lista.
6. Imprime a média na tela.
7. Verifica se algum número na lista é maior que a média.
8. Imprime o resultado na tela, indicando se algum número é maior que a média ou não.

Este código demonstra o uso de estruturas de controle, como loops (for), estruturas condicionais (if-else) e manipulação de dados em D.