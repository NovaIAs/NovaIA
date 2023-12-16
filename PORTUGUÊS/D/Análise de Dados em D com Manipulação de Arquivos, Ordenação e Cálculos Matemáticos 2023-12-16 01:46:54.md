Claro! Aqui está um código complexo em D que envolve manipulação de arquivos, ordenação de dados e cálculos matemáticos:

```
import std.stdio;
import std.file;
import std.algorithm;

void main()
{
    writeln("Bem-vindo ao programa de análise de dados!");

    // Lê o arquivo de entrada
    auto inputFile = "dados.txt";
    auto fileContents = cast(string)read(inputFile);

    // Converte o conteúdo do arquivo em um array de inteiros
    int[] data = fileContents.split().map!(a => a.to!int).array;

    // Ordena os dados em ordem crescente
    data.sort();

    // Calcula a média dos valores
    double average = data.reduce!((a, b) => a + b) / data.length;

    writeln("Os dados foram ordenados e a média dos valores é: ", average);

    // Calcula a mediana dos valores
    double median;
    if (data.length % 2 == 0)
    {
        median = (data[data.length / 2 - 1] + data[data.length / 2]) / 2.0;
    }
    else
    {
        median = data[data.length / 2];
    }
    writeln("A mediana dos valores é: ", median);

    // Calcula o desvio padrão dos valores
    double sumSquaredDifferences = 0;
    foreach (value; data)
    {
        sumSquaredDifferences += (value - average) ^^ 2;
    }
    double standardDeviation = sqrt(sumSquaredDifferences / data.length);
    writeln("O desvio padrão dos valores é: ", standardDeviation);

    // Escreve os resultados em um arquivo de saída
    auto outputFile = "resultados.txt";
    auto outputContents = "Média: " ~ average.to!string ~ "\n" ~
                          "Mediana: " ~ median.to!string ~ "\n" ~
                          "Desvio Padrão: " ~ standardDeviation.to!string;
    write(outputFile, outputContents);

    writeln("Os resultados foram salvos no arquivo resultados.txt. O programa foi concluído.");
}
```

Este código realiza uma análise de dados a partir de um arquivo de entrada chamado "dados.txt". Ele lê o conteúdo do arquivo, converte os dados em um array de inteiros, ordena os valores em ordem crescente e calcula a média, mediana e desvio padrão dos valores. Em seguida, os resultados são escritos em um arquivo de saída chamado "resultados.txt".

Espero que este código complexo atenda às suas expectativas!