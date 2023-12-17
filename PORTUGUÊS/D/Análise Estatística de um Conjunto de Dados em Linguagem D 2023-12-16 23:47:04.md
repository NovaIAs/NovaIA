Claro! Aqui está um código complexo em linguagem D, que faz uma análise estatística de um conjunto de dados:

```d
import std.stdio;
import std.algorithm;
import std.range;
import std.conv;
import std.array;

void main()
{
    // Dados de exemplo
    int[] dados = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Calcular a média
    float media = dados.sum / dados.length;

    // Calcular o desvio padrão
    float somaQuadrados = dados.map!(x => (x - media) ^^ 2).sum;
    float desvioPadrao = sqrt(somaQuadrados / dados.length);

    // Calcular o valor máximo
    int maximo = dados.maxElement;

    // Calcular o valor mínimo
    int minimo = dados.minElement;

    // Calcular a mediana
    int mediana = dados.dup.sort().length % 2 == 0
        ? dados.dup.sort()[dados.dup.sort().length / 2]
        : dados.dup.sort()[dados.dup.sort().length / 2];

    // Calcular a moda
    int moda = dados.histogram().maxElement;

    // Imprimir os resultados
    writeln("Média: ", media.to!string);
    writeln("Desvio Padrão: ", desvioPadrao.to!string);
    writeln("Máximo: ", maximo.to!string);
    writeln("Mínimo: ", minimo.to!string);
    writeln("Mediana: ", mediana.to!string);
    writeln("Moda: ", moda.to!string);
}
```

Este código em D realiza uma análise estatística básica de um conjunto de dados numéricos. Primeiro, criamos um array chamado "dados" com alguns valores de exemplo. Em seguida, calculamos a média, o desvio padrão, o valor máximo, o valor mínimo, a mediana e a moda dos dados.

Para calcular a média, somamos todos os valores e dividimos pelo número total de elementos. O desvio padrão é calculado subtraindo-se a média de cada valor e elevando ao quadrado, somando todos esses quadrados e dividindo pelo número total de elementos, e então, tirando a raiz quadrada. O valor máximo e mínimo são obtidos através das funções `maxElement` e `minElement` do módulo `std.algorithm`. 

A mediana é calculada ordenando os dados em ordem crescente e então, verificando se o número de elementos é par ou ímpar. Se for par, a mediana é o valor no meio do array ordenado. Se for ímpar, a mediana é a média dos dois valores centrais.

Por fim, a moda é calculada utilizando a função `histogram()` do módulo `std.algorithm`, que retorna um array de contagens de ocorrências para cada valor único presente no conjunto de dados. A moda é o valor com a maior contagem.

Os resultados são impressos utilizando a função `writeln()` da biblioteca padrão `std.stdio`.

Espero que este código atenda às suas expectativas! Se você tiver alguma outra solicitação ou dúvida, estou à disposição para ajudar.