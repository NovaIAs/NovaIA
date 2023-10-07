Claro! Aqui está um código complexo em D que utiliza manipulação de arquivos e algoritmos de ordenação para ler um arquivo de texto, contar a frequência de cada palavra e, em seguida, escrever as palavras e suas frequências em um novo arquivo de saída. 

```d
import std.algorithm;
import std.array;
import std.file;
import std.stdio;
import std.string;
import std.typecons;

void main()
{
    string inputFile = "texto.txt";
    string outputFile = "frequencia.txt";

    // Lê o arquivo de entrada
    auto fileContents = cast(string)read(inputFile);

    // Converte o conteúdo para minúsculo e remove todos os caracteres especiais
    fileContents = fileContents.toLower().filter!(c => isAlpha(c) || c == ' ');

    // Divide o conteúdo em palavras
    auto words = fileContents.split();

    // Cria um dicionário para armazenar a frequência de cada palavra
    auto frequency = new Dictionary!(string, int);

    // Conta a frequência de cada palavra
    foreach (word; words)
    {
        if (word in frequency)
            frequency[word]++;
        else
            frequency[word] = 1;
    }

    // Ordena as palavras pelo valor da frequência (decrescente) e pelo valor da palavra (crescente)
    auto sortedWords = frequency.byKeyValue().array.sort!((a, b) => a.value > b.value || (a.value == b.value && a.key < b.key));

    // Abre o arquivo de saída para escrita
    auto outFile = File(outputFile, FileMode.Out);

    // Escreve as palavras e suas frequências no arquivo de saída
    foreach (pair; sortedWords)
    {
        outFile.writeln(format("%s: %d", pair.key, pair.value));
    }

    // Fecha o arquivo de saída
    outFile.close();

    writeln("Frequência das palavras calculada e escrita no arquivo ", outputFile);
}
```

Neste código, primeiro definimos os nomes dos arquivos de entrada e saída. Em seguida, lemos o conteúdo do arquivo de entrada para a variável `fileContents`.

Para garantir uma análise mais precisa, convertemos todo o conteúdo para minúsculo e removemos caracteres especiais usando a função `toLower()` e o filtro `filter`.

Em seguida, dividimos o conteúdo em palavras usando o método `split()` e criamos um dicionário chamado `frequency` para armazenar a frequência de cada palavra.

Utilizando um loop `foreach`, percorremos todas as palavras e, se a palavra já estiver no dicionário, incrementamos sua frequência. Caso contrário, adicionamos a palavra ao dicionário com frequência 1.

Depois disso, ordenamos as palavras pelo valor da frequência e, em caso de empate, pelo valor da palavra em ordem alfabética. Isso é feito usando a função `sort()` em conjunto com uma função lambda como critério de ordenação.

Em seguida, criamos o arquivo de saída com o nome especificado e escrevemos as palavras e suas frequências nele usando um loop `foreach`. Por fim, fechamos o arquivo de saída e exibimos uma mensagem de conclusão.

Esse código é apenas um exemplo de como poderíamos implementar a contagem de frequência de palavras em um arquivo de texto. É possível personalizar e adaptar esse código de acordo com as necessidades específicas do projeto.