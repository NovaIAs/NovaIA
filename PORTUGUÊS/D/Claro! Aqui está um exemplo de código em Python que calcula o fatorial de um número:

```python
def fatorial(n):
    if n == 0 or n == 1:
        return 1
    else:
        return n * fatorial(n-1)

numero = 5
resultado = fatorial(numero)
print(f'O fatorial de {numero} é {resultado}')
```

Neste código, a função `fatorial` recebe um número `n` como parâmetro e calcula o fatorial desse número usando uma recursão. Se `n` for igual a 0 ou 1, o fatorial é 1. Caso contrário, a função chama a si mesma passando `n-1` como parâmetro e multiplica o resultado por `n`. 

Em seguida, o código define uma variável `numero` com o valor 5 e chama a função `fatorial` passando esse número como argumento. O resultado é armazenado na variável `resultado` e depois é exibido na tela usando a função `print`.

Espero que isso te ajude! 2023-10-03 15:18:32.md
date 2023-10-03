Claro! Aqui está um código complexo em D que envolve manipulação de arquivos e análise de dados:

```d
import std.stdio;
import std.algorithm;
import std.array;
import std.file;
import std.string;
import std.conv;

void main()
{
    // Lê o arquivo de entrada
    string[] linhas = readLines("entrada.txt");

    // Cria um dicionário para armazenar as palavras e suas frequências
    int[string] frequencias;

    // Percorre todas as linhas do arquivo
    foreach (linha; linhas)
    {
        // Remove pontuações e converte todas as letras para minúsculas
        linha = linha.stripChars(",.;:!?").toLower;

        // Divide a linha em palavras
        string[] palavras = linha.split(" ");

        // Atualiza as frequências das palavras no dicionário
        foreach (palavra; palavras)
        {
            if (palavra.length > 0)
            {
                frequencias[palavra]++;
            }
        }
    }

    // Ordena as palavras por frequência
    KeyValuePair[] palavrasOrdenadas = 
        sort(map!((k, v) => KeyValuePair(k, v))(frequencias));

    // Escreve as palavras e suas frequências no arquivo de saída
    foreach (palavraFreq; palavrasOrdenadas)
    {
        writefln("%s: %s", palavraFreq.key, palavraFreq.value);
    }
}
```

Este código lê um arquivo de texto chamado "entrada.txt" e conta a frequência de todas as palavras presentes no arquivo. Em seguida, ele ordena as palavras por frequência e escreve o resultado no arquivo de saída.

- Primeiro, o código lê todas as linhas do arquivo de entrada usando a função `readLines` da biblioteca `std.file`.
- Em seguida, ele cria um dicionário chamado `frequencias` para armazenar as palavras e suas frequências. O tipo do dicionário é `int[string]`, onde a chave é a palavra e o valor é a frequência.
- O código percorre cada linha do arquivo e realiza as seguintes ações:
  - Remove pontuações das linhas usando a função `stripChars` da biblioteca `std.string`.
  - Converte todas as letras para minúsculas usando a função `toLower` da biblioteca `std.string`.
  - Divide a linha em palavras usando a função `split` da biblioteca `std.string`.
  - Atualiza as frequências das palavras no dicionário incrementando o valor correspondente.
- Após percorrer todas as linhas, o código ordena as palavras por frequência usando a função `sort` da biblioteca `std.algorithm` e a função `map` para transformar o dicionário em um array de pares chave-valor.
- Por fim, o código escreve as palavras e suas frequências no arquivo de saída usando a função `writefln` da biblioteca `std.stdio`.

Espero que este exemplo atenda às suas expectativas!