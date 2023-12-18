Claro! Aqui está um exemplo de código complexo em D:

```D
import std.stdio;
import std.algorithm;
import std.container;
import std.range;
import std.string;
import std.random;
import std.datetime;

void main()
{
    auto listaDeNumeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto listaEmbaralhada = listaDeNumeros.dup.shuffle;

    writeln("Lista original: ", listaDeNumeros);
    writeln("Lista embaralhada: ", listaEmbaralhada);

    auto listaOrdenada = listaEmbaralhada.dup.sort;

    writeln("Lista ordenada: ", listaOrdenada);

    auto soma = listaOrdenada.reduce!"a + b"(0);

    writeln("A soma dos elementos da lista é: ", soma);

    auto dataAtual = Clock.currTime;

    writeln("Data atual: ", dataAtual);

    auto dataFormatada = dataAtual.format("%Y-%m-%d %H:%M:%S");

    writeln("Data formatada: ", dataFormatada);

    auto texto = "Olá, Mundo!";

    auto textoInvertido = texto.dup.reverse;

    writeln("Texto original: ", texto);
    writeln("Texto invertido: ", textoInvertido);
}
```

Este código em D demonstra o uso de várias funcionalidades da linguagem. Primeiro, criamos uma lista de números de 1 a 10. Em seguida, embaralhamos a lista usando a função `shuffle`. Depois, exibimos a lista original e a lista embaralhada.

Em seguida, duplicamos a lista embaralhada e a ordenamos usando a função `sort`. Em seguida, calculamos a soma de todos os elementos da lista usando a função `reduce`.

Depois disso, obtemos a data e hora atuais usando `Clock.currTime` e a formatamos no formato `"%Y-%m-%d %H:%M:%S"` usando a função `format`.

Por fim, definimos uma variável de texto contendo a frase "Olá, Mundo!" e a invertemos usando a função `reverse`. Exibimos o texto original e o texto invertido.

Este código demonstra várias funcionalidades do D, como manipulação de listas, ordenação, redução, formatação de datas e manipulação de strings. Ele pode ser usado como base para desenvolver programas mais complexos em D.