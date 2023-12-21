Claro! Abaixo está um código complexo em Perl que realiza uma tarefa específica: encontrar todos os anagramas de uma palavra fornecida pelo usuário.

```perl
use strict;
use warnings;

print "Digite uma palavra: ";
my $palavra = <STDIN>;
chomp $palavra;

my @letras = split //, $palavra;
my $tamanho = scalar @letras;

print "Anagramas da palavra $palavra:\n";
gerar_anagramas(\@letras, 0, $tamanho - 1);

sub gerar_anagramas {
    my ($letras, $inicio, $fim) = @_;

    if ($inicio == $fim) {
        print join("", @$letras) . "\n";
    } else {
        for (my $i = $inicio; $i <= $fim; $i++) {
            trocar($letras, $inicio, $i);
            gerar_anagramas($letras, $inicio + 1, $fim);
            trocar($letras, $inicio, $i); # desfaz a troca
        }
    }
}

sub trocar {
    my ($letras, $pos1, $pos2) = @_;
    my $temp = $letras->[$pos1];
    $letras->[$pos1] = $letras->[$pos2];
    $letras->[$pos2] = $temp;
}
```

Este código começa solicitando ao usuário que digite uma palavra. Em seguida, a palavra é armazenada na variável `$palavra`. 

A palavra é então dividida em um array de caracteres usando a função `split //, $palavra`, onde cada letra é armazenada em um elemento do array `@letras`. 

Em seguida, a função `gerar_anagramas` é chamada, passando o array `@letras`, o índice inicial (0) e o índice final (`$tamanho - 1`) como argumentos.

A função `gerar_anagramas` é uma função recursiva que utiliza o algoritmo de permutação para gerar todos os anagramas possíveis. Ela usa um loop `for` para iterar sobre as letras do array `@letras`, trocando a letra atual com a letra na posição inicial e chamando a função recursivamente para gerar os anagramas restantes. Após a chamada recursiva, a função desfaz a troca para garantir que todas as combinações sejam geradas corretamente.

Finalmente, o código imprime todos os anagramas encontrados na tela.

Espero que este código atenda às suas expectativas!