Certamente! Aqui está um código em Perl que utiliza algumas funcionalidades avançadas da linguagem para manipulação de strings, matrizes e funções recursivas. O código consiste em um programa que encontra todas as combinações possíveis de uma lista de palavras, gerando todas as permutações e imprimindo-as na tela.

```perl
#!/usr/bin/perl
use strict;
use warnings;

sub encontrar_combinacoes {
    my ($palavras, $atual, $resultado) = @_;
    
    if (@$palavras == 0) {
        push(@$resultado, $atual);
        return;
    }
    
    for my $i (0 .. @$palavras - 1) {
        my @resto = @$palavras;
        my $palavra = splice(@resto, $i, 1);
        
        encontrar_combinacoes(\@resto, $atual . " " . $palavra, $resultado);
    }
}

sub imprimir_combinacoes {
    my ($resultado) = @_;
    
    for my $i (0 .. @$resultado - 1) {
        my $comb = $resultado->[$i];
        $comb =~ s/^\s+|\s+$//g;  # Remove espaços extras no início e no final
        
        print "Combinação ", $i + 1, ": $comb\n";
    }
    
    print "\nTotal de combinações encontradas: ", scalar @$resultado, "\n";
}

# Lista de palavras para combinar
my @palavras = ("maçã", "banana", "laranja", "abacaxi");

# Array para armazenar as combinações encontradas
my @resultado;

# Encontra todas as combinações possíveis
encontrar_combinacoes(\@palavras, "", \@resultado);

# Imprime as combinações encontradas
imprimir_combinacoes(\@resultado);
```

Este código começa com a definição de duas funções: `encontrar_combinacoes` e `imprimir_combinacoes`. A primeira função é uma função recursiva que recebe uma lista de palavras, uma string atual (representando a combinação atual) e uma referência para um array de resultados. Ela utiliza um loop `for` para iterar através das palavras na lista, removendo uma palavra por vez e chamando recursivamente a função com o restante das palavras e a palavra removida adicionada à string atual. Quando a lista de palavras estiver vazia, ou seja, todas as palavras foram utilizadas, a combinação atual é adicionada ao array de resultados.

A segunda função, `imprimir_combinacoes`, recebe o array de resultados e imprime cada combinação na tela, removendo espaços extras no início e no final de cada combinação. Além disso, ela também imprime o total de combinações encontradas.

No programa principal, uma lista de palavras é definida e um array vazio é criado para armazenar as combinações encontradas. A função `encontrar_combinacoes` é chamada para encontrar todas as combinações possíveis, passando a lista de palavras, uma string vazia como combinação atual e uma referência para o array de resultados. Em seguida, a função `imprimir_combinacoes` é chamada para imprimir as combinações encontradas, passando o array de resultados.

Espero que este código complexo e diferenciado atenda às suas expectativas!