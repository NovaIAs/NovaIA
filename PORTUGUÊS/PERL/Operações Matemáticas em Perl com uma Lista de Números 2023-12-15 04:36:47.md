Claro! Aqui está um código em Perl que realiza várias operações matemáticas básicas em uma lista de números:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para calcular a soma dos elementos de uma lista
sub soma {
    my @numeros = @_;
    my $soma = 0;
    $soma += $_ for @numeros;
    return $soma;
}

# Função para calcular o produto dos elementos de uma lista
sub produto {
    my @numeros = @_;
    my $produto = 1;
    $produto *= $_ for @numeros;
    return $produto;
}

# Função para calcular a média dos elementos de uma lista
sub media {
    my @numeros = @_;
    my $soma = soma(@numeros);
    my $media = $soma / scalar @numeros;
    return $media;
}

# Função para calcular o fatorial de um número
sub fatorial {
    my $numero = shift;
    my $fatorial = 1;
    for my $i (1..$numero) {
        $fatorial *= $i;
    }
    return $fatorial;
}

# Lista de números
my @numeros = (1, 2, 3, 4, 5);

# Calcula a soma dos números
my $soma = soma(@numeros);
print "A soma dos números é: $soma\n";

# Calcula o produto dos números
my $produto = produto(@numeros);
print "O produto dos números é: $produto\n";

# Calcula a média dos números
my $media = media(@numeros);
print "A média dos números é: $media\n";

# Calcula o fatorial do último número da lista
my $ultimo_numero = $numeros[-1];
my $fatorial = fatorial($ultimo_numero);
print "O fatorial de $ultimo_numero é: $fatorial\n";
```

Este código em Perl define quatro funções: `soma`, `produto`, `media` e `fatorial`. A função `soma` recebe uma lista de números e retorna a soma deles. A função `produto` recebe uma lista de números e retorna o produto deles. A função `media` recebe uma lista de números e retorna a média deles. A função `fatorial` recebe um número e retorna o fatorial desse número.

Em seguida, o código define uma lista de números e faz chamadas às funções para calcular a soma, o produto, a média e o fatorial do último número da lista. Os resultados são impressos na tela.

Espero que este código atenda às suas expectativas e demonstre a versatilidade do Perl na manipulação de listas e operações matemáticas.