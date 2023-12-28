Claro! Aqui está um código complexo em Perl com um algoritmo de criptografia de texto usando a cifra de César. A cifra de César é uma técnica de criptografia que substitui cada letra do texto original por uma letra deslocada um número fixo de posições à direita no alfabeto.

```
#!/usr/bin/perl
use strict;
use warnings;

sub criptografar {
    my ($texto, $deslocamento) = @_;
    my $criptografado = "";

    foreach my $letra (split //, $texto) {
        my $ascii = ord($letra);
        my $nova_letra = $letra;

        if ($ascii >= 65 && $ascii <= 90) {  # letras maiúsculas
            $nova_letra = chr((($ascii - 65 + $deslocamento) % 26) + 65);
        } elsif ($ascii >= 97 && $ascii <= 122) {  # letras minúsculas
            $nova_letra = chr((($ascii - 97 + $deslocamento) % 26) + 97);
        }

        $criptografado .= $nova_letra;
    }

    return $criptografado;
}

sub descriptografar {
    my ($texto, $deslocamento) = @_;
    my $descriptografado = "";

    foreach my $letra (split //, $texto) {
        my $ascii = ord($letra);
        my $nova_letra = $letra;

        if ($ascii >= 65 && $ascii <= 90) {  # letras maiúsculas
            $nova_letra = chr((($ascii - 65 - $deslocamento) % 26) + 65);
        } elsif ($ascii >= 97 && $ascii <= 122) {  # letras minúsculas
            $nova_letra = chr((($ascii - 97 - $deslocamento) % 26) + 97);
        }

        $descriptografado .= $nova_letra;
    }

    return $descriptografado;
}

# Exemplo de uso
my $texto_original = "Olá, Mundo!";
my $deslocamento = 5;

my $texto_criptografado = criptografar($texto_original, $deslocamento);
print "Texto criptografado: $texto_criptografado\n";

my $texto_descriptografado = descriptografar($texto_criptografado, $deslocamento);
print "Texto descriptografado: $texto_descriptografado\n";
```

Neste código, temos duas sub-rotinas: `criptografar` e `descriptografar`. A sub-rotina `criptografar` recebe um texto e um valor de deslocamento como parâmetros e retorna o texto criptografado usando a cifra de César. A sub-rotina `descriptografar` recebe um texto criptografado e o valor de deslocamento e retorna o texto original descriptografado.

O código principal mostra um exemplo de uso, onde a variável `$texto_original` contém o texto a ser criptografado, e a variável `$deslocamento` define o valor de deslocamento para a cifra de César. O texto criptografado e descriptografado são exibidos no final.

Este código utiliza as funções `ord` e `chr` para converter caracteres em seus respectivos valores ASCII e vice-versa. Ele também usa as estruturas de controle `if` e `foreach` para percorrer individualmente cada letra do texto e aplicar o deslocamento adequado.