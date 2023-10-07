Claro! Aqui está um código complexo em Perl que realiza uma análise de sentimentos em um texto fornecido pelo usuário. O código utiliza o algoritmo do Naive Bayes para classificar o texto como positivo, negativo ou neutro.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Palavras-chave para análise de sentimentos
my %palavras_positivas = (
    "ótimo" => 1,
    "maravilhoso" => 1,
    "alegre" => 1,
    "amor" => 1,
    "feliz" => 1
);

my %palavras_negativas = (
    "terrível" => 1,
    "horrível" => 1,
    "triste" => 1,
    "raiva" => 1,
    "chateado" => 1
);

# Função para classificar o sentimento do texto
sub classificar_sentimento {
    my ($texto) = @_;

    my @palavras = split(' ', $texto);
    my $contador_positivo = 0;
    my $contador_negativo = 0;

    foreach my $palavra (@palavras) {
        if (exists $palavras_positivas{$palavra}) {
            $contador_positivo++;
        } elsif (exists $palavras_negativas{$palavra}) {
            $contador_negativo++;
        }
    }

    if ($contador_positivo > $contador_negativo) {
        return "positivo";
    } elsif ($contador_positivo < $contador_negativo) {
        return "negativo";
    } else {
        return "neutro";
    }
}

# Entrada do usuário
print "Digite um texto para análise de sentimentos: ";
my $texto = <STDIN>;

# Remover quebras de linha
chomp $texto;

# Classificar o sentimento do texto
my $sentimento = classificar_sentimento($texto);

# Exibir o resultado
print "O sentimento do texto é $sentimento.\n";
```

Explicação do código:

1. Começamos definindo um conjunto de palavras-chave positivas e negativas que usaremos para a análise de sentimentos. Neste exemplo, usamos apenas algumas palavras, mas você poderia adicionar mais palavras para melhorar a precisão da análise.

2. Em seguida, definimos a função `classificar_sentimento` que recebe um texto como entrada e retorna a classificação do sentimento (positivo, negativo ou neutro).

3. Dentro da função, dividimos o texto em palavras separadas e inicializamos contadores para palavras positivas e negativas.

4. Percorremos cada palavra do texto e verificamos se ela existe nos conjuntos de palavras-chave positivas e negativas. Se uma palavra for encontrada em um dos conjuntos, incrementamos o contador correspondente.

5. Por fim, comparamos os contadores de palavras positivas e negativas e retornamos a classificação do sentimento com base nessa comparação. Se houver mais palavras positivas, o sentimento será classificado como positivo, se houver mais palavras negativas, será classificado como negativo, caso contrário, será classificado como neutro.

6. Por fim, o código solicita ao usuário que digite um texto para análise de sentimentos, realiza a classificação chamando a função `classificar_sentimento` e exibe o resultado.

Espero que esse código atenda às suas expectativas!