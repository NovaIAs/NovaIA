Claro! Aqui está um código em Perl que realiza a análise de sentimentos em um texto utilizando a biblioteca Lingua::Sentiment. 

Primeiro, vamos instalar a biblioteca Lingua::Sentiment, caso já não esteja instalada. No terminal, digite:
```
cpan Lingua::Sentiment
```

Agora, vamos ao código:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Lingua::Sentiment;

# Função que realiza a análise de sentimentos em um texto
sub analisar_sentimento {
    my $texto = shift;

    # Criação do objeto Lingua::Sentiment
    my $analisador = Lingua::Sentiment->new;

    # Análise de sentimentos no texto
    my $sentimento = $analisador->get_sentiment($texto);

    # Imprime o resultado da análise
    print "Sentimento: $sentimento\n";
}

# Texto de exemplo
my $texto_exemplo = "Eu amo esse novo filme! É incrível e emocionante.";

# Chamada da função para analisar o sentimento no texto de exemplo
analisar_sentimento($texto_exemplo);
```

Aqui, começamos por importar os módulos `strict` e `warnings` para garantir um código mais seguro e eficiente.

Em seguida, importamos o módulo `Lingua::Sentiment`, que é responsável pela análise de sentimentos. Caso não esteja instalado, é necessário instalá-lo através do comando `cpan Lingua::Sentiment`.

Definimos a função `analisar_sentimento`, que recebe um texto como parâmetro. Dentro desta função, criamos um objeto `Lingua::Sentiment` através da chamada de `Lingua::Sentiment->new`. 

Em seguida, utilizamos o método `get_sentiment` para realizar a análise de sentimentos no texto fornecido. O resultado é armazenado na variável `$sentimento`.

Por fim, imprimimos o resultado da análise na tela.

No exemplo fornecido, o texto "Eu amo esse novo filme! É incrível e emocionante." será analisado e o resultado será impresso no formato "Sentimento: positivo".

Este é apenas um exemplo básico de como utilizar a biblioteca Lingua::Sentiment para realizar a análise de sentimentos em um texto. É possível adaptar e expandir esse código para atender a diferentes necessidades e casos de uso.