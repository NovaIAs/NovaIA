Certamente! Aqui está um código complexo em Perl que realiza uma análise de sentimento de textos em português utilizando a biblioteca Lingua::Sentiment.

```perl
use strict;
use warnings;
use Lingua::Sentiment;

# Cria um objeto para análise de sentimento
my $analisador = Lingua::Sentiment->new('pt');

# Texto de exemplo
my $texto = "Eu adorei o novo filme! A atuação dos atores foi incrível e a trama envolvente.";

# Realiza a análise de sentimento do texto
my $sentimento = $analisador->get_sentiment($texto);

# Imprime o resultado
print "Análise de Sentimento:\n";
print "Texto: $texto\n";
print "Sentimento: $sentimento\n";
print "\n";

# Outro texto de exemplo
$texto = "Que péssimo atendimento! Fiquei extremamente insatisfeito com o serviço prestado.";

# Realiza a análise de sentimento do texto
$sentimento = $analisador->get_sentiment($texto);

# Imprime o resultado
print "Análise de Sentimento:\n";
print "Texto: $texto\n";
print "Sentimento: $sentimento\n";
print "\n";
```

Neste código, utilizamos a biblioteca Lingua::Sentiment para realizar a análise de sentimento de textos em português. Começamos importando as bibliotecas necessárias e criando um objeto `$analisador` para a análise de sentimento.

Em seguida, definimos um texto de exemplo e utilizamos o método `get_sentiment` do objeto `$analisador` para obter o sentimento do texto. Este método retorna um valor numérico que varia de -1 a 1, representando sentimentos negativos, neutros ou positivos, respectivamente.

Imprimimos então o resultado da análise de sentimento para o primeiro texto de exemplo e repetimos o processo para um segundo texto de exemplo.

Esse código utiliza uma biblioteca externa para realizar a análise de sentimento, o que o torna mais complexo e diferenciado. Ele pode ser útil em aplicações que necessitem avaliar a opinião dos usuários com base em seus textos.