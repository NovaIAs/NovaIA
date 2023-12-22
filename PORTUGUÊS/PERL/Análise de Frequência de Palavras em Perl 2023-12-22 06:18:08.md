Claro! Aqui está um código em Perl que realiza uma tarefa complexa e tem um tamanho considerável:

```perl
#!/usr/bin/perl

# Este programa realiza uma análise de texto para contar a frequência de palavras em um arquivo.

use strict;
use warnings;

# Define o arquivo de entrada
my $arquivo = "texto.txt";

# Abre o arquivo para leitura
open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo': $!";

# Cria um hash para armazenar a contagem das palavras
my %contagem_palavras;

# Lê cada linha do arquivo
while (my $linha = <$fh>) {
    # Remove pontuação e converte todas as letras para minúsculas
    $linha =~ s/[[:punct:]]//g;
    $linha = lc $linha;
    
    # Divide a linha em palavras
    my @palavras = split /\s+/, $linha;
    
    # Incrementa a contagem de cada palavra encontrada
    foreach my $palavra (@palavras) {
        $contagem_palavras{$palavra}++;
    }
}

# Fecha o arquivo
close($fh);

# Imprime a contagem das palavras em ordem decrescente
foreach my $palavra (sort { $contagem_palavras{$b} <=> $contagem_palavras{$a} } keys %contagem_palavras) {
    print "$palavra: $contagem_palavras{$palavra}\n";
}
```

Explicação do código:

1. Começamos definindo o caminho para o arquivo de entrada, que é chamado "texto.txt".
2. Abre o arquivo para leitura usando a função `open`.
3. Cria um hash chamado `%contagem_palavras` para armazenar a contagem das palavras.
4. Em um loop, lemos cada linha do arquivo usando a variável `$linha`.
5. Removemos a pontuação e convertemos todas as letras para minúsculas usando expressões regulares.
6. Dividimos a linha em palavras, usando o caractere espaço como separador, e armazenamos em um array chamado `@palavras`.
7. Percorremos cada palavra e incrementamos sua contagem no hash `%contagem_palavras`.
8. Fechamos o arquivo.
9. Por fim, imprimimos a contagem das palavras em ordem decrescente de ocorrência, usando o hash `%contagem_palavras`.

Esse código em Perl é capaz de analisar um arquivo de texto, contar a frequência de cada palavra e exibir a contagem em ordem decrescente. É um exemplo de um programa mais complexo que utiliza recursos da linguagem Perl para realizar uma tarefa específica.