Claro! Aqui está um exemplo de um código complexo em Perl que envolve a manipulação de arquivos, expressões regulares e estruturas de controle. O código abaixo foi projetado para analisar um arquivo de texto e contar a frequência de cada palavra no texto:

```perl
use strict;
use warnings;

# Abre o arquivo de entrada
open(my $arquivo, '<', 'texto.txt') or die "Não foi possível abrir o arquivo: $!";

# Declara um hash para armazenar as palavras e suas frequências
my %frequencia;

# Lê o arquivo linha por linha
while (my $linha = <$arquivo>) {
  # Remove os caracteres especiais e converte para minúsculas
  $linha =~ s/[^\p{L}\p{N}\s]//g;
  $linha = lc($linha);

  # Divide a linha em palavras
  my @palavras = split(/\s+/, $linha);

  # Incrementa a frequência de cada palavra no hash
  foreach my $palavra (@palavras) {
    $frequencia{$palavra}++;
  }
}

# Fecha o arquivo de entrada
close($arquivo);

# Ordena as palavras por frequência em ordem decrescente
my @palavras_ordenadas = sort { $frequencia{$b} <=> $frequencia{$a} } keys %frequencia;

# Imprime a frequência de cada palavra
foreach my $palavra (@palavras_ordenadas) {
  print "$palavra: $frequencia{$palavra}\n";
}
```

Neste código, começamos abrindo o arquivo de entrada "texto.txt" usando a função `open()`. Em seguida, declaramos um hash `%frequencia` que será usado para armazenar as palavras e suas contagens.

Em um loop `while`, lemos o arquivo linha por linha usando a variável `$linha`. Para cada linha, removemos os caracteres especiais usando uma expressão regular e convertemos todas as letras para minúsculas.

Em seguida, dividimos a linha em palavras usando a função `split()`. Iteramos sobre cada palavra e incrementamos sua contagem no hash `%frequencia`.

Depois de ler todo o arquivo, fechamos o arquivo de entrada usando a função `close()`.

Em seguida, ordenamos as palavras por frequência em ordem decrescente usando a função `sort()`. Por fim, imprimimos a frequência de cada palavra usando um loop `foreach`.

Espero que este código atenda às suas expectativas!