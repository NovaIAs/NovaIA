Claro! Aqui está um código complexo em PERL que realiza uma análise de texto e conta a frequência de cada palavra em um arquivo de texto:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definindo o nome do arquivo de entrada
my $arquivo = 'texto.txt';

# Abrindo o arquivo para leitura
open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo' $!";

# Inicializando um hash vazio para armazenar as palavras e suas frequências
my %frequencia;

# Lendo o arquivo linha por linha
while (my $linha = <$fh>) {
    # Removendo caracteres especiais e convertendo para minúsculas
    $linha =~ s/[^\w\s]//g;
    $linha = lc($linha);

    # Dividindo a linha em palavras
    my @palavras = split(/\s+/, $linha);

    # Atualizando a frequência de cada palavra no hash
    foreach my $palavra (@palavras) {
        $frequencia{$palavra}++;
    }
}

# Fechando o arquivo
close($fh);

# Ordenando as palavras pelo valor de frequência em ordem decrescente
my @palavras_ordenadas = sort { $frequencia{$b} <=> $frequencia{$a} } keys %frequencia;

# Imprimindo as palavras e suas frequências
foreach my $palavra (@palavras_ordenadas) {
    print "$palavra: $frequencia{$palavra}\n";
}
```

Explicação do código:

1. Primeiro, definimos o nome do arquivo de entrada (no exemplo, "texto.txt").

2. Abrimos o arquivo em modo de leitura usando a função `open`.

3. Inicializamos um hash vazio chamado `%frequencia` para armazenar as palavras encontradas e suas frequências.

4. Em seguida, percorremos o arquivo linha por linha usando um loop `while`.

5. Em cada linha, removemos caracteres especiais usando a substituição `s/[^\w\s]//g`, que remove tudo que não é letra, número ou espaço em branco.

6. Convertemos a linha resultante para minúsculas usando a função `lc`.

7. Dividimos a linha em palavras usando a função `split`, que divide a string em um array usando um padrão (no caso, `\s+` representa um ou mais espaços em branco).

8. Iteramos sobre o array de palavras e atualizamos a frequência de cada palavra no hash `%frequencia`.

9. Após processar todas as linhas do arquivo, fechamos o arquivo usando a função `close`.

10. Ordenamos as palavras pelo valor de frequência em ordem decrescente usando a função `sort`.

11. Finalmente, percorremos as palavras ordenadas e imprimimos cada palavra e sua frequência na saída.

Espero que este código atenda às suas expectativas!