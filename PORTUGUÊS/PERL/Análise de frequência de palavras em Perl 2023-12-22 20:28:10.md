Claro! Aqui está um exemplo de código complexo em Perl, que realiza uma análise de texto para identificar a frequência de palavras em um arquivo:

```perl
use strict;
use warnings;

# Define o nome do arquivo de entrada
my $arquivo = "texto.txt";

# Cria um hash para armazenar a frequência de cada palavra
my %frequencia;

# Abre o arquivo para leitura
open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo $arquivo: $!";

# Lê o arquivo linha por linha
while (my $linha = <$fh>) {
    chomp $linha;

    # Remove caracteres especiais e converte todas as letras para minúsculas
    $linha =~ s/[^\w\s]//g;
    $linha = lc $linha;

    # Divide a linha em palavras
    my @palavras = split /\s+/, $linha;

    # Incrementa a frequência de cada palavra no hash
    foreach my $palavra (@palavras) {
        $frequencia{$palavra}++;
    }
}

# Fecha o arquivo
close($fh);

# Ordena as palavras por frequência decrescente
my @palavras_ordenadas = sort { $frequencia{$b} <=> $frequencia{$a} } keys %frequencia;

# Imprime as palavras e suas frequências
foreach my $palavra (@palavras_ordenadas) {
    print "$palavra: $frequencia{$palavra}\n";
}
```

Neste código, utilizamos as bibliotecas `strict` e `warnings` para garantir um código seguro e evitar erros comuns. Em seguida, definimos o nome do arquivo de entrada que será usado para a análise (no exemplo, `texto.txt`).

Abrimos o arquivo e, em seguida, lemos linha por linha usando um laço `while`. Cada linha é processada para remover caracteres especiais e converter todas as letras para minúsculas. Em seguida, dividimos a linha em palavras usando o espaço como delimitador.

Utilizamos um hash `%frequencia` para armazenar a frequência de cada palavra. Percorremos todas as palavras e incrementamos sua frequência no hash.

Após ler todo o arquivo, fechamos o arquivo e ordenamos as palavras por frequência decrescente, usando a função `sort`. Percorremos as palavras ordenadas e imprimimos cada palavra junto com sua frequência.

Espero que este código complexo em Perl atenda às suas expectativas!