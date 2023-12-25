Claro! Aqui está um código complexo em Perl que realiza a análise de texto e gera estatísticas sobre as palavras encontradas:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Definindo o caminho para o arquivo de texto
my $arquivo = 'caminho/para/o/arquivo.txt';

# Lendo o arquivo
open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo': $!";

# Inicializando as variáveis de contagem
my %palavras;
my $total_palavras = 0;

# Processando cada linha do arquivo
while (my $linha = <$fh>) {
    # Removendo caracteres não alfanuméricos e convertendo para minúsculas
    $linha =~ s/[^a-zA-Z0-9\s]//g;
    $linha = lc $linha;

    # Separando a linha em palavras
    my @palavras_linha = split /\s+/, $linha;

    # Atualizando as contagens
    foreach my $palavra (@palavras_linha) {
        $palavras{$palavra}++;
        $total_palavras++;
    }
}

# Fechando o arquivo
close($fh);

# Imprimindo as estatísticas
print "Estatísticas das palavras encontradas no arquivo '$arquivo':\n";
print "--------------------------------------------\n";
print "Total de palavras: $total_palavras\n";
print "--------------------------------------------\n";
foreach my $palavra (sort keys %palavras) {
    my $frequencia = $palavras{$palavra};
    my $porcentagem = ($frequencia / $total_palavras) * 100;
    print "$palavra: $frequencia ocorrência(s) ($porcentagem%)\n";
}
print "--------------------------------------------\n";
```

Explicação do código:

1. Começamos definindo o caminho para o arquivo de texto que desejamos analisar, na variável `$arquivo`.

2. Abrimos o arquivo em modo de leitura usando a função `open`, armazenando o manipulador do arquivo na variável `$fh`. Se ocorrer algum erro ao abrir o arquivo, uma mensagem de erro será exibida.

3. Inicializamos duas variáveis: `%palavras`, que será um hash para armazenar as palavras encontradas e suas frequências, e `$total_palavras`, que armazena a contagem total de palavras encontradas.

4. Em seguida, começamos a processar cada linha do arquivo usando um loop `while`. A cada iteração, removemos todos os caracteres não alfanuméricos e convertemos a linha para minúsculas.

5. Utilizamos a função `split` para separar a linha em palavras, utilizando um ou mais espaços como delimitador. As palavras são armazenadas no array `@palavras_linha`.

6. Atualizamos as contagens de palavras no hash `%palavras` e incrementamos `$total_palavras` para cada palavra encontrada.

7. Após processar todas as linhas do arquivo, fechamos o arquivo usando a função `close`.

8. Imprimimos as estatísticas das palavras encontradas. Primeiro, exibimos o total de palavras encontradas, e em seguida, percorremos o hash `%palavras` para imprimir cada palavra, sua frequência e porcentagem de ocorrência em relação ao total de palavras.

9. Por fim, imprimimos uma linha de separação para melhor visualização.

Esse código é capaz de ler um arquivo de texto, processá-lo e fornecer estatísticas sobre as palavras encontradas. Espero que seja útil!