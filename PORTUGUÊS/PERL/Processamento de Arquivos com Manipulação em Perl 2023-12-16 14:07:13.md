Claro! Aqui está um exemplo de um código complexo em Perl que envolve manipulação de arquivos, expressões regulares e estruturas de controle:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Definindo o diretório de entrada e saída
my $diretorio_entrada = "./arquivos_entrada";
my $diretorio_saida = "./arquivos_saida";

# Verificando se o diretório de saída existe, caso contrário, criando-o
if (!-d $diretorio_saida) {
    mkdir $diretorio_saida or die "Não foi possível criar o diretório de saída: $!";
}

# Abrindo o diretório de entrada
opendir(my $dh, $diretorio_entrada) or die "Não foi possível abrir o diretório de entrada: $!";

# Percorrendo todos os arquivos do diretório de entrada
while (my $arquivo = readdir($dh)) {
    next if ($arquivo eq '.' or $arquivo eq '..'); # Ignorando os diretórios '.' e '..'
    
    # Abrindo o arquivo de entrada
    open(my $entrada, "<", "$diretorio_entrada/$arquivo") or die "Não foi possível abrir o arquivo de entrada: $!";

    # Criando o arquivo de saída com o mesmo nome, mas extensão .txt
    my $arquivo_saida = "$diretorio_saida/$arquivo.txt";
    open(my $saida, ">", $arquivo_saida) or die "Não foi possível criar o arquivo de saída: $!";

    # Lendo o conteúdo do arquivo de entrada linha por linha
    while (my $linha = <$entrada>) {
        chomp $linha; # Removendo a quebra de linha

        # Realizando algumas manipulações no conteúdo da linha
        $linha =~ s/(\d+)/$1 * 2/ge; # Multiplicando todos os números por 2
        $linha =~ s/(palavra)/uc($1)/ge; # Convertendo a palavra "palavra" para maiúsculas

        # Escrevendo a linha modificada no arquivo de saída
        print $saida "$linha\n";
    }

    # Fechando os arquivos
    close($entrada);
    close($saida);

    print "Arquivo $arquivo processado com sucesso!\n";
}

# Fechando o diretório de entrada
closedir($dh);

print "Todos os arquivos foram processados e salvos em '$diretorio_saida'.\n";
```

Este código em Perl faz um processamento em vários arquivos de um diretório de entrada. Ele realiza as seguintes ações:

1. Define os diretórios de entrada e saída.
2. Verifica se o diretório de saída existe e, caso não exista, cria-o.
3. Abre o diretório de entrada e itera sobre cada arquivo encontrado.
4. Abre o arquivo de entrada correspondente.
5. Cria o arquivo de saída com o mesmo nome, mas extensão `.txt`.
6. Lê o conteúdo do arquivo de entrada linha por linha.
7. Realiza duas manipulações no conteúdo da linha: multiplica todos os números por 2 e converte a palavra "palavra" para maiúsculas.
8. Escreve a linha modificada no arquivo de saída.
9. Fecha os arquivos.
10. Repete os passos 4 a 9 para cada arquivo encontrado no diretório de entrada.
11. Fecha o diretório de entrada.
12. Exibe uma mensagem informando que todos os arquivos foram processados e salvos no diretório de saída.

Espero que este código complexo atenda às suas necessidades!