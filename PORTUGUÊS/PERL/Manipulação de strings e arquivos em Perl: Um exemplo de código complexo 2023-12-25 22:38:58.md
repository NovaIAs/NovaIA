Claro! Aqui está um exemplo de um código complexo em Perl, que realiza a manipulação de strings e arquivos de texto:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função que verifica se uma string é um palíndromo
sub eh_palindromo {
    my $string = shift;

    # Remove espaços em branco e converte para minúsculas
    $string =~ s/\s+//g;
    $string = lc $string;

    # Verifica se a string é um palíndromo
    return $string eq reverse $string;
}

# Função que conta o número de palavras em um arquivo
sub contar_palavras {
    my $arquivo = shift;
    my $contador = 0;

    # Abre o arquivo em modo de leitura
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo': $!";

    while (my $linha = <$fh>) {
        # Divide a linha em palavras
        my @palavras = split /\s+/, $linha;

        # Incrementa o contador com o número de palavras na linha
        $contador += scalar @palavras;
    }

    # Fecha o arquivo
    close($fh);

    return $contador;
}

# Função principal
sub main {
    print "Digite uma frase: ";
    my $frase = <STDIN>;
    chomp $frase;

    if (eh_palindromo($frase)) {
        print "A frase é um palíndromo!\n";
    } else {
        print "A frase não é um palíndromo!\n";
    }

    print "Digite o nome de um arquivo de texto: ";
    my $arquivo = <STDIN>;
    chomp $arquivo;

    my $quantidade_palavras = contar_palavras($arquivo);
    print "O arquivo contém $quantidade_palavras palavras.\n";
}

# Executa a função principal
main();

```

Este código em Perl é composto por três funções principais: `eh_palindromo`, `contar_palavras` e `main`.

A função `eh_palindromo` verifica se uma string é um palíndromo, removendo espaços em branco e convertendo todos os caracteres para minúsculas antes de fazer a comparação.

A função `contar_palavras` recebe o nome de um arquivo de texto como parâmetro e conta o número de palavras no arquivo, dividindo cada linha em palavras e incrementando um contador.

A função `main` é a função principal do programa, que solicita ao usuário uma frase, verifica se ela é um palíndromo usando a função `eh_palindromo`, e em seguida, solicita ao usuário o nome de um arquivo de texto e conta o número de palavras no arquivo usando a função `contar_palavras`.

Esse código é um exemplo de como você pode usar as funcionalidades de manipulação de strings e arquivos em Perl para executar tarefas específicas. Lembre-se de adaptar o código de acordo com suas necessidades e requisitos.