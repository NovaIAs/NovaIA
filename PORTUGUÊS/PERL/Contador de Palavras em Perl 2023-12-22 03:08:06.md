Claro, vou gerar um código em Perl para você. Aqui está um exemplo de um código complexo em Perl que faz a leitura de um arquivo de texto, realiza a contagem de palavras e exibe o resultado na saída:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para contar as palavras em um texto
sub contar_palavras {
    my ($texto) = @_;
    
    my %palavras;
    my @palavras = split(/\W+/, $texto);
    
    foreach my $palavra (@palavras) {
        $palavras{lc($palavra)}++;
    }
    
    return %palavras;
}

# Função para exibir o resultado
sub exibir_resultado {
    my %palavras = @_;
    
    foreach my $palavra (sort keys %palavras) {
        printf "%-10s %d\n", $palavra, $palavras{$palavra};
    }
}

# Função principal
sub main {
    # Lê o nome do arquivo de texto
    print "Digite o nome do arquivo de texto: ";
    my $arquivo = <STDIN>;
    chomp($arquivo);
    
    # Abre o arquivo
    open(my $fh, "<", $arquivo) or die "Não foi possível abrir o arquivo '$arquivo' $!";
    
    # Lê o conteúdo do arquivo
    my $texto = do { local $/; <$fh> };
    
    # Fecha o arquivo
    close($fh);
    
    # Conta as palavras
    my %palavras = contar_palavras($texto);
    
    # Exibe o resultado
    exibir_resultado(%palavras);
}

# Chama a função principal
main();
```

Este código começa definindo duas funções: `contar_palavras` e `exibir_resultado`. A função `contar_palavras` recebe um texto como entrada e retorna um hash contendo a contagem de cada palavra no texto. A função `exibir_resultado` recebe o hash de palavras e exibe o resultado na saída.

Em seguida, temos a função principal `main`, que realiza as seguintes etapas:

1. Solicita ao usuário o nome do arquivo de texto a ser lido.
2. Abre o arquivo especificado pelo usuário.
3. Lê o conteúdo do arquivo.
4. Fecha o arquivo.
5. Chama a função `contar_palavras` passando o texto lido como argumento.
6. Chama a função `exibir_resultado` passando o hash de palavras como argumento.

O código é executado chamando a função principal `main`.

Este código em Perl é capaz de ler um arquivo de texto e contar quantas vezes cada palavra aparece no texto, exibindo o resultado na tela.