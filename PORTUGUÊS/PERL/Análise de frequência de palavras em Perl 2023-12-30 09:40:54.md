Claro, vou criar um código em Perl complexo e diferenciado para você. O código a seguir é um exemplo de uma aplicação de processamento de texto que lê um arquivo de texto, conta a ocorrência de cada palavra e imprime as palavras mais frequentes.

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para ler um arquivo de texto
sub ler_arquivo {
    my ($arquivo) = @_;

    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo': $!";
    my $texto = join('', <$fh>);
    close($fh);

    return $texto;
}

# Função para contar a ocorrência de cada palavra
sub contar_palavras {
    my ($texto) = @_;

    my %contagem;
    while ($texto =~ /(\w+)/g) {
        my $palavra = lc $1; # Converter para minúsculas
        $contagem{$palavra}++;
    }

    return \%contagem;
}

# Função para imprimir as palavras mais frequentes
sub imprimir_palavras_frequentes {
    my ($contagem, $n) = @_;

    my $total_palavras = 0;
    foreach my $palavra (sort { $contagem->{$b} <=> $contagem->{$a} } keys %$contagem) {
        my $ocorrencias = $contagem->{$palavra};
        printf("%-10s %5d\n", $palavra, $ocorrencias);
        last if ++$total_palavras >= $n;
    }
}

# Programa principal
sub main {
    my $arquivo = 'texto.txt'; # Nome do arquivo de texto
    my $texto = ler_arquivo($arquivo);
    my $contagem = contar_palavras($texto);
    imprimir_palavras_frequentes($contagem, 10); # Imprimir as 10 palavras mais frequentes
}

main();
```

Neste código, temos três funções principais:

- `ler_arquivo`: lê um arquivo de texto e retorna seu conteúdo como uma string.
- `contar_palavras`: recebe um texto como entrada e conta a ocorrência de cada palavra, armazenando as informações em um hash.
- `imprimir_palavras_frequentes`: recebe a contagem de palavras e um número `n` como entrada, e imprime as `n` palavras mais frequentes, juntamente com o número de ocorrências.

No programa principal, o arquivo de texto é lido utilizando a função `ler_arquivo`, em seguida, o texto é processado pela função `contar_palavras`, que retorna um hash com a contagem de palavras. Por fim, a função `imprimir_palavras_frequentes` é chamada para imprimir as 10 palavras mais frequentes.

Espero que este código atenda às suas expectativas!