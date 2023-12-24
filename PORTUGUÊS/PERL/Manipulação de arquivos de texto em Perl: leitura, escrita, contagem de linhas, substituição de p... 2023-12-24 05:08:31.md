Claro! Aqui está um código complexo em Perl que manipula arquivos de texto e realiza várias operações:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para ler o conteúdo de um arquivo
sub ler_arquivo {
    my $arquivo = shift;
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo: $arquivo $!";
    
    my @linhas;
    while (my $linha = <$fh>) {
        chomp $linha;
        push @linhas, $linha;
    }
    
    close($fh);
    return @linhas;
}

# Função para escrever um conteúdo em um arquivo
sub escrever_arquivo {
    my ($arquivo, @conteudo) = @_;
    open(my $fh, '>', $arquivo) or die "Não foi possível abrir o arquivo: $arquivo $!";
    
    foreach my $linha (@conteudo) {
        print $fh "$linha\n";
    }
    
    close($fh);
}

# Função para contar o número de linhas em um arquivo
sub contar_linhas {
    my $arquivo = shift;
    my @linhas = ler_arquivo($arquivo);
    my $num_linhas = scalar @linhas;
    
    return $num_linhas;
}

# Função para substituir uma palavra por outra em um arquivo
sub substituir_palavra {
    my ($arquivo, $palavra_antiga, $palavra_nova) = @_;
    my @linhas = ler_arquivo($arquivo);
    
    foreach my $linha (@linhas) {
        $linha =~ s/\b$palavra_antiga\b/$palavra_nova/g;
    }
    
    escrever_arquivo($arquivo, @linhas);
}

# Função para inverter o conteúdo de um arquivo
sub inverter_conteudo {
    my $arquivo = shift;
    my @linhas = ler_arquivo($arquivo);
    
    @linhas = reverse @linhas;
    
    escrever_arquivo($arquivo, @linhas);
}

# Função principal
sub main {
    my $arquivo = "exemplo.txt";
    
    # Lê o conteúdo do arquivo
    my @conteudo = ler_arquivo($arquivo);
    
    # Imprime o número de linhas no arquivo
    my $num_linhas = contar_linhas($arquivo);
    print "O arquivo $arquivo possui $num_linhas linhas.\n";
    
    # Substitui uma palavra por outra no arquivo
    substituir_palavra($arquivo, "exemplo", "teste");
    
    # Inverte o conteúdo do arquivo
    inverter_conteudo($arquivo);
    
    # Lê o conteúdo do arquivo novamente
    @conteudo = ler_arquivo($arquivo);
    
    # Imprime o novo conteúdo
    print "Novo conteúdo:\n";
    foreach my $linha (@conteudo) {
        print "$linha\n";
    }
}

# Chama a função principal
main();
```

Neste código, temos uma série de funções para manipular arquivos de texto. A função `ler_arquivo` recebe o nome de um arquivo como parâmetro e retorna um array contendo as linhas do arquivo. A função `escrever_arquivo` recebe o nome de um arquivo e um array de conteúdo, e escreve o conteúdo no arquivo. A função `contar_linhas` recebe o nome de um arquivo e retorna o número de linhas no arquivo. A função `substituir_palavra` recebe o nome de um arquivo, uma palavra antiga e uma palavra nova, e substitui todas as ocorrências da palavra antiga pela palavra nova no arquivo. A função `inverter_conteudo` recebe o nome de um arquivo e inverte o conteúdo do arquivo. Por fim, a função `main` é a função principal que chama todas as outras funções para realizar as operações no arquivo "exemplo.txt".