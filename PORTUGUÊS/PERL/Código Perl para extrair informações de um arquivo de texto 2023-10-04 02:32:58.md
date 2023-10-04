Claro! Aqui está um exemplo de um código complexo em Perl que utiliza expressões regulares para analisar um arquivo de texto e extrair informações específicas:

```perl
#!/usr/bin/perl

use strict;
use warnings;

sub extrair_informacoes {
    my ($arquivo) = @_;
    
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo $arquivo: $!";
    
    my %informacoes;
    
    while (my $linha = <$fh>) {
        chomp $linha;
        
        if ($linha =~ /Nome:\s+(.*)/) {
            $informacoes{nome} = $1;
        }
        
        if ($linha =~ /Idade:\s+(\d+)/) {
            $informacoes{idade} = $1;
        }
        
        if ($linha =~ /Email:\s+(\S+@\S+)/) {
            $informacoes{email} = $1;
        }
        
        # Adicione mais padrões de busca conforme necessário...
    }
    
    close($fh);
    
    return \%informacoes;
}

# Exemplo de uso:
my $arquivo = 'informacoes.txt';
my $informacoes = extrair_informacoes($arquivo);

print "Nome: $informacoes->{nome}\n";
print "Idade: $informacoes->{idade}\n";
print "Email: $informacoes->{email}\n";
```

Neste código, definimos uma sub-rotina chamada `extrair_informacoes`, que recebe o nome de um arquivo como argumento. A função abre o arquivo, lê linha por linha e usa expressões regulares para extrair informações específicas.

As informações extraídas são armazenadas em um hash chamado `%informacoes`, onde a chave é o tipo de informação (nome, idade, email) e o valor é o valor extraído do arquivo.

No exemplo de uso, passamos o nome do arquivo 'informacoes.txt' para a função `extrair_informacoes` e armazenamos o resultado em uma variável `$informacoes`. Em seguida, imprimimos as informações extraídas na tela.

Este código pode ser adaptado para buscar por outros padrões de informações, como endereço, telefone, etc. Basta adicionar mais blocos `if` e expressões regulares correspondentes no loop `while`.