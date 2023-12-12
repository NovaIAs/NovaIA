Claro! Aqui está um exemplo de código em Perl que lida com manipulação de arquivos, expressões regulares e funções personalizadas:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Função que verifica se uma palavra é um palíndromo
sub verifica_palindromo {
    my $palavra = shift;
    my $reversa = reverse $palavra;
    
    if ($palavra eq $reversa) {
        return 1;
    } else {
        return 0;
    }
}

# Ler o nome do arquivo de entrada
print "Digite o nome do arquivo de entrada: ";
my $arquivo_entrada = <STDIN>;
chomp $arquivo_entrada;

# Abrir o arquivo de entrada
open(my $entrada, '<', $arquivo_entrada) or die "Erro ao abrir o arquivo de entrada: $!";

# Ler o conteúdo do arquivo de entrada
my @linhas = <$entrada>;

# Fechar o arquivo de entrada
close($entrada);

# Criar um novo arquivo de saída
my $arquivo_saida = "saida.txt";
open(my $saida, '>', $arquivo_saida) or die "Erro ao criar o arquivo de saída: $!";

# Percorrer cada linha do arquivo de entrada
foreach my $linha (@linhas) {
    chomp $linha;
    
    # Verificar se a linha contém um palíndromo
    if (verifica_palindromo($linha)) {
        # Escrever a linha no arquivo de saída
        print $saida "$linha\n";
    }
}

# Fechar o arquivo de saída
close($saida);

print "Processamento concluído! Os palíndromos encontrados foram salvos no arquivo $arquivo_saida.\n";
```

Este código permite que você digite o nome de um arquivo de entrada, lê o conteúdo desse arquivo, verifica se cada linha é um palíndromo usando a função `verifica_palindromo`, e em seguida, salva todas as linhas que são palíndromos em um novo arquivo de saída chamado "saida.txt".

A função `verifica_palindromo` recebe uma palavra como parâmetro e retorna `1` se a palavra for um palíndromo (ou seja, se ela for a mesma lida de trás para frente), caso contrário, retorna `0`.

Certifique-se de que o Perl esteja instalado em seu sistema para executar o código acima.