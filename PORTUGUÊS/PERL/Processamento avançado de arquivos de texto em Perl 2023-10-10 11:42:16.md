Claro! Aqui está um código em Perl complexo e diferenciado, que envolve o processamento de arquivos de texto e a aplicação de várias operações de manipulação de dados. 

```
#!/usr/bin/perl

use strict;
use warnings;

# Função para processar cada linha do arquivo de entrada
sub processar_linha {
    my ($linha) = @_;
    
    # Quebrar a linha em palavras
    my @palavras = split(' ', $linha);
    
    # Inverter a ordem das palavras
    @palavras = reverse @palavras;
    
    # Converter todas as palavras para maiúsculas
    @palavras = map { uc($_) } @palavras;
    
    # Concatenar as palavras em uma única string separada por vírgulas
    my $nova_linha = join(', ', @palavras);
    
    return $nova_linha;
}

# Ler o nome do arquivo de entrada fornecido pelo usuário
print "Digite o nome do arquivo de entrada: ";
my $nome_arquivo = <STDIN>;
chomp $nome_arquivo;

# Tentar abrir o arquivo de entrada
open my $arquivo, '<', $nome_arquivo or die "Não foi possível abrir o arquivo: $!";

# Ler todas as linhas do arquivo de entrada
my @linhas = <$arquivo>;

# Fechar o arquivo de entrada
close $arquivo;

# Processar cada linha do arquivo
my @novas_linhas = map { processar_linha($_) } @linhas;

# Ler o nome do arquivo de saída fornecido pelo usuário
print "Digite o nome do arquivo de saída: ";
my $nome_arquivo_saida = <STDIN>;
chomp $nome_arquivo_saida;

# Tentar abrir o arquivo de saída
open my $arquivo_saida, '>', $nome_arquivo_saida or die "Não foi possível abrir o arquivo de saída: $!";

# Escrever as novas linhas no arquivo de saída
print $arquivo_saida join("\n", @novas_linhas);

# Fechar o arquivo de saída
close $arquivo_saida;

print "Processamento concluído. O arquivo de saída '$nome_arquivo_saida' foi gerado com sucesso.\n";
```

Este código em Perl realiza as seguintes tarefas:

1. Solicita ao usuário o nome do arquivo de entrada.
2. Abre o arquivo de entrada e lê todas as linhas.
3. Processa cada linha do arquivo utilizando a função `processar_linha`.
4. Na função `processar_linha`, a linha é quebrada em palavras utilizando o espaço como delimitador.
5. A ordem das palavras é invertida.
6. Todas as palavras são convertidas para maiúsculas.
7. As palavras são concatenadas em uma única string separada por vírgulas.
8. As novas linhas processadas são armazenadas em um array `@novas_linhas`.
9. Solicita ao usuário o nome do arquivo de saída.
10. Abre o arquivo de saída e escreve as novas linhas.
11. Fecha o arquivo de saída.
12. Exibe uma mensagem de conclusão do processamento.

Este código é apenas um exemplo complexo de manipulação de arquivos em Perl, mas pode ser adaptado e personalizado conforme necessário.