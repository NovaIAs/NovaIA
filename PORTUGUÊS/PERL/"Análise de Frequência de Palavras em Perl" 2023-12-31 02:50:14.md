Claro! Aqui está um exemplo complexo de código em Perl que realiza a análise de um arquivo de texto para contar a frequência das palavras e exibir o resultado em ordem decrescente:

```perl
#!/usr/bin/perl

use strict;
use warnings;

sub contar_palavras {
    my $arquivo = shift;
    
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo '$arquivo': $!";
    
    my %contagem;
    
    while (my $linha = <$fh>) {
        chomp $linha;
        
        my @palavras = split /\s+/, $linha;
        
        foreach my $palavra (@palavras) {
            $contagem{lc $palavra}++;
        }
    }
    
    close $fh;
    
    return %contagem;
}

sub exibir_resultado {
    my %contagem = @_;
    
    foreach my $palavra (sort { $contagem{$b} <=> $contagem{$a} } keys %contagem) {
        print "$palavra: $contagem{$palavra}\n";
    }
}

# Caminho para o arquivo de texto
my $arquivo = "caminho/para/o/arquivo.txt";

# Contar as palavras do arquivo
my %contagem = contar_palavras($arquivo);

# Exibir o resultado
exibir_resultado(%contagem);
```

Explicação do código:

1. No início, definimos a localização do interpretador Perl com a linha `#!/usr/bin/perl`.

2. Em seguida, importamos as pragmas `strict` e `warnings` para nos ajudar a escrever um código mais seguro e livre de erros.

3. A função `contar_palavras` é definida para realizar a contagem das palavras no arquivo de texto. Ela recebe o caminho para o arquivo como parâmetro. Primeiro, o arquivo é aberto para leitura. Em seguida, percorremos cada linha do arquivo, removemos a quebra de linha com `chomp` e dividimos a linha em palavras usando a expressão regular `\s+`. Para cada palavra encontrada, incrementamos sua contagem no hash `%contagem`. Por fim, o arquivo é fechado e o hash `%contagem` é retornado.

4. A função `exibir_resultado` recebe o hash `%contagem` como parâmetro e exibe as palavras e suas respectivas contagens em ordem decrescente. Primeiro, as chaves do hash são ordenadas usando a função `sort`, com um código de comparação que usa as contagens como critério. Em seguida, percorremos as palavras ordenadas e exibimos a palavra e sua contagem.

5. Na parte principal do código, definimos o caminho para o arquivo de texto que queremos analisar.

6. Chamamos a função `contar_palavras` passando o caminho do arquivo como argumento e armazenamos o resultado no hash `%contagem`.

7. Por fim, chamamos a função `exibir_resultado` passando o hash `%contagem` como argumento, para exibir o resultado na tela.

Espero que este exemplo atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais esclarecimentos, fique à vontade para perguntar.