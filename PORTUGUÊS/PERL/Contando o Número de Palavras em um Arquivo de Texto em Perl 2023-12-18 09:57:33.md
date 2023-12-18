Certamente! Aqui está um exemplo de um código complexo em PERL que realiza uma tarefa específica. Este código usa expressões regulares, manipulação de arquivos e estruturas de controle para processar um arquivo de texto, contando o número de ocorrências de palavras diferentes e armazenando os resultados em um hash.

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Declaração do arquivo de entrada
my $arquivo = 'texto.txt';

# Abertura do arquivo em modo de leitura
open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo $arquivo: $!";

# Declaração do hash para contar as palavras
my %palavras;

# Loop para ler cada linha do arquivo
while (my $linha = <$fh>) {
    # Remover caracteres especiais e converter todas as palavras para minúsculas
    $linha =~ s/[^\w\s]//g;
    $linha = lc($linha);

    # Dividir a linha em palavras individuais
    my @palavras_linha = split(/\s+/, $linha);

    # Iterar sobre cada palavra da linha
    foreach my $palavra (@palavras_linha) {
        # Verificar se a palavra já existe no hash
        if (exists $palavras{$palavra}) {
            # Se existir, incrementar o contador
            $palavras{$palavra}++;
        } else {
            # Caso contrário, adicionar a palavra ao hash com o contador inicializado como 1
            $palavras{$palavra} = 1;
        }
    }
}

# Fechar o arquivo
close($fh);

# Imprimir os resultados
foreach my $palavra (keys %palavras) {
    print "A palavra '$palavra' aparece $palavras{$palavra} vezes.\n";
}
```

Explicação do código:
1. Primeiro, é feita a declaração do arquivo de entrada como 'texto.txt'.
2. Em seguida, o arquivo é aberto em modo de leitura usando a função `open`, e é atribuído à variável `$fh`.
3. Um hash chamado `%palavras` é declarado para armazenar as contagens de palavras.
4. É feito um loop para ler cada linha do arquivo, utilizando a variável `$linha`.
5. A expressão regular `s/[^\w\s]//g` é usada para remover quaisquer caracteres especiais da linha.
6. A função `lc` é usada para converter todas as palavras da linha para minúsculas.
7. A linha é dividida em palavras individuais usando a função `split`, e as palavras são armazenadas no array `@palavras_linha`.
8. Em seguida, é feito um loop `foreach` para iterar sobre cada palavra da linha.
9. Dentro do loop, é verificado se a palavra já existe no hash `%palavras`, usando a função `exists`.
10. Se a palavra já existir, o contador correspondente é incrementado.
11. Caso contrário, a palavra é adicionada ao hash `%palavras` com o contador inicializado como 1.
12. Após o loop, o arquivo é fechado usando a função `close`.
13. Em seguida, é feito um loop `foreach` para imprimir os resultados, percorrendo as chaves do hash `%palavras`.
14. Cada palavra é impressa junto com a quantidade de vezes que ocorre.

Espero que esse código atenda às suas expectativas! Lembre-se de personalizar o nome do arquivo de entrada e adaptar o código conforme necessário.