Claro! Aqui está um código complexo em Perl que envolve manipulação de arquivos, expressões regulares e tratamento de exceções. O objetivo deste código é ler um arquivo de texto, encontrar palavras repetidas e armazenar essas informações em um novo arquivo de saída.

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Definir o nome do arquivo de entrada e saída
my $arquivo_entrada = "entrada.txt";
my $arquivo_saida = "saida.txt";

# Abrir o arquivo de entrada para leitura
open(my $entrada, '<', $arquivo_entrada) or die "Não foi possível abrir o arquivo de entrada: $!";

# Criar um hash para armazenar as palavras repetidas
my %palavras_repetidas;

# Ler o arquivo de entrada linha por linha
while (my $linha = <$entrada>) {
    chomp $linha;
    
    # Extrair palavras da linha usando expressões regulares
    my @palavras = $linha =~ /[a-zA-Z]+/g;
    
    # Verificar se cada palavra já foi encontrada antes
    foreach my $palavra (@palavras) {
        # Converter a palavra para letras minúsculas para evitar diferenciação de maiúsculas e minúsculas
        my $palavra_min = lc $palavra;
        
        # Incrementar o contador no hash para a palavra atual
        $palavras_repetidas{$palavra_min}++;
    }
}

# Fechar o arquivo de entrada
close($entrada);

# Abrir o arquivo de saída para escrita
open(my $saida, '>', $arquivo_saida) or die "Não foi possível abrir o arquivo de saída: $!";

# Escrever as palavras repetidas no arquivo de saída
foreach my $palavra (sort keys %palavras_repetidas) {
    my $contador = $palavras_repetidas{$palavra};
    print $saida "$palavra: $contador\n";
}

# Fechar o arquivo de saída
close($saida);

print "Processo concluído. As palavras repetidas foram salvas em $arquivo_saida.\n";
```

Explicação:

1. Começamos definindo os nomes dos arquivos de entrada e saída. No exemplo, usamos "entrada.txt" como arquivo de entrada e "saida.txt" como arquivo de saída. Você pode alterar esses nomes conforme necessário.

```perl
my $arquivo_entrada = "entrada.txt";
my $arquivo_saida = "saida.txt";
```

2. Em seguida, abrimos o arquivo de entrada para leitura usando a função `open()`. Se ocorrer algum erro ao abrir o arquivo, o programa irá imprimir uma mensagem de erro e encerrar.

```perl
open(my $entrada, '<', $arquivo_entrada) or die "Não foi possível abrir o arquivo de entrada: $!";
```

3. Criamos um hash chamado `%palavras_repetidas` para armazenar as palavras repetidas. O hash será usado como um contador para cada palavra.

```perl
my %palavras_repetidas;
```

4. Em seguida, percorremos o arquivo de entrada linha por linha usando um loop `while`. A função `chomp()` é usada para remover a quebra de linha de cada linha lida.

```perl
while (my $linha = <$entrada>) {
    chomp $linha;
```

5. Dentro do loop, usamos uma expressão regular para encontrar todas as palavras em cada linha. As palavras são armazenadas em um array chamado `@palavras`.

```perl
my @palavras = $linha =~ /[a-zA-Z]+/g;
```

6. Em seguida, percorremos cada palavra e as inserimos no hash `%palavras_repetidas`, incrementando o contador para cada palavra encontrada. Convertemos as palavras para letras minúsculas usando `lc()` para evitar diferenciação de maiúsculas e minúsculas.

```perl
foreach my $palavra (@palavras) {
    my $palavra_min = lc $palavra;
    $palavras_repetidas{$palavra_min}++;
}
```

7. Após percorrer todo o arquivo de entrada, fechamos o arquivo usando a função `close()`.

```perl
close($entrada);
```

8. Em seguida, abrimos o arquivo de saída para escrita usando a função `open()`. Se ocorrer algum erro ao abrir o arquivo, o programa irá imprimir uma mensagem de erro e encerrar.

```perl
open(my $saida, '>', $arquivo_saida) or die "Não foi possível abrir o arquivo de saída: $!";
```

9. Agora, percorremos as palavras repetidas no hash `%palavras_repetidas` e as escrevemos no arquivo de saída. As palavras são ordenadas em ordem alfabética usando `sort`.

```perl
foreach my $palavra (sort keys %palavras_repetidas) {
    my $contador = $palavras_repetidas{$palavra};
    print $saida "$palavra: $contador\n";
}
```

10. Por fim, fechamos o arquivo de saída e imprimimos uma mensagem de conclusão.

```perl
close($saida);
print "Processo concluído. As palavras repetidas foram salvas em $arquivo_saida.\n";
```

Espero que este código atenda às suas expectativas!