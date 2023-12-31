Certamente! Aqui está um exemplo de um código complexo em Perl que utiliza estruturas de controle, manipulação de arquivos e expressões regulares:

```
#!/usr/bin/perl

# Função para ler um arquivo e retornar um array com as linhas
sub ler_arquivo {
    my ($arquivo) = @_;
    open(my $fh, '<', $arquivo) or die "Não foi possível abrir o arquivo: $!";
    my @linhas = <$fh>;
    close($fh);
    return @linhas;
}

# Função para escrever um array de linhas em um arquivo
sub escrever_arquivo {
    my ($arquivo, @linhas) = @_;
    open(my $fh, '>', $arquivo) or die "Não foi possível abrir o arquivo: $!";
    foreach my $linha (@linhas) {
        print $fh $linha;
    }
    close($fh);
}

# Função para substituir todas as ocorrências de uma palavra por outra em um array de linhas
sub substituir_palavra {
    my ($palavra_antiga, $palavra_nova, @linhas) = @_;
    foreach my $linha (@linhas) {
        $linha =~ s/\b$palavra_antiga\b/$palavra_nova/g;
    }
    return @linhas;
}

# Função principal
sub main {
    my $arquivo_entrada = "entrada.txt";
    my $arquivo_saida = "saida.txt";
    my @linhas = ler_arquivo($arquivo_entrada);

    # Realiza algum processamento no array de linhas
    @linhas = substituir_palavra("gato", "cachorro", @linhas);
    @linhas = substituir_palavra("azul", "verde", @linhas);

    # Escreve o resultado em um arquivo de saída
    escrever_arquivo($arquivo_saida, @linhas);

    print "Processamento concluído. O resultado foi salvo em $arquivo_saida.\n";
}

# Chamada da função principal
main();
```

Neste código, temos três funções principais: `ler_arquivo`, `escrever_arquivo` e `substituir_palavra`. A função `ler_arquivo` recebe o nome de um arquivo como parâmetro e retorna um array com as linhas do arquivo. A função `escrever_arquivo` recebe o nome de um arquivo e um array de linhas como parâmetros e escreve as linhas no arquivo. A função `substituir_palavra` recebe uma palavra antiga, uma palavra nova e um array de linhas, e substitui todas as ocorrências da palavra antiga pela palavra nova em cada linha.

Na função `main`, definimos os nomes dos arquivos de entrada e saída, chamamos a função `ler_arquivo` para obter as linhas do arquivo de entrada e, em seguida, realizamos algum processamento no array de linhas. Neste exemplo, utilizamos a função `substituir_palavra` duas vezes, substituindo "gato" por "cachorro" e "azul" por "verde". Por fim, chamamos a função `escrever_arquivo` para gravar o resultado no arquivo de saída e exibimos uma mensagem informando que o processamento foi concluído.

Espero que este código atenda às suas necessidades! Se tiver alguma dúvida ou precisar de mais alguma coisa, fique à vontade para perguntar.