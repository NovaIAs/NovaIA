Claro! Aqui está um código complexo em Perl, que realiza uma análise de dados em um arquivo CSV. O código é dividido em várias etapas, cada uma com uma função específica. Vou explicar cada trecho do código para facilitar o entendimento:

```perl
#!/usr/bin/perl
use strict;
use warnings;
use Text::CSV;

# Função para ler o arquivo CSV e armazenar os dados em uma matriz
sub ler_arquivo_csv {
    my ($arquivo) = @_;

    my @dados;
    my $csv = Text::CSV->new({ binary => 1 }) or die "Não foi possível abrir o arquivo: $!";
    
    open(my $fh, "<:encoding(utf8)", $arquivo) or die "Não foi possível abrir o arquivo: $!";
    
    while (my $linha = $csv->getline($fh)) {
        push @dados, $linha;
    }
    
    close($fh);

    return \@dados;
}

# Função para filtrar os dados com base em critérios específicos
sub filtrar_dados {
    my ($dados) = @_;

    my @dados_filtrados;

    foreach my $linha (@$dados) {
        # Critério de filtro: valor da segunda coluna maior que 10
        if ($linha->[1] > 10) {
            push @dados_filtrados, $linha;
        }
    }

    return \@dados_filtrados;
}

# Função para calcular a média dos valores da terceira coluna
sub calcular_media {
    my ($dados) = @_;

    my $soma = 0;
    my $contador = 0;

    foreach my $linha (@$dados) {
        $soma += $linha->[2];
        $contador++;
    }

    my $media = $soma / $contador;

    return $media;
}

# Função para escrever os resultados em um novo arquivo CSV
sub escrever_resultados {
    my ($dados_filtrados, $media) = @_;

    my $csv = Text::CSV->new({ binary => 1, eol => "\n" }) or die "Não foi possível criar o arquivo: $!";

    open(my $fh, ">:encoding(utf8)", "resultados.csv") or die "Não foi possível criar o arquivo: $!";

    foreach my $linha (@$dados_filtrados) {
        $csv->print($fh, $linha);
    }

    print $fh "Média dos valores da terceira coluna: $media\n";

    close($fh);
}

# Chamada das funções em sequência
my $arquivo_csv = "dados.csv";

my $dados = ler_arquivo_csv($arquivo_csv);
my $dados_filtrados = filtrar_dados($dados);
my $media = calcular_media($dados_filtrados);
escrever_resultados($dados_filtrados, $media);

print "Análise de dados concluída! Resultados salvos no arquivo resultados.csv.\n";
```

Primeiramente, é feita a inclusão dos módulos `strict`, `warnings` e `Text::CSV`, que ajudam a garantir boas práticas e fornecem funcionalidades para lidar com arquivos CSV.

Em seguida, temos a definição da função `ler_arquivo_csv`, que recebe o nome do arquivo CSV como parâmetro e retorna uma matriz com os dados do arquivo. A função utiliza o módulo `Text::CSV` para ler o arquivo linha por linha e armazenar os dados em uma matriz.

A função `filtrar_dados` recebe a matriz de dados e retorna uma nova matriz contendo apenas as linhas que atendem a um critério específico. Neste exemplo, o critério é que o valor da segunda coluna seja maior que 10.

A função `calcular_media` recebe a matriz de dados filtrados e calcula a média dos valores da terceira coluna.

A função `escrever_resultados` recebe a matriz de dados filtrados e a média calculada e escreve os resultados em um novo arquivo CSV chamado "resultados.csv". A função utiliza novamente o módulo `Text::CSV` para formatar os dados corretamente.

Por fim, as funções são chamadas em sequência para realizar a análise de dados. O nome do arquivo CSV de entrada é definido na variável `$arquivo_csv`. O resultado final é impresso na tela, indicando que a análise foi concluída e os resultados foram salvos no arquivo "resultados.csv".

Espero que esse código atenda às suas expectativas!