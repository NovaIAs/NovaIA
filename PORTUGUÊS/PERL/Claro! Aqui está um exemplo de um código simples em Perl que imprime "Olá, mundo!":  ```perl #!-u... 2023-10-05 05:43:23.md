Claro! Aqui está um exemplo de um código complexo em Perl que realiza um processamento de dados e gera um relatório:

```perl
#!/usr/bin/perl

use strict;
use warnings;

# Função para processar os dados
sub processar_dados {
    my ($dados) = @_;

    # Inicialização de variáveis
    my %total_por_categoria;
    my %quantidade_por_categoria;
    my $total_geral = 0;
    my $quantidade_geral = 0;

    # Processamento dos dados
    foreach my $linha (@$dados) {
        my ($categoria, $valor) = split(',', $linha);

        # Atualização dos totais por categoria
        $total_por_categoria{$categoria} += $valor;
        $quantidade_por_categoria{$categoria}++;

        # Atualização do total geral
        $total_geral += $valor;
        $quantidade_geral++;
    }

    # Impressão do relatório
    print "Relatório de Dados:\n\n";
    print "Total Geral: $total_geral\n";
    print "Quantidade Geral: $quantidade_geral\n\n";

    foreach my $categoria (keys %total_por_categoria) {
        my $total_categoria = $total_por_categoria{$categoria};
        my $quantidade_categoria = $quantidade_por_categoria{$categoria};

        print "Categoria: $categoria\n";
        print "Total: $total_categoria\n";
        print "Quantidade: $quantidade_categoria\n\n";
    }
}

# Dados de exemplo
my @dados = (
    "Categoria A,100",
    "Categoria B,200",
    "Categoria A,150",
    "Categoria C,300",
    "Categoria B,250",
    "Categoria A,50"
);

# Chamada da função para processar os dados
processar_dados(\@dados);
```

Neste exemplo, temos um código em Perl que processa uma lista de dados representados por categorias e valores. Ele armazena o total e a quantidade de cada categoria, bem como o total geral e a quantidade geral de dados.

Primeiro, definimos a função `processar_dados` que recebe uma referência para a lista de dados. Dentro da função, inicializamos as variáveis necessárias para armazenar os totais e quantidades por categoria, bem como as variáveis para o total geral e a quantidade geral.

Em seguida, percorremos a lista de dados utilizando um loop `foreach`. Para cada linha de dados, separamos a categoria e o valor utilizando a função `split`. Atualizamos os totais e quantidades por categoria e também o total geral e a quantidade geral.

Ao final do processamento, imprimimos o relatório de dados na tela. Primeiro, exibimos o total geral e a quantidade geral. Depois, percorremos as categorias utilizando o `foreach` e imprimimos o total e a quantidade para cada categoria.

Por fim, temos uma lista de dados de exemplo representados por categorias e valores. Chamamos a função `processar_dados` passando a referência para essa lista como argumento.

Espero que esse exemplo atenda às suas expectativas!