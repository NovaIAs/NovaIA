Claro! Aqui está um código complexo em PHP que envolve manipulação de arquivos, processamento de dados e geração de relatórios.

```php
<?php

// Função para ler um arquivo CSV
function lerArquivoCSV($caminhoArquivo) {
    $dados = [];
    
    if (($handle = fopen($caminhoArquivo, "r")) !== FALSE) {
        while (($linha = fgetcsv($handle, 1000, ",")) !== FALSE) {
            $dados[] = $linha;
        }
        fclose($handle);
    }
    
    return $dados;
}

// Função para processar os dados
function processarDados($dados) {
    $totalVendas = 0;
    $totalProdutos = 0;
    $relatorio = [];
    
    foreach ($dados as $linha) {
        $produto = $linha[0];
        $quantidade = $linha[1];
        $precoUnitario = $linha[2];
        
        $totalVendas += $quantidade * $precoUnitario;
        $totalProdutos += $quantidade;
        
        if (isset($relatorio[$produto])) {
            $relatorio[$produto]['quantidade'] += $quantidade;
            $relatorio[$produto]['valorTotal'] += $quantidade * $precoUnitario;
        } else {
            $relatorio[$produto] = [
                'quantidade' => $quantidade,
                'valorTotal' => $quantidade * $precoUnitario
            ];
        }
    }
    
    $relatorio['totalVendas'] = $totalVendas;
    $relatorio['totalProdutos'] = $totalProdutos;
    
    return $relatorio;
}

// Função para gerar o relatório
function gerarRelatorio($relatorio) {
    echo "Relatório de Vendas\n";
    echo "===================\n\n";
    
    foreach ($relatorio as $produto => $dados) {
        if ($produto != 'totalVendas' && $produto != 'totalProdutos') {
            echo "Produto: $produto\n";
            echo "Quantidade: " . $dados['quantidade'] . "\n";
            echo "Valor Total: R$" . number_format($dados['valorTotal'], 2, ',', '.') . "\n\n";
        }
    }
    
    echo "Total de Vendas: R$" . number_format($relatorio['totalVendas'], 2, ',', '.') . "\n";
    echo "Total de Produtos: " . $relatorio['totalProdutos'] . "\n";
}

// Caminho do arquivo CSV
$caminhoArquivoCSV = "vendas.csv";

// Leitura do arquivo CSV
$dados = lerArquivoCSV($caminhoArquivoCSV);

// Processamento dos dados
$relatorio = processarDados($dados);

// Geração do relatório
gerarRelatorio($relatorio);
?>
```

Este código lê um arquivo CSV contendo informações de vendas de produtos. Ele processa os dados e gera um relatório com o total de vendas por produto, quantidade vendida e valor total. Além disso, o relatório também mostra o total geral de vendas e o total de produtos vendidos.

Para utilizar o código, você precisa ter um arquivo CSV chamado "vendas.csv" no mesmo diretório do script PHP. O arquivo CSV deve ter três colunas: nome do produto, quantidade vendida e preço unitário.

O código começa definindo a função `lerArquivoCSV`, que lê o arquivo CSV e retorna os dados em um array. Em seguida, temos a função `processarDados`, que recebe os dados do arquivo CSV e realiza o processamento para gerar as informações do relatório.

A função `gerarRelatorio` é responsável por exibir o relatório na tela. Ela itera sobre os dados processados e exibe as informações de cada produto, além do total de vendas e total de produtos.

Por fim, o código lê o arquivo CSV, processa os dados e gera o relatório chamando as funções adequadas.

Espero que este código atenda às suas necessidades! Se tiver alguma dúvida adicional, estou à disposição para ajudar.