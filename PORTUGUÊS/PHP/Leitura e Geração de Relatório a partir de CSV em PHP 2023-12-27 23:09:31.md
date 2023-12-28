Claro! Aqui está um código PHP complexo que faz a leitura de um arquivo CSV e gera um relatório com os dados encontrados. O código foi escrito em português e possui explicações detalhadas em cada trecho:

```php
<?php

// Função para ler o arquivo CSV e retornar os dados como um array
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

// Função para gerar o relatório com os dados do arquivo CSV
function gerarRelatorio($dados) {
    $relatorio = "";

    // Verifica se existem dados para serem processados
    if (!empty($dados)) {
        // Obtém o cabeçalho do arquivo CSV
        $cabecalho = $dados[0];
        $numColunas = count($cabecalho);

        // Inicia a construção do relatório
        $relatorio .= "<table>";
        $relatorio .= "<thead><tr>";

        // Adiciona as colunas do cabeçalho no relatório
        foreach ($cabecalho as $coluna) {
            $relatorio .= "<th>$coluna</th>";
        }

        $relatorio .= "</tr></thead>";
        $relatorio .= "<tbody>";

        // Pula o primeiro elemento do array (cabeçalho)
        for ($i = 1; $i < count($dados); $i++) {
            $linha = $dados[$i];
            $relatorio .= "<tr>";

            // Adiciona os valores de cada coluna no relatório
            for ($j = 0; $j < $numColunas; $j++) {
                $valor = isset($linha[$j]) ? $linha[$j] : "";
                $relatorio .= "<td>$valor</td>";
            }

            $relatorio .= "</tr>";
        }

        $relatorio .= "</tbody></table>";
    } else {
        $relatorio = "Não foram encontrados dados para gerar o relatório.";
    }

    return $relatorio;
}

// Caminho do arquivo CSV
$caminhoArquivoCSV = "caminho/do/arquivo.csv";

// Lê o arquivo CSV
$dadosCSV = lerArquivoCSV($caminhoArquivoCSV);

// Gera o relatório
$relatorioCSV = gerarRelatorio($dadosCSV);

// Exibe o relatório na tela
echo $relatorioCSV;
```

Neste código, temos duas funções principais:

1. `lerArquivoCSV($caminhoArquivo)`: Essa função recebe o caminho do arquivo CSV como parâmetro e retorna os dados do arquivo como um array. Ela utiliza a função `fopen` para abrir o arquivo, `fgetcsv` para ler as linhas do arquivo e `fclose` para fechá-lo.

2. `gerarRelatorio($dados)`: Essa função recebe os dados obtidos do arquivo CSV e gera um relatório em formato de tabela HTML. Ela percorre o array de dados, montando as linhas e colunas do relatório. Caso não haja dados no array, exibe uma mensagem informando que não foram encontrados dados.

No código principal, definimos o caminho do arquivo CSV que será lido e chamamos as funções `lerArquivoCSV` e `gerarRelatorio` para obter e processar os dados do arquivo. Em seguida, o relatório é exibido na tela utilizando o `echo`.