Claro! Abaixo está um exemplo de um código em Shell que realiza um processo complexo de análise de arquivos em um diretório e gera um relatório detalhado.

```bash
#!/bin/bash

# Função para calcular o tamanho de um arquivo em kilobytes
tamanho_arquivo() {
    arquivo=$1
    tamanho=$(du -k "$arquivo" | cut -f1)
    echo "Tamanho do arquivo $arquivo: $tamanho KB"
}

# Função para verificar a quantidade de linhas em um arquivo
contar_linhas() {
    arquivo=$1
    linhas=$(wc -l < "$arquivo")
    echo "Quantidade de linhas do arquivo $arquivo: $linhas"
}

# Função para verificar a quantidade de palavras em um arquivo
contar_palavras() {
    arquivo=$1
    palavras=$(wc -w < "$arquivo")
    echo "Quantidade de palavras do arquivo $arquivo: $palavras"
}

# Função para gerar um relatório detalhado de um diretório
gerar_relatorio() {
    diretorio=$1
    relatorio="relatorio.txt"

    # Inicializa o relatório
    echo "Relatório de análise de arquivos" > "$relatorio"
    echo "Diretório analisado: $diretorio" >> "$relatorio"
    echo "------------------------------" >> "$relatorio"

    # Percorre todos os arquivos no diretório
    for arquivo in "$diretorio"/*; do
        if [ -f "$arquivo" ]; then
            echo "Arquivo: $arquivo" >> "$relatorio"

            # Calcula o tamanho do arquivo
            tamanho_arquivo "$arquivo" >> "$relatorio"

            # Conta a quantidade de linhas do arquivo
            contar_linhas "$arquivo" >> "$relatorio"

            # Conta a quantidade de palavras do arquivo
            contar_palavras "$arquivo" >> "$relatorio"

            echo "------------------------------" >> "$relatorio"
        fi
    done

    echo "Relatório gerado com sucesso em $relatorio."
}

# Executa o código principal
diretorio_analisado="/caminho/do/diretorio"
gerar_relatorio "$diretorio_analisado"
```

Este código em Shell é um exemplo de um programa complexo que analisa arquivos em um diretório específico e gera um relatório detalhado com informações como tamanho do arquivo, quantidade de linhas e palavras. 

A função `tamanho_arquivo` utiliza o comando `du` para obter o tamanho do arquivo em kilobytes e a função `contar_linhas` utiliza o comando `wc` para contar o número de linhas. Já a função `contar_palavras` utiliza o mesmo comando `wc` para contar o número de palavras.

A função `gerar_relatorio` recebe como parâmetro o diretório que será analisado. Ela cria um arquivo de relatório chamado "relatorio.txt" e percorre todos os arquivos no diretório. Para cada arquivo encontrado, são chamadas as funções auxiliares para obter as informações desejadas e gravar no relatório.

Por fim, o código principal define o diretório a ser analisado e chama a função `gerar_relatorio` passando o diretório como parâmetro.

Este código é apenas um exemplo e pode ser adaptado para atender às necessidades específicas de um projeto real.