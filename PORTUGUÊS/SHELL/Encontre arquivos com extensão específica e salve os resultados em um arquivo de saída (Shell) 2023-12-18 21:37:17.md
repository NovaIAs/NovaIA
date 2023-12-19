Claro! Aqui está um exemplo de código complexo em Shell que realiza uma tarefa específica: encontrar todos os arquivos com uma determinada extensão em um diretório e suas subpastas e exibir os resultados em um arquivo de saída.

```shell
#!/bin/bash

# Função recursiva para percorrer todos os diretórios e subdiretórios
pesquisar_arquivos() {
    local diretorio="$1"
    local extensao="$2"
    local arquivo_saida="$3"

    # Percorre todos os arquivos e diretórios no diretório atual
    for arquivo in "$diretorio"/*; do
        if [[ -d "$arquivo" ]]; then
            # Se o item é um diretório, chama a função recursivamente para o diretório
            pesquisar_arquivos "$arquivo" "$extensao" "$arquivo_saida"
        elif [[ -f "$arquivo" ]]; then
            # Se o item é um arquivo, verifica a extensão
            if [[ "${arquivo##*.}" == "$extensao" ]]; then
                # Se a extensão corresponde, escreve o caminho completo do arquivo no arquivo de saída
                echo "$arquivo" >> "$arquivo_saida"
            fi
        fi
    done
}

# Diretório a ser pesquisado
diretorio_pesquisa="/caminho/do/diretorio"

# Extensão dos arquivos a serem encontrados
extensao_arquivos="txt"

# Arquivo de saída que irá conter a lista dos arquivos encontrados
arquivo_saida="/caminho/do/arquivo/saida.txt"

# Chama a função para pesquisar os arquivos no diretório especificado
pesquisar_arquivos "$diretorio_pesquisa" "$extensao_arquivos" "$arquivo_saida"

# Exibe uma mensagem informando que a pesquisa foi concluída
echo "A pesquisa foi concluída. Os resultados foram salvos em $arquivo_saida."
```

Explicação do código:

- A função `pesquisar_arquivos()` é uma função recursiva que recebe três parâmetros: o diretório a ser pesquisado, a extensão dos arquivos a serem encontrados e o arquivo de saída para salvar os resultados.
- O código principal começa definindo o diretório a ser pesquisado, a extensão dos arquivos a serem encontrados e o arquivo de saída.
- Em seguida, a função `pesquisar_arquivos()` é chamada com esses parâmetros.
- Dentro da função, é usado um loop `for` para percorrer todos os arquivos e diretórios no diretório especificado.
- Se um item for um diretório, a função é chamada recursivamente para esse diretório.
- Se um item for um arquivo, é verificado se a extensão corresponde à extensão especificada.
- Se a extensão corresponder, o caminho completo do arquivo é escrito no arquivo de saída.
- Ao finalizar a pesquisa, é exibida uma mensagem informando que a pesquisa foi concluída e os resultados foram salvos no arquivo de saída.

Espero que esse exemplo atenda às suas necessidades!