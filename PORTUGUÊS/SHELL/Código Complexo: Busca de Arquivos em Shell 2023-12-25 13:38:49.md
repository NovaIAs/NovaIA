Claro! Aqui está um exemplo de um código complexo em Shell que utiliza várias funcionalidades da linguagem para criar uma aplicação de busca de arquivos em um diretório específico:

```shell
#!/bin/bash

# Função para buscar arquivos em um diretório recursivamente
buscar_arquivos() {
    local diretorio="$1"
    local padrao="$2"

    # Loop pelos arquivos e diretórios no diretório atual
    for entrada in "$diretorio"/*; do
        if [[ -d "$entrada" ]]; then
            # Chama a função recursivamente para o diretório encontrado
            buscar_arquivos "$entrada" "$padrao"
        elif [[ -f "$entrada" ]]; then
            # Verifica se o arquivo corresponde ao padrão
            if [[ "$(basename "$entrada")" == *"$padrao"* ]]; then
                echo "Arquivo encontrado: $entrada"
            fi
        fi
    done
}

# Solicita ao usuário o diretório e o padrão de busca
read -p "Digite o diretório a ser pesquisado: " diretorio
read -p "Digite o padrão de busca: " padrao

# Chama a função de busca de arquivos
buscar_arquivos "$diretorio" "$padrao"
```

Este código é um exemplo de uma aplicação de busca de arquivos em um diretório específico. Ele utiliza uma função `buscar_arquivos` que recebe o diretório e o padrão de busca como parâmetros e realiza a busca recursivamente em todos os diretórios e subdiretórios.

Primeiro, o usuário é solicitado a fornecer o diretório a ser pesquisado e o padrão de busca. Em seguida, a função `buscar_arquivos` é chamada passando esses valores como argumentos.

Dentro da função, é feito um loop por todos os arquivos e diretórios no diretório atual. Se um diretório for encontrado, a função é chamada recursivamente para esse diretório. Se um arquivo for encontrado, é verificado se o nome do arquivo corresponde ao padrão de busca. Se corresponder, uma mensagem é exibida indicando que o arquivo foi encontrado.

Espero que este exemplo tenha sido útil e atenda às suas necessidades!