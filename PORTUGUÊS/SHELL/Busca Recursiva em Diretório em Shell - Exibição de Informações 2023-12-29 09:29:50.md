Claro! Abaixo está um exemplo de um código complexo em Shell que realiza um processo de busca recursiva em um diretório e exibe informações sobre os arquivos encontrados:

```shell
#!/bin/bash

# Função recursiva para percorrer diretórios
buscar_arquivos() {
    local diretorio=$1

    # Loop pelos arquivos e diretórios no diretório atual
    for item in "$diretorio"/*; do
        if [ -d "$item" ]; then
            # Se for um diretório, chama a função recursivamente
            buscar_arquivos "$item"
        elif [ -f "$item" ]; then
            # Se for um arquivo, exibe informações
            nome_arquivo=$(basename "$item")
            tamanho_arquivo=$(du -h "$item" | awk '{print $1}')
            data_modificacao=$(stat -c %x "$item")

            echo "Arquivo encontrado: $nome_arquivo"
            echo "Tamanho: $tamanho_arquivo"
            echo "Data de modificação: $data_modificacao"
            echo "---------------------------"
        fi
    done
}

# Solicita ao usuário o diretório a ser buscado
echo "Digite o diretório a ser buscado:"
read diretorio_busca

# Verifica se o diretório existe
if [ -d "$diretorio_busca" ]; then
    echo "Buscando arquivos em $diretorio_busca..."
    echo "---------------------------"

    # Chama a função de busca
    buscar_arquivos "$diretorio_busca"

    echo "---------------------------"
    echo "Busca concluída!"
else
    echo "Diretório não encontrado."
fi
```

Explicação do código:

1. A primeira linha `#!/bin/bash` indica que o interpretador a ser utilizado é o Bash. Isso é necessário para executar o script Shell.

2. A função `buscar_arquivos` é definida para percorrer recursivamente os diretórios e exibir informações sobre os arquivos encontrados. Ela recebe um parâmetro `diretorio` que representa o diretório a ser percorrido.

3. Dentro da função `buscar_arquivos`, utilizamos um loop `for` para percorrer os itens no diretório atual (`"$diretorio"/*`). Se o item for um diretório, chamamos a função novamente de forma recursiva. Se for um arquivo, exibimos informações sobre ele.

4. Utilizamos comandos como `basename`, `du`, `awk` e `stat` para obter informações sobre os arquivos, como o nome, tamanho e data de modificação.

5. O código principal começa solicitando ao usuário o diretório a ser buscado. Em seguida, verificamos se o diretório existe.

6. Se o diretório existir, chamamos a função `buscar_arquivos` passando o diretório como argumento.

7. No final, exibimos uma mensagem indicando que a busca foi concluída.

Esse é um exemplo de código complexo em Shell que demonstra o uso de funções, loops, comandos de manipulação de arquivos e interação com o usuário. Esse código pode ser útil para realizar buscas em diretórios e exibir informações sobre os arquivos encontrados.