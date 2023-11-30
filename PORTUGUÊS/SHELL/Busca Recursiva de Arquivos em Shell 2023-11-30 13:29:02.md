Claro! Aqui está um código complexo em Shell que realiza um processo de busca recursiva por arquivos em um diretório especificado pelo usuário, exibindo informações detalhadas sobre cada arquivo encontrado:

```shell
#!/bin/bash

# Função que realiza a busca recursiva nos diretórios
function buscar_arquivos {
    local diretorio=$1

    # Loop para percorrer cada arquivo/diretório no diretório atual
    for item in "$diretorio"/*; do
        if [[ -d "$item" ]]; then
            # Se for um diretório, chama a função recursivamente para buscar nele
            buscar_arquivos "$item"
        elif [[ -f "$item" ]]; then
            # Se for um arquivo, exibe informações detalhadas
            nome_arquivo=$(basename "$item")
            tamanho_arquivo=$(du -h "$item" | cut -f1)
            data_modificacao=$(stat -c %x "$item")

            echo "Arquivo encontrado: $item"
            echo "Nome: $nome_arquivo"
            echo "Tamanho: $tamanho_arquivo"
            echo "Data de modificação: $data_modificacao"
            echo "-------------------------"
        fi
    done
}

# Solicita ao usuário o diretório inicial para a busca
read -p "Digite o diretório inicial para a busca: " diretorio_inicial

# Verifica se o diretório existe e é válido
if [[ -d "$diretorio_inicial" ]]; then
    echo "Iniciando busca no diretório: $diretorio_inicial"
    echo "-------------------------"

    # Chama a função para buscar os arquivos
    buscar_arquivos "$diretorio_inicial"

    echo "Busca concluída."
else
    echo "Diretório inválido."
fi
```

Explicação do código:
1. O código começa com a declaração do shebang `#!/bin/bash`, indicando que o script será executado em um ambiente bash.
2. Em seguida, definimos a função `buscar_arquivos`, que realiza a busca recursiva nos diretórios.
3. Dentro da função `buscar_arquivos`, fazemos um loop para percorrer cada item (arquivos e diretórios) do diretório atual.
4. Se o item for um diretório, chamamos a função `buscar_arquivos` recursivamente para buscar nele.
5. Se o item for um arquivo, exibimos informações detalhadas sobre o arquivo, como o nome, tamanho e data de modificação.
6. No corpo principal do código, solicitamos ao usuário o diretório inicial para a busca.
7. Verificamos se o diretório existe e é válido.
8. Se o diretório for válido, iniciamos a busca no diretório informado, chamando a função `buscar_arquivos`.
9. Ao final da busca, exibimos uma mensagem informando que a busca foi concluída.

Espero que esse código atenda às suas necessidades!