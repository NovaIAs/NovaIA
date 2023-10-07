Claro! Aqui está um código em Shell (bash) que realiza uma tarefa complexa: ele lê um arquivo contendo uma lista de nomes de arquivos e diretórios, extrai informações relevantes sobre cada um deles, e armazena essas informações em um novo arquivo.

```shell
#!/bin/bash

# Função para extrair informações de um arquivo/diretório
extrair_informacoes() {
    local arquivo=$1
    
    # Verifica se é um arquivo ou diretório
    if [ -f "$arquivo" ]; then
        tipo="Arquivo"
    elif [ -d "$arquivo" ]; then
        tipo="Diretório"
    else
        tipo="Desconhecido"
    fi

    # Obtém o tamanho do arquivo/diretório
    tamanho=$(du -sh "$arquivo" | awk '{ print $1 }')

    # Obtém a data de modificação do arquivo/diretório
    data_modificacao=$(stat -c "%y" "$arquivo")

    # Escreve as informações no novo arquivo
    echo "Nome: $arquivo" >> informacoes.txt
    echo "Tipo: $tipo" >> informacoes.txt
    echo "Tamanho: $tamanho" >> informacoes.txt
    echo "Data de modificação: $data_modificacao" >> informacoes.txt
    echo "" >> informacoes.txt
}

# Lê o arquivo contendo a lista de nomes de arquivos/diretórios
arquivo_lista="lista.txt"
while IFS= read -r nome; do
    extrair_informacoes "$nome"
done < "$arquivo_lista"
```

Neste código, começamos definindo a função `extrair_informacoes()`, que recebe um arquivo/diretório como parâmetro. Dentro dessa função, verificamos se o parâmetro é um arquivo ou diretório usando as condições `if` e `elif`. Em seguida, usamos os comandos `du` e `awk` para obter o tamanho do arquivo/diretório e o comando `stat` para obter a data de modificação. Por fim, escrevemos todas essas informações no arquivo "informacoes.txt".

Na parte principal do código, definimos a variável `arquivo_lista` com o nome do arquivo que contém a lista de nomes de arquivos/diretórios. Em seguida, usamos um loop `while` para ler cada linha desse arquivo e chamar a função `extrair_informacoes()` para cada nome lido.

Esse código é capaz de lidar com diferentes tipos de arquivos e diretórios e extrair informações importantes sobre eles, como tamanho e data de modificação. Ele também é capaz de lidar com uma lista grande de nomes, pois utiliza um loop para processá-los um por um.