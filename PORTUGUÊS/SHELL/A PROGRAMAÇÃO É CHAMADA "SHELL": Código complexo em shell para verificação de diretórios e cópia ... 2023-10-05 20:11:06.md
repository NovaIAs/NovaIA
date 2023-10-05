Claro! Vou criar um código em shell bastante complexo e explicar cada parte dele em português. Aqui está:

```
#!/bin/bash

# Função para verificar se um diretório está vazio
function verificar_diretorio_vazio() {
    local diretorio="$1"
    if [ "$(ls -A "$diretorio")" ]; then
        return 1
    else
        return 0
    fi
}

# Função para realizar uma tarefa específica
function realizar_tarefa() {
    local arquivo="$1"
    local destino="$2"
    # Realize aqui a tarefa desejada, como copiar o arquivo para o destino
    cp "$arquivo" "$destino"
}

# Diretório de origem dos arquivos
diretorio_origem="/caminho/do/diretorio/origem"

# Diretório de destino dos arquivos
diretorio_destino="/caminho/do/diretorio/destino"

# Verificar se o diretório de origem está vazio
verificar_diretorio_vazio "$diretorio_origem"
if [ $? -eq 0 ]; then
    echo "O diretório de origem está vazio!"
    exit 1
fi

# Criar o diretório de destino, se não existir
if [ ! -d "$diretorio_destino" ]; then
    mkdir -p "$diretorio_destino"
fi

# Percorrer todos os arquivos do diretório de origem
for arquivo in "$diretorio_origem"/*; do
    # Verificar se o arquivo é um diretório
    if [ -d "$arquivo" ]; then
        continue
    fi

    # Extrair o nome do arquivo e a extensão
    nome_arquivo=$(basename "$arquivo")
    extensao="${nome_arquivo##*.}"

    # Verificar se a extensão é válida
    case "$extensao" in
        txt|pdf|docx)
            realizar_tarefa "$arquivo" "$diretorio_destino"
            ;;
        jpg|png|gif)
            realizar_tarefa "$arquivo" "$diretorio_destino"
            ;;
        *)
            echo "Extensão inválida: $extensao"
            ;;
    esac
done

echo "Tarefa concluída com sucesso!"
```

Este código em shell realiza as seguintes tarefas:

1. Verifica se o diretório de origem está vazio, utilizando a função `verificar_diretorio_vazio()`. Caso esteja vazio, exibe uma mensagem de erro e encerra a execução.
2. Cria o diretório de destino, caso não exista, utilizando o comando `mkdir -p`.
3. Percorre todos os arquivos do diretório de origem utilizando um loop `for`.
4. Verifica se cada arquivo é um diretório. Se for, o loop continua para o próximo arquivo.
5. Extrai o nome e a extensão do arquivo utilizando os comandos `basename` e a substituição de padrão `${nome_arquivo##*.}`.
6. Verifica se a extensão do arquivo é válida. No exemplo, são consideradas válidas as extensões `txt`, `pdf`, `docx`, `jpg`, `png` e `gif`. Para cada extensão válida, a função `realizar_tarefa()` é chamada para executar a tarefa desejada, como copiar o arquivo para o diretório de destino.
7. Se a extensão do arquivo não for válida, exibe uma mensagem de erro.
8. Ao final do loop, exibe uma mensagem de sucesso.

Espero que este código complexo em shell atenda às suas expectativas! Se tiver mais alguma dúvida ou precisar de algo mais específico, estou à disposição.