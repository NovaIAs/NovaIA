Claro! Aqui está um código complexo em shell script que envolve manipulação de arquivos, loops e funções:

```shell
#!/bin/bash

# Função para verificar se a linha é um comentário
is_comentario() {
    if [[ $1 =~ ^[[:space:]]*# ]]; then
        return 0
    else
        return 1
    fi
}

# Função para verificar se a linha está vazia
is_linha_vazia() {
    if [[ -z $1 ]]; then
        return 0
    else
        return 1
    fi
}

# Diretório onde os arquivos serão salvos
diretorio=/caminho/para/diretorio

# Loop para percorrer todos os arquivos em um diretório específico
for arquivo in $diretorio/*; do
    # Verifica se o arquivo é um diretório
    if [[ -d $arquivo ]]; then
        continue
    fi
    
    # Cria um novo arquivo com o mesmo nome, mas com a extensão ".novo"
    novo_arquivo="${arquivo}.novo"
    touch $novo_arquivo
    
    # Loop para percorrer as linhas do arquivo original
    while IFS= read -r linha; do
        # Verifica se a linha é um comentário ou está vazia
        if is_comentario "$linha" || is_linha_vazia "$linha"; then
            # Adiciona a linha no novo arquivo sem modificações
            echo "$linha" >> $novo_arquivo
        else
            # Transforma a linha em maiúsculas e adiciona no novo arquivo
            linha_maiuscula=$(echo "$linha" | tr '[:lower:]' '[:upper:]')
            echo "$linha_maiuscula" >> $novo_arquivo
        fi
    done < "$arquivo"
    
    echo "Arquivo $arquivo processado com sucesso!"
done

echo "Todos os arquivos foram processados."
```

Neste código, temos algumas funções definidas para verificar se uma linha é um comentário ou está vazia. Em seguida, definimos um diretório onde os arquivos serão salvos.

Utilizando um loop, percorremos todos os arquivos nesse diretório. Se o arquivo for um diretório, pulamos para a próxima iteração.

Criamos um novo arquivo com a extensão ".novo" e, em seguida, percorremos cada linha do arquivo original. Se a linha for um comentário ou estiver vazia, ela é adicionada ao novo arquivo sem modificações. Caso contrário, transformamos a linha em maiúsculas e a adicionamos no novo arquivo.

Ao final, exibimos uma mensagem informando que todos os arquivos foram processados.

Espero que este código atenda às suas expectativas! Lembre-se de substituir o valor do diretório para o caminho correto antes de executar o script.