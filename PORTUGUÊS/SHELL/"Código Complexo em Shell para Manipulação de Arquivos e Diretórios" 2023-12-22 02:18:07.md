Claro! Aqui está um exemplo de um código complexo em Shell que realiza uma série de tarefas relacionadas à manipulação de arquivos e diretórios:

```shell
#!/bin/bash

# Função para contar o número de linhas em um arquivo
contar_linhas() {
    arquivo=$1
    linhas=$(wc -l < "$arquivo")
    echo "O arquivo $arquivo possui $linhas linhas."
}

# Função para criar um diretório com base em um prefixo e sufixo
criar_diretorio() {
    prefixo=$1
    sufixo=$2
    data=$(date +%Y%m%d%H%M%S)
    diretorio="$prefixo$data$sufixo"
    mkdir "$diretorio"
    echo "Diretório $diretorio criado com sucesso."
}

# Função para renomear arquivos em um diretório
renomear_arquivos() {
    diretorio=$1
    prefixo=$2
    sufixo=$3
    contador=1
    for arquivo in "$diretorio"/*; do
        if [ -f "$arquivo" ]; then
            novo_nome="$prefixo$contador$sufixo"
            mv "$arquivo" "$diretorio/$novo_nome"
            echo "Arquivo $arquivo renomeado para $novo_nome."
            ((contador++))
        fi
    done
}

# Função para compactar um diretório em um arquivo tar.gz
compactar_diretorio() {
    diretorio=$1
    arquivo_destino=$2
    tar -czf "$arquivo_destino" "$diretorio"
    echo "Diretório $diretorio compactado em $arquivo_destino."
}

# Função principal
main() {
    # Contar o número de linhas no arquivo exemplo.txt
    contar_linhas "exemplo.txt"

    # Criar um diretório com prefixo "dir" e sufixo "_backup"
    criar_diretorio "dir" "_backup"

    # Renomear arquivos no diretório atual com prefixo "arquivo_" e sufixo ".txt"
    renomear_arquivos "." "arquivo_" ".txt"

    # Compactar o diretório atual em um arquivo tar.gz chamado "backup.tar.gz"
    compactar_diretorio "." "backup.tar.gz"
}

# Executar a função principal
main
```

Neste exemplo, temos várias funções que podem ser úteis em diferentes cenários. A função `contar_linhas` recebe um arquivo como parâmetro e usa o comando `wc -l` para contar o número de linhas. A função `criar_diretorio` cria um diretório com base em um prefixo, sufixo e na data atual. A função `renomear_arquivos` renomeia todos os arquivos em um diretório com base em um prefixo, sufixo e um contador. A função `compactar_diretorio` compacta um diretório em um arquivo tar.gz usando o comando `tar`.

Na função principal `main`, realizamos uma série de operações de exemplo. Contamos as linhas de um arquivo chamado `exemplo.txt`, criamos um diretório com prefixo `dir` e sufixo `_backup`, renomeamos arquivos no diretório atual com prefixo `arquivo_` e sufixo `.txt` e, por fim, compactamos o diretório atual em um arquivo tar.gz chamado `backup.tar.gz`.

Espero que este código complexo em Shell seja útil para você!