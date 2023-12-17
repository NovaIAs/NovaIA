Claro! Aqui está um exemplo de um código complexo em Shell, que realiza uma tarefa de manipulação de arquivos e diretórios:

```shell
#!/bin/bash

# Função para listar todos os arquivos em um diretório e subdiretórios
listar_arquivos() {
    local diretorio=$1
    for arquivo in "$diretorio"/*; do
        if [ -f "$arquivo" ]; then
            echo "Arquivo encontrado: $arquivo"
        elif [ -d "$arquivo" ]; then
            listar_arquivos "$arquivo"
        fi
    done
}

# Diretório raiz
diretorio_raiz="/caminho/do/diretorio"

# Verifica se o diretório raiz existe
if [ -d "$diretorio_raiz" ]; then
    echo "Diretório raiz encontrado: $diretorio_raiz"
    listar_arquivos "$diretorio_raiz"
else
    echo "Diretório raiz não encontrado: $diretorio_raiz"
fi
```

Neste código, temos uma função chamada `listar_arquivos` que recebe um diretório como parâmetro e percorre recursivamente todos os arquivos e diretórios dentro dele. Se um arquivo for encontrado, é exibida uma mensagem informando o caminho completo do arquivo. Caso um subdiretório seja encontrado, a função é chamada recursivamente para listar os arquivos dentro dele.

Em seguida, temos uma variável `diretorio_raiz` que define o caminho do diretório raiz a ser verificado. Se o diretório raiz existir, é exibida uma mensagem informando o caminho completo do diretório, e a função `listar_arquivos` é chamada passando o diretório raiz como parâmetro. Caso contrário, é exibida uma mensagem informando que o diretório raiz não foi encontrado.

Esse é apenas um exemplo de código complexo em Shell, mas há muitas outras possibilidades e funcionalidades que podem ser implementadas.