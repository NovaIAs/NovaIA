Claro! Aqui está um código complexo em Shell que realiza a tarefa de ordenar um arquivo de texto chamado "input.txt" em ordem alfabética e salvar o resultado no arquivo "output.txt". 

```shell
#!/bin/bash

# Função para ordenar o arquivo
ordenar_arquivo() {
    local arquivo_entrada=$1
    local arquivo_saida=$2

    # Verificar se o arquivo de entrada existe
    if [ ! -f "$arquivo_entrada" ]; then
        echo "O arquivo de entrada não existe!"
        exit 1
    fi

    # Verificar se o arquivo de saída já existe
    if [ -f "$arquivo_saida" ]; then
        echo "O arquivo de saída já existe!"
        exit 1
    fi

    # Ordenar o arquivo de entrada e salvar o resultado no arquivo de saída
    sort "$arquivo_entrada" -o "$arquivo_saida"

    echo "Arquivo ordenado com sucesso!"
}

# Chamada da função para ordenar o arquivo
ordenar_arquivo "input.txt" "output.txt"
```

Explicação do código:

1. A primeira linha `#!/bin/bash` indica que o interpretador a ser usado é o Bash.

2. A função `ordenar_arquivo` recebe dois parâmetros: o nome do arquivo de entrada e o nome do arquivo de saída.

3. Dentro da função, verificamos se o arquivo de entrada existe usando o comando `if [ ! -f "$arquivo_entrada" ]; then`. Caso não exista, exibimos uma mensagem de erro e encerramos o script com `exit 1`.

4. Em seguida, verificamos se o arquivo de saída já existe usando o comando `if [ -f "$arquivo_saida" ]; then`. Caso exista, exibimos uma mensagem de erro e encerramos o script com `exit 1`.

5. Utilizamos o comando `sort` para ordenar o arquivo de entrada em ordem alfabética e redirecionamos a saída para o arquivo de saída com a opção `-o`.

6. Por fim, exibimos uma mensagem de sucesso informando que o arquivo foi ordenado com sucesso.

Para utilizar esse código, basta salvar em um arquivo com a extensão .sh (por exemplo, "ordenar_arquivo.sh"), dar permissão de execução usando o comando `chmod +x ordenar_arquivo.sh` e executar o script com `./ordenar_arquivo.sh`. Certifique-se de que o arquivo de entrada "input.txt" existe e contém o conteúdo que deseja ordenar. O resultado será salvo no arquivo "output.txt".