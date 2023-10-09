Claro! Aqui está um exemplo de um código complexo em Shell que realiza uma tarefa de comparação de arquivos e exibe o resultado de forma detalhada:

```shell
#!/bin/bash

# Função para comparar dois arquivos
comparar_arquivos() {
    arquivo1="$1"
    arquivo2="$2"

    # Verificar se os dois arquivos existem
    if [ ! -f "$arquivo1" ] || [ ! -f "$arquivo2" ]; then
        echo "Erro: Arquivos não encontrados."
        return 1
    fi

    # Obter o tamanho dos arquivos
    tamanho_arquivo1=$(wc -c < "$arquivo1")
    tamanho_arquivo2=$(wc -c < "$arquivo2")

    # Verificar se os arquivos têm o mesmo tamanho
    if [ "$tamanho_arquivo1" != "$tamanho_arquivo2" ]; then
        echo "Os arquivos têm tamanhos diferentes."
    else
        echo "Os arquivos têm o mesmo tamanho."
    fi

    # Calcular o hash MD5 dos arquivos
    hash_arquivo1=$(md5sum "$arquivo1" | awk '{print $1}')
    hash_arquivo2=$(md5sum "$arquivo2" | awk '{print $1}')

    # Verificar se os hashes MD5 são iguais
    if [ "$hash_arquivo1" = "$hash_arquivo2" ]; then
        echo "Os arquivos são idênticos."
    else
        echo "Os arquivos são diferentes."
    fi
}

# Solicitar ao usuário os caminhos dos arquivos a serem comparados
echo "Digite o caminho do primeiro arquivo:"
read caminho_arquivo1

echo "Digite o caminho do segundo arquivo:"
read caminho_arquivo2

# Chamar a função para comparar os arquivos
comparar_arquivos "$caminho_arquivo1" "$caminho_arquivo2"
```

Neste código, utilizamos uma função chamada `comparar_arquivos` que recebe como parâmetros dois caminhos de arquivo. Em primeiro lugar, verificamos se os arquivos existem. Em seguida, obtemos o tamanho dos arquivos e comparamos se os tamanhos são iguais ou diferentes.

Em seguida, calculamos o hash MD5 de cada arquivo usando o comando `md5sum` e comparamos os hashes para determinar se os arquivos são idênticos ou diferentes.

O código também solicita ao usuário os caminhos dos arquivos a serem comparados e, em seguida, chama a função `comparar_arquivos` passando esses caminhos como argumentos.

Espero que esta demonstração de código em Shell atenda às suas necessidades!