Claro! Aqui está um código complexo em Shell que realiza uma tarefa de processamento de arquivos:

```shell
#!/bin/bash

# Função para processar cada linha do arquivo
processar_linha() {
  linha=$1
  # Realizar algum processamento com a linha, como converter letras para maiúsculas
  linha_processada=$(echo "$linha" | tr '[:lower:]' '[:upper:]')
  echo $linha_processada
}

# Função para processar o arquivo
processar_arquivo() {
  arquivo=$1
  while IFS= read -r linha; do
    processar_linha "$linha"
  done < "$arquivo"
}

# Verificar se o número de argumentos é válido
if [ $# -eq 0 ]; then
  echo "Uso: ./processar_arquivo.sh arquivo.txt"
  exit 1
fi

# Verificar se o arquivo existe
arquivo=$1
if [ ! -f "$arquivo" ]; then
  echo "Arquivo não encontrado: $arquivo"
  exit 1
fi

# Processar o arquivo
processar_arquivo "$arquivo"
```

Este código em Shell é dividido em três partes principais:

1. A função `processar_linha()`: Esta função recebe uma linha como parâmetro e realiza algum processamento, como converter todas as letras para maiúsculas. Neste exemplo, usamos o comando `tr` para realizar a conversão.

2. A função `processar_arquivo()`: Esta função recebe um arquivo como parâmetro e lê cada linha do arquivo. Para cada linha, chama a função `processar_linha()` para processar a linha.

3. O bloco principal: Primeiro, verificamos se o número de argumentos é válido. Caso contrário, exibimos uma mensagem de uso e saímos com código de erro 1. Em seguida, verificamos se o arquivo fornecido existe. Caso contrário, exibimos uma mensagem de erro e saímos com código de erro 1. Finalmente, chamamos a função `processar_arquivo()` passando o arquivo fornecido como argumento.

Para utilizar esse código, você precisa salvá-lo em um arquivo com extensão `.sh`, como `processar_arquivo.sh`. Em seguida, dê permissão de execução para o arquivo usando o comando `chmod +x processar_arquivo.sh`. Agora você pode executar o código passando o nome do arquivo que deseja processar como argumento, por exemplo: `./processar_arquivo.sh arquivo.txt`. O código irá ler o arquivo fornecido, processar cada linha e exibir o resultado.