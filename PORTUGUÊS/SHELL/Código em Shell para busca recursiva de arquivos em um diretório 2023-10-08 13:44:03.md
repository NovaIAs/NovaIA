Claro! Aqui está um exemplo de código em Shell que realiza uma tarefa complexa: a busca recursiva de arquivos em um diretório e em seus subdiretórios, exibindo o caminho completo de cada arquivo encontrado.

```
#!/bin/bash

# Função para buscar arquivos recursivamente
buscar_arquivos() {
  local diretorio="$1"

  # Loop para percorrer todos os arquivos e diretórios no diretório atual
  for item in "$diretorio"/*; do
    if [ -f "$item" ]; then
      # Exibe o caminho completo do arquivo encontrado
      echo "Arquivo encontrado: $item"
    elif [ -d "$item" ]; then
      # Chama a função recursivamente para buscar dentro do subdiretório
      buscar_arquivos "$item"
    fi
  done
}

# Pede ao usuário que informe o diretório a ser buscado
echo "Digite o diretório a ser buscado:"
read diretorio_inicial

# Verifica se o diretório informado existe
if [ -d "$diretorio_inicial" ]; then
  # Chama a função para buscar arquivos
  buscar_arquivos "$diretorio_inicial"
else
  echo "Diretório inválido!"
fi
```

Neste código, começamos definindo uma função chamada `buscar_arquivos`, que recebe como parâmetro o diretório a ser buscado. Dentro dessa função, utilizamos um loop `for` para percorrer todos os itens (arquivos e diretórios) no diretório atual. 

Se o item for um arquivo (`-f "$item"`), exibimos o caminho completo do arquivo encontrado. Caso contrário, se for um diretório (`-d "$item"`), chamamos a função `buscar_arquivos` recursivamente para buscar dentro deste subdiretório.

Em seguida, pedimos ao usuário que informe o diretório a ser buscado e armazenamos essa entrada na variável `diretorio_inicial`. Verificamos se o diretório existe (`-d "$diretorio_inicial"`) e, se sim, chamamos a função `buscar_arquivos` passando o diretório inicial como argumento. Caso contrário, exibimos uma mensagem de erro.

Esse código pode ser útil para buscar arquivos em uma estrutura de diretórios complexa de forma recursiva.