Certamente! Aqui está um código complexo em Shell que realiza uma tarefa de processamento de dados avançada usando comandos do Linux:

```shell
#!/bin/bash

# Função para processar os arquivos
processar_arquivos() {
  # Verificar se o arquivo existe
  if [ -e "$1" ]; then
    # Obter o nome do arquivo
    nome_arquivo=$(basename "$1")
    echo "Processando o arquivo: $nome_arquivo"

    # Contagem de palavras
    qtd_palavras=$(wc -w "$1" | awk '{print $1}')
    echo "Número de palavras no arquivo: $qtd_palavras"

    # Contagem de linhas
    qtd_linhas=$(wc -l "$1" | awk '{print $1}')
    echo "Número de linhas no arquivo: $qtd_linhas"

    # Contagem de caracteres
    qtd_caracteres=$(wc -m "$1" | awk '{print $1}')
    echo "Número de caracteres no arquivo: $qtd_caracteres"

    # Calcular a média de palavras por linha
    media_palavras_por_linha=$(echo "scale=2; $qtd_palavras / $qtd_linhas" | bc)
    echo "Média de palavras por linha: $media_palavras_por_linha"

    echo "-----------------------------------"
  else
    echo "O arquivo $1 não existe."
  fi
}

# Diretório de entrada
diretorio_entrada="/caminho/do/diretorio"

# Obter a lista de arquivos do diretório
arquivos=$(ls "$diretorio_entrada")

# Loop para processar cada arquivo
for arquivo in $arquivos; do
  caminho_arquivo="$diretorio_entrada/$arquivo"
  processar_arquivos "$caminho_arquivo"
done
```

Explicação do código:
1. Começamos definindo uma função chamada `processar_arquivos` que recebe o caminho de um arquivo como parâmetro. Essa função irá processar o arquivo e exibir informações como o nome do arquivo, número de palavras, número de linhas, número de caracteres e média de palavras por linha.
2. Em seguida, definimos a variável `diretorio_entrada` que armazena o caminho do diretório onde estão os arquivos que serão processados. Você deve substituir "/caminho/do/diretorio" pelo caminho real do diretório desejado.
3. Utilizamos o comando `ls` para obter a lista de arquivos no diretório de entrada e armazenamos o resultado na variável `arquivos`.
4. Em um loop `for`, iteramos sobre cada arquivo da lista de arquivos.
5. Para cada arquivo, concatenamos o caminho completo do arquivo com o caminho do diretório de entrada e armazenamos na variável `caminho_arquivo`.
6. Chamamos a função `processar_arquivos` passando o `caminho_arquivo` como argumento.
7. A função `processar_arquivos` verifica se o arquivo existe usando o comando `[ -e "$1" ]`.
8. Se o arquivo existir, ele extrai o nome do arquivo usando o comando `basename "$1"` e o armazena na variável `nome_arquivo`.
9. Em seguida, usa os comandos `wc -w`, `wc -l` e `wc -m` para contar respectivamente o número de palavras, linhas e caracteres no arquivo. O resultado é armazenado nas variáveis `qtd_palavras`, `qtd_linhas` e `qtd_caracteres`.
10. Com base nessas contagens, calculamos a média de palavras por linha usando o comando `bc` e armazenamos na variável `media_palavras_por_linha`.
11. Por fim, exibimos todas as informações coletadas na tela, separadas por linhas tracejadas para facilitar a leitura.

Lembre-se de substituir "/caminho/do/diretorio" pelo caminho real do diretório que contém os arquivos de entrada que você deseja processar. Certifique-se também de ter as permissões adequadas para executar o script Shell.