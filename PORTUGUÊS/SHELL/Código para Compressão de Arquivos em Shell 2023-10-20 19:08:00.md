Claro! Aqui está um código complexo em Shell que realiza um processo de compressão de arquivos em um diretório específico. O código utiliza os comandos "find", "tar" e "gzip" para encontrar os arquivos, criar um arquivo tar e, em seguida, comprimi-lo usando o algoritmo de compressão Gzip.

```shell
#!/bin/bash

# Diretório onde os arquivos serão comprimidos
diretorio="/caminho/do/diretorio"

# Nome do arquivo tar
nome_arquivo="arquivos_comprimidos_$(date +%Y%m%d%H%M%S).tar.gz"

# Encontrar todos os arquivos no diretório (exceto diretórios)
arquivos=$(find "$diretorio" -type f)

# Criar arquivo tar
tar -cf "$nome_arquivo" $arquivos

# Comprimir arquivo tar usando Gzip
gzip "$nome_arquivo"

# Verificar se a compressão foi bem-sucedida
if [ $? -eq 0 ]; then
    echo "Arquivos comprimidos com sucesso!"
else
    echo "Erro ao comprimir os arquivos."
fi
```

Explicação do código:

1. Definimos o diretório onde os arquivos serão comprimidos na variável `diretorio`.
2. Definimos o nome do arquivo tar usando a data e hora atual na variável `nome_arquivo`.
3. Utilizamos o comando `find` para encontrar todos os arquivos no diretório especificado, excluindo os diretórios. Os resultados são armazenados na variável `arquivos`.
4. Utilizamos o comando `tar` para criar um arquivo tar com o nome especificado, e passamos a lista de arquivos encontrados como argumento.
5. Utilizamos o comando `gzip` para comprimir o arquivo tar gerado anteriormente usando o algoritmo Gzip.
6. Verificamos se a compressão foi bem-sucedida verificando o código de saída do comando anterior (`$?`). Se for igual a 0, exibimos uma mensagem de sucesso. Caso contrário, exibimos uma mensagem de erro.

Esse código pode ser executado em um terminal Shell para comprimir todos os arquivos em um diretório específico em um único arquivo tar.gz. Certifique-se de substituir "/caminho/do/diretorio" pelo caminho correto do diretório que você deseja comprimir.