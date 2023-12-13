Claro! Aqui está um exemplo de código complexo em Shell que realiza uma tarefa de gerenciamento de arquivos. O código a seguir cria um diretório chamado "Backup" no diretório atual, copia todos os arquivos com extensão ".txt" para esse diretório e, em seguida, comprime o diretório de backup em um arquivo tar.gz.

```shell
#!/bin/bash

# Criação do diretório de backup
echo "Criando diretório de backup..."
mkdir Backup

# Copia dos arquivos .txt para o diretório de backup
echo "Copiando arquivos .txt para o diretório de backup..."
find . -maxdepth 1 -type f -name "*.txt" -exec cp {} Backup/ \;

# Compressão do diretório de backup em um arquivo tar.gz
echo "Compactando diretório de backup..."
tar -czf backup.tar.gz Backup/

# Verificação do resultado
if [ -f backup.tar.gz ]; then
  echo "Backup criado com sucesso!"
else
  echo "Erro ao criar o backup."
fi
```

Explicação do código:
1. A primeira linha `#!/bin/bash` é conhecida como shebang e indica que o script deve ser executado utilizando o interpretador de comandos bash.
2. Na linha 4, é criado um diretório chamado "Backup" utilizando o comando `mkdir`.
3. A linha 7 utiliza o comando `find` para encontrar todos os arquivos com extensão ".txt" no diretório atual, e o comando `cp` é usado para copiar cada arquivo encontrado para o diretório de backup.
4. Na linha 10, o comando `tar` é utilizado para compactar o diretório de backup em um arquivo tar.gz. O parâmetro `-czf` indica a criação de um arquivo compactado no formato tar.gz.
5. Nas linhas 13-16, é feita uma verificação para confirmar se o arquivo de backup foi criado com sucesso.

Esse código é apenas um exemplo de complexidade em Shell e pode ser adaptado conforme necessário para atender às suas necessidades específicas.