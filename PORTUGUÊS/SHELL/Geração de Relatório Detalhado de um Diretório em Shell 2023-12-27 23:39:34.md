Claro! Abaixo está um exemplo de um código complexo em Shell, que realiza o processamento de um diretório, extraindo informações de arquivos e gerando um relatório detalhado em um arquivo de saída.

```shell
#!/bin/bash

# Função para processar um arquivo
processar_arquivo() {
    arquivo=$1

    # Verificar o tipo do arquivo
    tipo=$(file -b "$arquivo")

    # Extrair informações do arquivo
    tamanho=$(du -h "$arquivo" | awk '{print $1}')
    data_modificacao=$(stat -c %x "$arquivo")
    permissoes=$(stat -c %a "$arquivo")

    # Escrever as informações no arquivo de saída
    echo "Arquivo: $arquivo" >> relatorio.txt
    echo "Tipo: $tipo" >> relatorio.txt
    echo "Tamanho: $tamanho" >> relatorio.txt
    echo "Data de modificação: $data_modificacao" >> relatorio.txt
    echo "Permissões: $permissoes" >> relatorio.txt
    echo "--------------------------------------" >> relatorio.txt
}

# Função para processar um diretório
processar_diretorio() {
    diretorio=$1

    # Verificar se o diretório existe
    if [[ ! -d "$diretorio" ]]; then
        echo "Diretório não encontrado: $diretorio"
        return
    fi

    # Listar os arquivos no diretório
    arquivos=$(find "$diretorio" -type f)

    # Processar cada arquivo encontrado
    for arquivo in $arquivos; do
        processar_arquivo "$arquivo"
    done
}

# Diretório a ser processado
diretorio="/caminho/do/diretorio"

# Verificar se o diretório existe
if [[ ! -d "$diretorio" ]]; then
    echo "Diretório não encontrado: $diretorio"
    exit 1
fi

# Criar arquivo de saída
echo "Relatório de Arquivos" > relatorio.txt
echo "Diretório: $diretorio" >> relatorio.txt
echo "--------------------------------------" >> relatorio.txt

# Processar o diretório
processar_diretorio "$diretorio"

# Mensagem de conclusão
echo "Processamento concluído. O relatório foi salvo em relatorio.txt"
```

Neste código, é definida uma função `processar_arquivo` que recebe como parâmetro o caminho de um arquivo e extrai informações como tipo, tamanho, data de modificação e permissões. Essas informações são escritas no arquivo de saída `relatorio.txt`.

A função `processar_diretorio` recebe o caminho de um diretório como parâmetro, verifica se ele existe e lista todos os arquivos encontrados. Em seguida, chama a função `processar_arquivo` para cada arquivo encontrado.

No código principal, é definido o diretório a ser processado e verificado se ele existe. É criado o arquivo `relatorio.txt` e o cabeçalho do relatório é escrito nele. Em seguida, a função `processar_diretorio` é chamada para processar o diretório especificado. Por fim, uma mensagem de conclusão é exibida.

Esse código pode ser utilizado para gerar um relatório detalhado de um diretório, útil para análise de arquivos e permissões.