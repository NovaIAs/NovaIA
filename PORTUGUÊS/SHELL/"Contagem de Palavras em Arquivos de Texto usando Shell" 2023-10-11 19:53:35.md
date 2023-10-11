Claro, vou criar um código complexo em Shell para exemplificar o uso de funções, estruturas de controle e manipulação de arquivos. Este código irá realizar o seguinte cenário:

Suponha que temos um diretório contendo vários arquivos de texto e queremos realizar a contagem de palavras presentes em cada um desses arquivos. Além disso, queremos gerar um relatório consolidado com o total de palavras em cada arquivo e o total geral de palavras em todos os arquivos.

```shell
#!/bin/bash

# Função para contar palavras em um arquivo
contar_palavras() {
    arquivo=$1
    palavras=$(cat $arquivo | wc -w)
    echo "O arquivo $arquivo possui $palavras palavra(s)"
}

# Diretório dos arquivos de texto
diretorio="/caminho/do/diretorio"

# Variável para armazenar o total de palavras em todos os arquivos
total_palavras=0

# Loop para percorrer todos os arquivos do diretório
for arquivo in $diretorio/*.txt; do
    contar_palavras $arquivo
    total_palavras=$((total_palavras + palavras))
done

# Exibindo o total de palavras em cada arquivo e o total geral
echo "Total de palavras em todos os arquivos: $total_palavras"
```

Explicação do código:

- A função `contar_palavras` recebe como parâmetro o caminho de um arquivo e utiliza o comando `cat` para exibir o conteúdo do arquivo, redirecionando-o para o comando `wc -w`, que conta o número de palavras. O resultado é armazenado na variável `palavras` e exibido com uma mensagem personalizada.

- A variável `diretorio` armazena o caminho do diretório onde estão os arquivos de texto.

- A variável `total_palavras` é inicializada com zero e será utilizada para armazenar o total de palavras em todos os arquivos.

- O loop `for` percorre todos os arquivos com extensão `.txt` presentes no diretório especificado. A cada iteração, chama a função `contar_palavras` passando o caminho do arquivo como argumento. Além disso, atualiza a variável `total_palavras` somando o número de palavras do arquivo atual.

- Ao final do loop, exibe o total de palavras em todos os arquivos utilizando a variável `total_palavras`.

Espero que este exemplo tenha sido útil para você entender o uso de funções, estruturas de controle e manipulação de arquivos em Shell.