Claro! Aqui está um código complexo em Shell que realiza uma tarefa específica: listar todos os arquivos em um diretório e calcular o tamanho total dos arquivos encontrados.

```shell
#!/bin/bash

# Função para calcular o tamanho do diretório
calcular_tamanho() {
    local diretorio=$1
    local tamanho=0

    # Loop pelos arquivos e diretórios dentro do diretório fornecido
    for item in "$diretorio"/*; do
        if [ -f "$item" ]; then
            # Se for um arquivo, adiciona o tamanho ao total
            tamanho=$((tamanho + $(stat -c%s "$item")))
        elif [ -d "$item" ]; then
            # Se for um diretório, chama a função recursivamente
            tamanho=$((tamanho + $(calcular_tamanho "$item")))
        fi
    done

    echo "$tamanho"
}

# Diretório a ser verificado
diretorio="/caminho/do/diretorio"

# Verifica se o diretório existe
if [ -d "$diretorio" ]; then
    # Chama a função para calcular o tamanho
    tamanho_total=$(calcular_tamanho "$diretorio")

    # Exibe o resultado
    echo "O tamanho total do diretório $diretorio é de $tamanho_total bytes."
else
    echo "O diretório $diretorio não existe."
fi
```

Este código começa definindo uma função chamada `calcular_tamanho`, que recebe um diretório como parâmetro e retorna o tamanho total desse diretório, incluindo todos os arquivos e subdiretórios.

Em seguida, definimos a variável `diretorio` com o caminho do diretório a ser verificado.

O código então verifica se o diretório existe usando um condicional `if [ -d "$diretorio" ]`. Se o diretório existir, a função `calcular_tamanho` é chamada com o parâmetro do diretório fornecido.

Dentro da função `calcular_tamanho`, usamos um loop `for` para percorrer todos os itens dentro do diretório. Se o item for um arquivo (`if [ -f "$item" ]`), adicionamos o tamanho do arquivo ao total usando o comando `stat -c%s "$item"`, que retorna o tamanho do arquivo em bytes.

Se o item for um diretório (`elif [ -d "$item" ]`), chamamos recursivamente a função `calcular_tamanho` passando o diretório como parâmetro e adicionamos o tamanho retornado ao total.

Finalmente, exibimos o resultado com `echo "O tamanho total do diretório $diretorio é de $tamanho_total bytes."`.

Caso o diretório não exista, exibimos a mensagem "O diretório $diretorio não existe."

Espero que este código atenda às suas necessidades!