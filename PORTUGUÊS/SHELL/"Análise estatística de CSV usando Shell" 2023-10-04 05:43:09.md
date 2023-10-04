Certamente! Aqui está um exemplo de um código complexo em Shell, que faz a leitura de um arquivo CSV, realiza algumas operações matemáticas e gera um relatório com os resultados:

```shell
#!/bin/bash

# Função para calcular a média de um conjunto de números
calcular_media() {
    local soma=0
    local contador=0

    # Loop para percorrer os números no arquivo CSV
    while IFS=',' read -r _ numero; do
        soma=$((soma + numero))
        contador=$((contador + 1))
    done < $1

    media=$(bc <<< "scale=2; $soma / $contador")
    echo "Média: $media"
}

# Função para calcular o desvio padrão de um conjunto de números
calcular_desvio_padrao() {
    local soma=0
    local contador=0
    local media=$2

    # Loop para percorrer os números no arquivo CSV
    while IFS=',' read -r _ numero; do
        soma=$(bc <<< "scale=2; $soma + ($numero - $media)^2")
        contador=$((contador + 1))
    done < $1

    desvio_padrao=$(bc <<< "scale=2; sqrt($soma / $contador)")
    echo "Desvio Padrão: $desvio_padrao"
}

# Função para gerar o relatório com os resultados
gerar_relatorio() {
    local arquivo=$1
    local media=$(calcular_media $arquivo)
    local desvio_padrao=$(calcular_desvio_padrao $arquivo $media)

    echo "Relatório:"
    echo "Arquivo CSV: $arquivo"
    echo "---------------------"
    echo "$media"
    echo "$desvio_padrao"
}

# Chamada da função para gerar o relatório
gerar_relatorio "dados.csv"
```

Explicação do código:

1. Primeiro, definimos duas funções: `calcular_media` e `calcular_desvio_padrao`. Cada uma dessas funções recebe o nome de um arquivo CSV como parâmetro.

2. A função `calcular_media` realiza um loop para percorrer os números no arquivo CSV e calcular a média. Ela usa as variáveis `soma` e `contador` para armazenar a soma dos números e o número de elementos, respectivamente.

3. A função `calcular_desvio_padrao` também realiza um loop para percorrer os números no arquivo CSV e calcular o desvio padrão. Ela utiliza as variáveis `soma`, `contador` e `media`. A variável `media` é passada como parâmetro, pois já foi calculada anteriormente.

4. Em seguida, temos a função `gerar_relatorio`, que recebe o nome de um arquivo CSV como parâmetro. Ela chama as funções `calcular_media` e `calcular_desvio_padrao` para obter a média e o desvio padrão, respectivamente.

5. Por fim, a função `gerar_relatorio` exibe o relatório com os resultados, incluindo o nome do arquivo CSV, a média e o desvio padrão.

6. No final do código, há uma chamada da função `gerar_relatorio` com o nome do arquivo CSV "dados.csv". Você pode substituir esse nome pelo nome do arquivo que deseja utilizar.

Espero que este código atenda às suas expectativas!