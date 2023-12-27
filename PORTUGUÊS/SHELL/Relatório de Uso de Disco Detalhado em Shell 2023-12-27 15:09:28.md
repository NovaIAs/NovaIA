Claro! Aqui está um código em Shell que gera um relatório de uso de disco detalhado, exibindo informações sobre o uso de cada diretório e subdiretório presentes em um determinado caminho.

```shell
#!/bin/bash

# Função recursiva para obter o uso de disco de um diretório
function calcularUsoDisco() {
    local diretorio=$1
    local espacos=$2

    # Obter o tamanho total do diretório
    local tamanhoDiretorio=$(du -sh $diretorio | awk '{print $1}')

    # Imprimir informações sobre o diretório atual
    echo "${espacos}Diretório: $diretorio"
    echo "${espacos}Tamanho total: $tamanhoDiretorio"

    # Iterar sobre os subdiretórios
    local subdiretorios=$(ls -l $diretorio | grep '^d' | awk '{print $NF}')
    for subdiretorio in $subdiretorios
    do
        local caminhoSubdiretorio="$diretorio/$subdiretorio"
        local espacosSubdiretorio="${espacos}  "

        # Chamar recursivamente a função para o subdiretório
        calcularUsoDisco $caminhoSubdiretorio "$espacosSubdiretorio"
    done
}

# Solicitar o caminho do diretório
echo "Informe o caminho do diretório:"
read caminhoDiretorio

# Verificar se o diretório existe
if [ -d "$caminhoDiretorio" ]; then
    echo "Calculando uso de disco para o diretório: $caminhoDiretorio"
    calcularUsoDisco $caminhoDiretorio ""
else
    echo "O diretório não existe!"
fi
```

Explicação:

1. No início do código, definimos uma função chamada `calcularUsoDisco`, que é uma função recursiva responsável por calcular o uso de disco de um determinado diretório e seus subdiretórios.

2. Dentro da função `calcularUsoDisco`, recebemos dois parâmetros: o diretório a ser analisado e uma string contendo espaços para fins de indentação na exibição dos dados.

3. Usamos o comando `du -sh` para obter o tamanho total do diretório passado como parâmetro. A opção `-s` indica que queremos apenas o tamanho total do diretório, e a opção `-h` formata a saída para ser mais legível.

4. Imprimimos na tela informações sobre o diretório atual, incluindo o caminho, usando a variável `espacos` para garantir a indentação correta.

5. Em seguida, usamos o comando `ls -l` para listar os subdiretórios presentes no diretório atual. A opção `-l` indica que queremos uma listagem detalhada, e o filtro `^d` garante que a listagem retorne apenas os diretórios.

6. A saída do comando `ls -l` é filtrada pelo comando `grep '^d'` para manter apenas as linhas que começam com a letra "d", indicando que são diretórios. Usamos o comando `awk '{print $NF}'` para obter apenas o nome dos subdiretórios.

7. Em um loop `for`, iteramos sobre cada subdiretório encontrado e chamamos recursivamente a função `calcularUsoDisco` para o subdiretório atual. Passamos o caminho completo do subdiretório como parâmetro e incrementamos a string `espacos` para garantir a indentação correta na exibição dos dados.

8. No restante do código, solicitamos ao usuário que informe o caminho do diretório a ser analisado e verificamos se o diretório existe usando o comando `if [ -d "$caminhoDiretorio" ]; then`. Se o diretório existir, chamamos a função `calcularUsoDisco` para iniciar o cálculo do uso de disco. Caso contrário, exibimos uma mensagem informando que o diretório não existe.

Esse código em Shell é capaz de fornecer um relatório detalhado sobre o uso de disco de um diretório, incluindo informações sobre cada subdiretório presente no caminho informado.