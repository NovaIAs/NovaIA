Claro! Vou gerar um código complexo em Shell para você. Este código tem como objetivo realizar a análise de um arquivo de texto e gerar um relatório com informações estatísticas sobre o conteúdo do arquivo. Vou dividir o código em diferentes etapas para melhor compreensão.

Etapa 1: Verificar a existência do arquivo

```bash
#!/bin/bash

# Verifica se o arquivo de entrada foi fornecido como argumento
if [ $# -eq 0 ]; then
    echo "Informe o arquivo de entrada como argumento."
    exit 1
fi

# Verifica se o arquivo de entrada existe
if [ ! -f "$1" ]; then
    echo "O arquivo $1 não existe."
    exit 1
fi

# Define o nome do arquivo de entrada
input_file="$1"
```

Nesta etapa, o código verifica se um arquivo de entrada foi fornecido como argumento e se ele realmente existe. Caso contrário, exibe uma mensagem de erro e encerra a execução do programa. O nome do arquivo de entrada é armazenado na variável `input_file`.

Etapa 2: Contagem de palavras

```bash
# Conta o número total de palavras no arquivo
total_words=$(wc -w < "$input_file")

# Exibe o número total de palavras
echo "Número total de palavras: $total_words"
```

Nesta etapa, o código utiliza o comando `wc` para contar o número total de palavras no arquivo de entrada. O resultado é armazenado na variável `total_words` e exibido na tela.

Etapa 3: Contagem de linhas

```bash
# Conta o número total de linhas no arquivo
total_lines=$(wc -l < "$input_file")

# Exibe o número total de linhas
echo "Número total de linhas: $total_lines"
```

Nesta etapa, o código utiliza novamente o comando `wc` para contar o número total de linhas no arquivo de entrada. O resultado é armazenado na variável `total_lines` e exibido na tela.

Etapa 4: Contagem de caracteres

```bash
# Conta o número total de caracteres no arquivo
total_chars=$(wc -m < "$input_file")

# Exibe o número total de caracteres
echo "Número total de caracteres: $total_chars"
```

Nesta etapa, o código utiliza o comando `wc` para contar o número total de caracteres no arquivo de entrada. O resultado é armazenado na variável `total_chars` e exibido na tela.

Etapa 5: Contagem de ocorrências de cada palavra

```bash
# Cria um array associativo para armazenar as ocorrências de cada palavra
declare -A word_count

# Lê cada palavra do arquivo e incrementa o contador correspondente
while read -r word; do
    ((word_count[$word]++))
done < "$input_file"

# Exibe a contagem de ocorrências de cada palavra
echo "Contagem de ocorrências de cada palavra:"
for word in "${!word_count[@]}"; do
    echo "$word: ${word_count[$word]}"
done
```

Nesta etapa, o código cria um array associativo chamado `word_count` para armazenar as ocorrências de cada palavra no arquivo. Em seguida, lê cada palavra do arquivo e incrementa o contador correspondente no array. Por fim, exibe a contagem de ocorrências de cada palavra na tela.

Etapa 6: Geração do relatório

```bash
# Gera o relatório com as informações estatísticas
report_file="relatorio.txt"

echo "Relatório de análise do arquivo $input_file" > "$report_file"
echo "---------------------------------------------" >> "$report_file"
echo "Número total de palavras: $total_words" >> "$report_file"
echo "Número total de linhas: $total_lines" >> "$report_file"
echo "Número total de caracteres: $total_chars" >> "$report_file"
echo "" >> "$report_file"
echo "Contagem de ocorrências de cada palavra:" >> "$report_file"
for word in "${!word_count[@]}"; do
   echo "$word: ${word_count[$word]}" >> "$report_file"
done

echo "Relatório gerado com sucesso: $report_file"
```

Nesta etapa, o código gera um relatório em um arquivo chamado "relatorio.txt". O relatório inclui todas as informações estatísticas coletadas anteriormente, como o número total de palavras, linhas e caracteres, além da contagem de ocorrências de cada palavra. O nome do arquivo de relatório é exibido na tela como confirmação.

Espero que este código atenda às suas expectativas!