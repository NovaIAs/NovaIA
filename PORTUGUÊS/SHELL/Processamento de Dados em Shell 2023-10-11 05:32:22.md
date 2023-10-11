Claro! Aqui está um código complexo em shell que realiza uma tarefa de processamento de dados. Ele lê um arquivo de entrada, realiza algumas transformações e gera um arquivo de saída com o resultado.

```
#!/bin/bash
# Script para processamento de dados

# Definindo o nome do arquivo de entrada
input_file="dados.txt"

# Definindo o nome do arquivo de saída
output_file="resultado.txt"

# Checando se o arquivo de entrada existe
if [ ! -f "$input_file" ]; then
  echo "Arquivo de entrada não encontrado."
  exit 1
fi

# Criando um array vazio para armazenar os dados processados
result=()

# Lendo o arquivo de entrada linha por linha
while IFS= read -r line; do
  # Removendo espaços em branco no início e fim da linha
  line=$(echo "$line" | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')

  # Transformando todas as letras em maiúsculas
  line=$(echo "$line" | tr '[:lower:]' '[:upper:]')

  # Adicionando a linha processada ao array result
  result+=("$line")
done < "$input_file"

# Verificando se o array result está vazio
if [ ${#result[@]} -eq 0 ]; then
  echo "Nenhum dado encontrado no arquivo de entrada."
  exit 1
fi

# Ordenando o array result em ordem alfabética
sorted_result=($(printf '%s\n' "${result[@]}" | sort))

# Criando o arquivo de saída e escrevendo os dados processados nele
echo "Resultado do processamento:" > "$output_file"
for line in "${sorted_result[@]}"; do
  echo "$line" >> "$output_file"
done

echo "Processamento concluído. O resultado foi salvo em $output_file."
```

Explicação do código:

1. O script começa definindo o nome do arquivo de entrada e o nome do arquivo de saída.
2. Em seguida, o script verifica se o arquivo de entrada existe. Se o arquivo não for encontrado, exibe uma mensagem de erro e encerra a execução.
3. Um array vazio chamado "result" é criado para armazenar os dados processados.
4. O script lê o arquivo de entrada linha por linha usando um loop "while".
5. Em cada linha, os espaços em branco no início e fim da linha são removidos usando o comando "sed".
6. Todas as letras da linha são transformadas em maiúsculas usando o comando "tr".
7. A linha processada é adicionada ao array "result".
8. Após o loop, o script verifica se o array "result" está vazio. Se estiver vazio, exibe uma mensagem de erro e encerra a execução.
9. O array "result" é ordenado em ordem alfabética usando o comando "sort" e armazenado no array "sorted_result".
10. O arquivo de saída é criado e a linha de cabeçalho é escrita nele.
11. Cada linha do array "sorted_result" é escrita no arquivo de saída.
12. Por fim, o script exibe uma mensagem de conclusão e informa o nome do arquivo de saída onde o resultado foi salvo.

Este código realiza várias etapas de processamento de dados, desde a leitura do arquivo de entrada até a geração do arquivo de saída com os dados processados. É um exemplo de código complexo que demonstra o uso de várias funcionalidades do shell.