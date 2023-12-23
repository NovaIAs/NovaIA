Certamente! Aqui está um código complexo em Shell que utiliza uma série de comandos e estruturas para realizar uma tarefa específica. Este código irá ler um arquivo de texto, contar a ocorrência de cada palavra e exibir as palavras em ordem decrescente de frequência.

```bash
#!/bin/bash

# Verifica se o arquivo de entrada foi informado
if [ $# -eq 0 ]; then
  echo "Uso: $0 arquivo_de_texto"
  exit 1
fi

# Armazena o nome do arquivo passado como argumento
arquivo=$1

# Verifica se o arquivo existe
if [ ! -f "$arquivo" ]; then
  echo "O arquivo '$arquivo' não existe."
  exit 1
fi

# Lê as palavras do arquivo, separadas por espaço ou quebra de linha
palavras=$(cat "$arquivo" | tr -s '[:space:]' '\n' | tr -s '[:punct:]' ' ' | tr '[:upper:]' '[:lower:]')

# Cria um array associativo para armazenar a contagem de cada palavra
declare -A contagem

# Percorre todas as palavras e incrementa sua contagem no array
for palavra in $palavras; do
  contagem[$palavra]=$(( ${contagem[$palavra]} + 1 ))
done

# Ordena as palavras com base na contagem em ordem decrescente
palavras_ordenadas=$(for palavra in "${!contagem[@]}"; do
                     echo "${contagem[$palavra]} $palavra"
                   done | sort -rn)

# Exibe as palavras em ordem decrescente de frequência
echo "Contagem de palavras em '$arquivo':"
echo "$palavras_ordenadas"
```

Explicação do código:
1. O código começa verificando se um arquivo de texto foi informado como argumento na linha de comando. Caso contrário, exibe uma mensagem de uso e encerra o programa.
2. Em seguida, o nome do arquivo é armazenado na variável `arquivo`.
3. É feita uma verificação para garantir que o arquivo exista. Caso contrário, exibe uma mensagem de erro e encerra o programa.
4. O conteúdo do arquivo é lido e todas as palavras são armazenadas na variável `palavras`. O comando `tr` é utilizado para substituir espaços e pontuações por quebras de linha, garantindo que cada palavra seja tratada individualmente.
5. Um array associativo chamado `contagem` é criado para armazenar a contagem de cada palavra.
6. Um loop percorre todas as palavras e incrementa sua contagem no array `contagem`.
7. Após a contagem ser feita, as palavras são ordenadas em ordem decrescente de frequência. O loop `for` extrai a contagem e a palavra de cada item no array `contagem`, e em seguida, o comando `sort -rn` é utilizado para ordenar a lista com base na contagem, em ordem decrescente.
8. Por fim, a contagem de palavras é exibida na saída padrão.

Espero que este código atenda às suas expectativas! Lembre-se de que você pode personalizá-lo ou adaptá-lo de acordo com suas necessidades específicas.