Claro! Vou criar um código complexo em Shell Script para você. Este código irá realizar a leitura de um arquivo de texto, contar a ocorrência de cada palavra e exibir as 10 palavras mais frequentes. Vou explicar passo a passo o que cada parte do código faz:

```shell
#!/bin/bash

# Definindo o arquivo de entrada
arquivo="texto.txt"

# Fazendo a leitura do arquivo e armazenando as palavras em um array
mapfile -t palavras < <(grep -oE '\w+' "$arquivo")

# Criando um array associativo para contar a ocorrência de cada palavra
declare -A contador

# Percorrendo o array de palavras e incrementando o contador de cada palavra
for palavra in "${palavras[@]}"; do
  ((contador[$palavra]++))
done

# Ordenando as palavras em ordem decrescente de ocorrência
ordenado=$(for palavra in "${!contador[@]}"; do
  printf "%d %s\n" "${contador[$palavra]}" "$palavra"
done | sort -rn | head -n 10)

# Exibindo as 10 palavras mais frequentes
echo "As 10 palavras mais frequentes são:"
echo "$ordenado"
```

Agora vou explicar cada parte do código:

1. `#!/bin/bash`: Essa linha indica que o script será executado pelo interpretador bash.

2. `arquivo="texto.txt"`: Nesta linha, definimos o nome do arquivo de texto que será lido. Você pode alterar o nome do arquivo conforme necessário.

3. `mapfile -t palavras < <(grep -oE '\w+' "$arquivo")`: Essa linha lê o arquivo de texto e extrai todas as palavras, armazenando-as em um array chamado "palavras". O comando `grep -oE '\w+' "$arquivo"` faz a busca por palavras no arquivo.

4. `declare -A contador`: Aqui, criamos um array associativo chamado "contador" para armazenar a contagem de ocorrências de cada palavra.

5. `for palavra in "${palavras[@]}"; do ((contador[$palavra]++)); done`: Este trecho do código percorre o array de palavras e incrementa o contador de cada palavra no array "contador".

6. `ordenado=$(for palavra in "${!contador[@]}"; do printf "%d %s\n" "${contador[$palavra]}" "$palavra"; done | sort -rn | head -n 10)`: Nesta linha, criamos uma variável chamada "ordenado" que irá armazenar as 10 palavras mais frequentes. Utilizamos um loop para percorrer o array "contador" e imprimir cada palavra com sua contagem. Em seguida, usamos os comandos `sort -rn` para ordenar as palavras em ordem decrescente de ocorrência e `head -n 10` para obter apenas as 10 primeiras palavras.

7. `echo "As 10 palavras mais frequentes são:"`: Exibimos uma mensagem indicando o resultado que será exibido.

8. `echo "$ordenado"`: Por fim, exibimos as 10 palavras mais frequentes armazenadas na variável "ordenado".

Espero que esse código atenda ao que você deseja!