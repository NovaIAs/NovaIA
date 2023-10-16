Claro! Abaixo está um código em Shell que realiza o seguinte:

1. Cria um diretório chamado "meu_diretorio".
2. Entra no diretório "meu_diretorio".
3. Cria 10 subdiretórios dentro de "meu_diretorio", numerados de 1 a 10.
4. Gera um arquivo chamado "numeros.txt" dentro de cada subdiretório.
5. Escreve neste arquivo os números de 1 a 100, um número por linha.
6. Faz a leitura de todos os arquivos "numeros.txt" e soma todos os números presentes neles.
7. Exibe a soma total dos números.

Aqui está o código:

```shell
#!/bin/bash

# Cria o diretório "meu_diretorio"
mkdir meu_diretorio

# Entra no diretório "meu_diretorio"
cd meu_diretorio

# Loop para criar os subdiretórios de 1 a 10
for i in {1..10}; do
  # Cria o subdiretório
  mkdir $i

  # Entra no subdiretório
  cd $i

  # Gera o arquivo "numeros.txt"
  touch numeros.txt

  # Loop para escrever os números de 1 a 100 no arquivo
  for j in {1..100}; do
    echo $j >> numeros.txt
  done

  # Retorna para o diretório pai
  cd ..
done

# Variável para armazenar a soma dos números
soma=0

# Loop para ler e somar os números de todos os arquivos "numeros.txt"
for arquivo in $(find . -type f -name "numeros.txt"); do
  # Lê cada número do arquivo e adiciona à soma
  while IFS= read -r linha; do
    soma=$((soma + linha))
  done < "$arquivo"
done

# Exibe a soma total dos números
echo "A soma dos números é: $soma"
```

Espero que isso atenda às suas necessidades! Se você precisar de mais ajuda ou tiver alguma dúvida, estou à disposição.