Claro! Aqui está um código complexo em Shell que realiza a transformação de uma frase em um acrônimo:

```shell
#!/bin/bash

read -p "Digite uma frase: " frase

acronimo=""

IFS=' ' read -ra palavras <<< "$frase"

for palavra in "${palavras[@]}"; do
  acronimo+="${palavra:0:1}"
done

echo "O acrônimo da frase '$frase' é: $acronimo"
```

Explicação do código:

1. O usuário é solicitado a digitar uma frase.
2. A variável `acronimo` é inicializada como uma string vazia.
3. A função `read` é usada para ler a frase digitada pelo usuário e armazená-la na variável `frase`.
4. Utilizando o comando `IFS`, definimos o caractere separador como espaço.
5. A frase é dividida em palavras e armazenadas no array `palavras`.
6. Em seguida, percorremos cada palavra no array `palavras`.
7. Para cada palavra, adicionamos o primeiro caractere ao acrônimo usando a sintaxe `${palavra:0:1}`.
8. Por fim, exibimos o acrônimo resultante.
9. O script é finalizado.

Espero que esse código complexo atenda às suas expectativas!