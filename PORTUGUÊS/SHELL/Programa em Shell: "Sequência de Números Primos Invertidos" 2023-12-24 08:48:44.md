Claro! Aqui está um exemplo de um código complexo em Shell:

```shell
#!/bin/bash

# Função para imprimir uma mensagem formatada
imprimir_mensagem() {
  echo "==============================="
  echo "$1"
  echo "==============================="
}

# Função para verificar se um número é primo
verificar_primo() {
  local num=$1
  local primo=true
  for ((i=2; i<=num/2; i++)); do
    if ((num % i == 0)); then
      primo=false
      break
    fi
  done
  return $primo
}

# Função para gerar uma sequência de números primos
gerar_sequencia_primos() {
  local limite=$1
  local sequencia=""
  for ((num=2; num<=limite; num++)); do
    verificar_primo $num
    if [ $? -eq 0 ]; then
      sequencia+=" $num"
    fi
  done
  echo "$sequencia"
}

# Função para inverter uma string
inverter_string() {
  local string=$1
  local tamanho=${#string}
  local invertida=""
  for ((i=tamanho-1; i>=0; i--)); do
    invertida+=${string:i:1}
  done
  echo "$invertida"
}

# Função principal
main() {
  imprimir_mensagem "Sequência de números primos invertidos"

  read -p "Digite o limite máximo: " limite

  sequencia=$(gerar_sequencia_primos $limite)
  invertida=$(inverter_string "$sequencia")

  imprimir_mensagem "Sequência de números primos: $sequencia"
  imprimir_mensagem "Sequência invertida: $invertida"
}

# Chamar a função principal
main
```

Este código em Shell é um exemplo de um programa que gera uma sequência de números primos até um limite máximo fornecido pelo usuário e, em seguida, inverte essa sequência.

O programa começa definindo uma função chamada `imprimir_mensagem`, que recebe uma mensagem como parâmetro e a exibe formatada na saída padrão. Essa função é utilizada para exibir mensagens informativas ao longo do programa.

A próxima função é a `verificar_primo`, que recebe um número como parâmetro e verifica se ele é primo. A função utiliza um laço `for` para testar se o número é divisível por algum outro número até a sua metade. Se for encontrado um divisor, a função define a variável local `primo` como `false`. Caso contrário, a variável permanece como `true`. No final, a função retorna o valor da variável `primo` como código de retorno.

A função `gerar_sequencia_primos` recebe um limite máximo como parâmetro e utiliza um laço `for` para percorrer todos os números de 2 até o limite. A cada iteração, a função chama a função `verificar_primo` para verificar se o número é primo. Se for primo, o número é adicionado à sequência de números primos na variável `sequencia`. No final, a função retorna a sequência.

A função `inverter_string` recebe uma string como parâmetro e utiliza um laço `for` para percorrer a string de trás para frente. A cada iteração, a função adiciona o caracter atual à string `invertida`. No final, a função retorna a string invertida.

A função principal `main` começa chamando a função `imprimir_mensagem` para exibir uma mensagem de boas-vindas. Em seguida, solicita ao usuário que digite o limite máximo para a geração da sequência de números primos. O valor digitado é passado como argumento para a função `gerar_sequencia_primos`, que retorna a sequência de números primos. Essa sequência é armazenada na variável `sequencia` e, em seguida, é chamada a função `inverter_string` para inverter a sequência. O resultado é armazenado na variável `invertida`.

Por fim, a função `imprimir_mensagem` é chamada novamente para exibir a sequência de números primos e a sequência invertida.

Espero que este exemplo atenda às suas expectativas!