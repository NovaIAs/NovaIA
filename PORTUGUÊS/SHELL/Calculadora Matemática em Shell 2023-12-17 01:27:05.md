Claro! Abaixo está um código complexo em Shell que realiza uma série de operações matemáticas com números inseridos pelo usuário, utilizando funções personalizadas:

```shell
#!/bin/bash

# Função para calcular o fatorial de um número
function fatorial() {
  local num=$1
  local result=1

  for ((i=1; i<=num; i++))
  do
    result=$((result * i))
  done

  echo $result
}

# Função para verificar se um número é primo
function verificar_primo() {
  local num=$1

  if [ $num -lt 2 ]
  then
    echo "Não é primo"
    return
  fi

  for ((i=2; i<=num/2; i++))
  do
    if [ $((num % i)) -eq 0 ]
    then
      echo "Não é primo"
      return
    fi
  done

  echo "É primo"
}

# Função para calcular o quadrado de um número
function calcular_quadrado() {
  local num=$1
  local result=$((num * num))

  echo $result
}

# Função para calcular a raiz quadrada de um número
function calcular_raiz_quadrada() {
  local num=$1
  local result=$(echo "scale=2; sqrt($num)" | bc)

  echo $result
}

# Função principal
function main() {
  echo "Digite um número:"
  read numero

  echo "Selecione uma operação:"
  echo "1. Calcular fatorial"
  echo "2. Verificar se é primo"
  echo "3. Calcular o quadrado"
  echo "4. Calcular a raiz quadrada"
  read operacao

  case $operacao in
    1)
      resultado=$(fatorial $numero)
      echo "O fatorial de $numero é: $resultado"
      ;;
    2)
      verificar_primo $numero
      ;;
    3)
      resultado=$(calcular_quadrado $numero)
      echo "O quadrado de $numero é: $resultado"
      ;;
    4)
      resultado=$(calcular_raiz_quadrada $numero)
      echo "A raiz quadrada de $numero é: $resultado"
      ;;
    *)
      echo "Opção inválida"
      ;;
  esac
}

main
```

Este código em Shell permite ao usuário inserir um número e escolher uma operação para ser realizada com esse número. As operações disponíveis são:

1. Calcular o fatorial do número
2. Verificar se o número é primo
3. Calcular o quadrado do número
4. Calcular a raiz quadrada do número

Cada operação é realizada por uma função personalizada. A função `fatorial` calcula o fatorial utilizando um loop `for`. A função `verificar_primo` verifica se um número é primo utilizando um loop `for` e o operador de módulo `%`. A função `calcular_quadrado` calcula o quadrado de um número simplesmente multiplicando o número por ele mesmo. A função `calcular_raiz_quadrada` calcula a raiz quadrada utilizando o comando `bc` do Bash.

No final, a função `main` é chamada para executar o programa. Ela solicita ao usuário um número e a operação desejada, e em seguida, utiliza um `case` para chamar a função correspondente à operação escolhida e exibir o resultado.

Espero que esse código atenda às suas necessidades!