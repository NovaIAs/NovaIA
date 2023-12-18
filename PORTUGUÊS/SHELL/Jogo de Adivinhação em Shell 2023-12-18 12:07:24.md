Claro! Aqui está um código complexo em Shell, feito especialmente para você. Ele consiste em um programa que simula um jogo de adivinhação, onde o jogador precisa acertar um número aleatório gerado pelo computador.

```shell
#!/bin/bash

# Função que gera um número aleatório entre 1 e 100
function gerar_numero_aleatorio() {
  echo $((RANDOM % 100 + 1))
}

# Função que verifica se o número digitado é igual ao número gerado
function verificar_palpite() {
  local palpite=$1
  local numero_gerado=$2

  if [[ $palpite -eq $numero_gerado ]]; then
    echo "Parabéns! Você acertou o número."
    exit
  elif [[ $palpite -lt $numero_gerado ]]; then
    echo "O número é maior do que $palpite."
  else
    echo "O número é menor do que $palpite."
  fi
}

echo "Bem-vindo ao jogo de adivinhação!"
echo "Tente adivinhar o número entre 1 e 100."

numero_gerado=$(gerar_numero_aleatorio)

while true; do
  read -p "Digite o seu palpite: " palpite

  if ! [[ $palpite =~ ^[0-9]+$ ]]; then
    echo "Por favor, digite apenas números."
    continue
  fi

  verificar_palpite $palpite $numero_gerado
done
```

Explicação do código:

1. A primeira linha `#!/bin/bash` especifica que o script deve ser executado com o interpretador bash.

2. A função `gerar_numero_aleatorio` utiliza o comando `RANDOM` para gerar um número aleatório entre 1 e 100.

3. A função `verificar_palpite` recebe o palpite do jogador e o número gerado como argumentos. Ela compara os valores e exibe uma mensagem indicando se o palpite é maior ou menor que o número gerado.

4. A partir da linha 12, o programa inicia exibindo a mensagem de boas-vindas e as instruções do jogo.

5. A variável `numero_gerado` recebe o resultado da chamada da função `gerar_numero_aleatorio`.

6. O loop `while true` mantém o jogo em execução até que o jogador acerte o número.

7. Dentro do loop, o comando `read` solicita ao jogador que digite o palpite.

8. A condição `if ! [[ $palpite =~ ^[0-9]+$ ]]` verifica se o palpite contém apenas números. Caso contrário, exibe uma mensagem de erro e continua para a próxima iteração do loop.

9. A função `verificar_palpite` é chamada passando o palpite e o número gerado como argumentos.

10. Se o palpite for igual ao número gerado, o programa exibe uma mensagem de parabéns e encerra.

11. Caso contrário, o programa exibe uma mensagem indicando se o palpite é maior ou menor que o número gerado.

Esse é um exemplo de um código complexo em Shell que implementa um jogo de adivinhação. Espero que ele atenda às suas expectativas!