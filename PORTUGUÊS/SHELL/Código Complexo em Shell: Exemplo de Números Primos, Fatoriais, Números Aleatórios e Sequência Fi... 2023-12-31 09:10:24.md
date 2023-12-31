Claro! Aqui está um exemplo de um código complexo em Shell que realiza uma série de tarefas diferentes:

```bash
#!/bin/bash

# Função para verificar se um número é primo
function isPrime() {
    local num=$1
    local prime=true

    if [ $num -le 1 ]; then
        prime=false
    fi

    for (( i=2; i<$num; i++ )); do
        if [ $(($num % $i)) -eq 0 ]; then
            prime=false
            break
        fi
    done

    return $prime
}

# Função para calcular o fatorial de um número
function factorial() {
    local num=$1
    local result=1

    for (( i=1; i<=$num; i++ )); do
        result=$(($result * $i))
    done

    echo $result
}

# Função para imprimir os números primos e seus fatoriais na faixa de 1 a 50
function printPrimesAndFactorials() {
    for (( num=1; num<=50; num++ )); do
        isPrime $num
        if [ $? = true ]; then
            echo "Número Primo: $num"
            echo "Fatorial: $(factorial $num)"
            echo
        fi
    done
}

# Função para gerar um número aleatório entre dois valores
function generateRandomNumber() {
    local min=$1
    local max=$2

    echo $(( (RANDOM % ($max - $min + 1)) + $min ))
}

# Função para imprimir a soma dos números pares de 1 a 100
function printSumOfEvenNumbers() {
    local sum=0

    for (( num=1; num<=100; num++ )); do
        if [ $(($num % 2)) -eq 0 ]; then
            sum=$(($sum + $num))
        fi
    done

    echo "Soma dos números pares de 1 a 100: $sum"
}

# Função para imprimir a sequência de Fibonacci até um número específico
function printFibonacciSequence() {
    local limit=$1
    local a=0
    local b=1

    echo "Sequência de Fibonacci até $limit:"

    while [ $a -le $limit ]; do
        echo $a
        temp=$a
        a=$b
        b=$(($temp + $b))
    done
}

# Chamada das funções
printPrimesAndFactorials
echo
echo "Número Aleatório entre 10 e 20: $(generateRandomNumber 10 20)"
echo
printSumOfEvenNumbers
echo
printFibonacciSequence 100
```

Explicação do código:

1. A função `isPrime` verifica se um número é primo. Ela itera de 2 até o número anterior ao número dado e verifica se o número é divisível por algum desses valores. Se for, o número não é primo.
2. A função `factorial` calcula o fatorial de um número. Ela itera de 1 até o número dado e multiplica cada valor pelo resultado atual.
3. A função `printPrimesAndFactorials` itera de 1 a 50 e para cada número, verifica se é primo usando a função `isPrime`. Se for primo, imprime o número e o seu fatorial usando a função `factorial`.
4. A função `generateRandomNumber` gera um número aleatório entre dois valores dados usando a função `RANDOM` do shell.
5. A função `printSumOfEvenNumbers` itera de 1 a 100 e para cada número par, soma-o ao resultado atual.
6. A função `printFibonacciSequence` imprime a sequência de Fibonacci até um número limite dado. A sequência começa com os valores 0 e 1, e para cada iteração, o próximo número é a soma dos dois anteriores.

No final do código, as funções são chamadas para demonstrar o funcionamento de cada uma delas.