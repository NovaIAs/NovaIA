Claro, aqui está um código em SHELL que utiliza loops e condicionais para encontrar os números primos menores ou iguais a um determinado número fornecido pelo usuário:

```
#!/bin/bash

echo "Digite um número: "
read n

# Inicializa um contador i
i=2

echo "Números primos menores ou iguais a $n:"

# Loop enquanto i for menor ou igual ao número fornecido pelo usuário
while [ $i -le $n ]
do
    # Inicializa um contador j
    j=2
    is_prime=1

    # Loop enquanto j for menor ou igual a i/2
    while [ $j -le $(($i / 2)) ]
    do
        # Se i for divisível por j, não é um número primo
        if [ $(($i % $j)) -eq 0 ]
        then
            is_prime=0
            break
        fi
        j=$(($j + 1))
    done

    # Se is_prime for 1, i é um número primo e é impresso na tela
    if [ $is_prime -eq 1 ]
    then
        echo $i
    fi

    i=$(($i + 1))
done
```

Explicação do código:

O código começa solicitando ao usuário que digite um número. Esse número é armazenado na variável `n`.

O contador `i` é inicializado em 2, pois 1 não é um número primo.

O código entra em um loop while que executa enquanto `i` for menor ou igual a `n`.

Dentro do loop, um novo contador `j` é inicializado em 2.

A variável `is_prime` é inicializada em 1, pois assumimos que `i` é um número primo até que seja provado o contrário.

O código entra em um segundo loop while que executa enquanto `j` for menor ou igual a `i/2`. Isso é feito para reduzir a quantidade de divisões que precisamos fazer. Se um número não for divisível por nenhum número menor ou igual a metade dele mesmo, então também não será divisível por um número maior que a metade dele mesmo.

Dentro do segundo loop, verificamos se `i` é divisível por `j`. Se for, definimos `is_prime` como 0 e interrompemos o loop utilizando o comando `break`.

Se `is_prime` for igual a 1, isso significa que `i` é um número primo e o imprimimos na tela.

Finalmente, incrementamos `i` em 1 e o loop continua até que `i` seja maior que `n`.

Esse código é capaz de encontrar todos os números primos menores ou iguais a qualquer número fornecido pelo usuário e pode ser utilizado para diversos fins, como análise de dados ou criptografia.