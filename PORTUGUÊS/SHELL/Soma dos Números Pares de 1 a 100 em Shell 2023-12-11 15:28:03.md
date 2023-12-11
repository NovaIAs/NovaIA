Claro! Aqui está um código em Shell que utiliza loops e estruturas condicionais para calcular a soma dos números pares de 1 a 100 e exibir o resultado:

```shell
#!/bin/bash

# Inicializa a variável de soma
soma=0

# Loop de 1 a 100
for ((i=1; i<=100; i++))
do
  # Verifica se o número é par
  if (( i % 2 == 0 ))
  then
    # Atualiza a soma
    soma=$((soma + i))
  fi
done

# Exibe o resultado
echo "A soma dos números pares de 1 a 100 é: $soma"
```

Explicação do código:

1. A linha `#!/bin/bash` é chamada de shebang e indica ao sistema operacional que este é um script em Shell e deve ser executado pelo interpretador `bash`.
2. A linha `soma=0` inicializa a variável `soma` com o valor zero.
3. O loop `for ((i=1; i<=100; i++))` é utilizado para percorrer os números de 1 a 100.
4. Dentro do loop, a linha `if (( i % 2 == 0 ))` verifica se o número `i` é par através do operador módulo `%`. Se o resultado da divisão de `i` por 2 for igual a zero, então o número é par.
5. Caso o número seja par, a linha `soma=$((soma + i))` atualiza a variável `soma` somando o valor de `i`.
6. Após o término do loop, a linha `echo "A soma dos números pares de 1 a 100 é: $soma"` exibe o resultado final.

Este código percorre todos os números de 1 a 100, verifica se cada número é par e, se for, adiciona-o à variável `soma`. No final, o código exibe a soma dos números pares encontrados.