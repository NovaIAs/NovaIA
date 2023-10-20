Com certeza! Aqui está um código complexo em Python que usa recursão para calcular o fatorial de um número:

```python
def fatorial(n):
    if n == 0:
        return 1
    else:
        return n * fatorial(n-1)

num = int(input("Digite um número inteiro: "))
resultado = fatorial(num)
print("O fatorial de", num, "é", resultado)
```

Neste código, a função `fatorial` é definida para calcular o fatorial de um número. Ela usa uma abordagem recursiva, o que significa que ela chama a si mesma para calcular o fatorial de um número menor. 

A função `fatorial` verifica se o número `n` é igual a zero. Se for, retorna 1, pois o fatorial de zero é 1. Caso contrário, calcula o fatorial de `n-1` multiplicado por `n`. Essa multiplicação é feita recursivamente até que `n` seja reduzido a zero.

Em seguida, o programa solicita ao usuário que digite um número inteiro. Esse número é armazenado na variável `num`. O programa chama a função `fatorial` passando o valor de `num` como argumento e armazena o resultado na variável `resultado`. Por fim, exibe o resultado do cálculo.

Esse código é complexo porque utiliza recursão, o que pode ser difícil de entender para algumas pessoas. Além disso, é flexível, pois pode calcular o fatorial de qualquer número inteiro não negativo.