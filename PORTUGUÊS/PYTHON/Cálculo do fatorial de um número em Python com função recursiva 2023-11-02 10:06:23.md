Claro! Vou criar um código em Python que irá calcular o fatorial de um número utilizando uma função recursiva. Essa função irá receber um número como parâmetro e retornar o fatorial desse número.

```python
def fatorial(n):
    if n == 0:
        return 1
    else:
        return n * fatorial(n-1)

num = int(input("Digite um número: "))
resultado = fatorial(num)
print("O fatorial de", num, "é:", resultado)
```

Nesse código, temos uma função chamada `fatorial`, que recebe um parâmetro `n`. Dentro dessa função, temos uma condição de parada, onde se o valor de `n` for igual a zero, a função retorna 1, pois sabemos que o fatorial de 0 é igual a 1.

Caso contrário, a função chama a si mesma passando como parâmetro o valor de `n` decrementado em 1. Essa chamada recursiva irá calcular o fatorial do número anterior, e assim por diante, até chegar ao caso base.

Após a definição da função, o programa solicita ao usuário que digite um número através da função `input` e converte esse valor para inteiro utilizando `int`. Em seguida, é chamada a função `fatorial` passando o número digitado como parâmetro, e o resultado é armazenado na variável `resultado`.

Por fim, o programa exibe na tela a mensagem com o número digitado e o resultado do fatorial utilizando a função `print`.