Claro! Vou criar um código complexo em Python para você. Este código irá calcular e mostrar a quantidade de números primos em um intervalo definido pelo usuário. Vamos lá:

```python
def eh_primo(numero):
    if numero <= 1:
        return False
    elif numero <= 3:
        return True
    elif numero % 2 == 0 or numero % 3 == 0:
        return False
    i = 5
    while i * i <= numero:
        if numero % i == 0 or numero % (i + 2) == 0:
            return False
        i += 6
    return True

def contar_primos(inicio, fim):
    quantidade_primos = 0
    for numero in range(inicio, fim + 1):
        if eh_primo(numero):
            quantidade_primos += 1
    return quantidade_primos

inicio = int(input("Informe o número inicial do intervalo: "))
fim = int(input("Informe o número final do intervalo: "))

quantidade = contar_primos(inicio, fim)

print(f"A quantidade de números primos no intervalo de {inicio} a {fim} é: {quantidade}")
```

Explicação do código:

1. A função `eh_primo(numero)` é responsável por verificar se um número é primo ou não. Ela utiliza o algoritmo de verificação de primalidade conhecido como Crivo de Eratóstenes para otimizar a verificação. O algoritmo verifica se o número é divisível por algum número até sua raiz quadrada, pulando os números múltiplos de 2 e 3.

2. A função `contar_primos(inicio, fim)` recebe dois números, `inicio` e `fim`, que representam o intervalo no qual será contado a quantidade de números primos. Ela utiliza um loop `for` para iterar por todos os números do intervalo e chama a função `eh_primo(numero)` para verificar se cada número é primo. Caso seja, incrementa a variável `quantidade_primos`.

3. O usuário é solicitado a fornecer o número inicial e final do intervalo desejado.

4. A função `contar_primos(inicio, fim)` é chamada passando os números fornecidos pelo usuário. O resultado é armazenado na variável `quantidade`.

5. Por fim, é exibido na tela a quantidade de números primos no intervalo fornecido pelo usuário.

Espero que esse código atenda às suas expectativas!