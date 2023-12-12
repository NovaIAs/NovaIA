Claro! Aqui está um código complexo em Python que realiza o cálculo de Fibonacci com uma abordagem de programação dinâmica para otimizar o desempenho:

```python
def fibonacci(n):
    # Verifica se o número já foi calculado anteriormente
    if n in fib_cache:
        return fib_cache[n]
    
    # Calcula o número de Fibonacci para n
    if n == 0:
        fib_cache[0] = 0
        return 0
    elif n == 1:
        fib_cache[1] = 1
        return 1
    else:
        fib_cache[n] = fibonacci(n-1) + fibonacci(n-2)
        return fib_cache[n]

# Dicionário para armazenar os números de Fibonacci já calculados
fib_cache = {}

# Solicita ao usuário um número para calcular o Fibonacci
numero = int(input("Digite um número para calcular o Fibonacci: "))

# Chama a função fibonacci para calcular o Fibonacci do número informado
resultado = fibonacci(numero)

# Exibe o resultado
print(f"O Fibonacci de {numero} é {resultado}.")
```

Neste código, utilizamos uma abordagem de programação dinâmica para otimizar o cálculo do número de Fibonacci. A função `fibonacci(n)` recebe como parâmetro um número `n` e verifica se o número já foi calculado anteriormente, utilizando um dicionário `fib_cache` para armazenar os números de Fibonacci já calculados.

Se o número já estiver no cache, o valor correspondente é retornado diretamente. Caso contrário, o cálculo é feito recursivamente utilizando a fórmula do Fibonacci: `fibonacci(n) = fibonacci(n-1) + fibonacci(n-2)`. Os resultados são armazenados no cache para reutilização futura.

No final, o usuário é solicitado a digitar um número para calcular o Fibonacci. O resultado é calculado chamando a função `fibonacci(numero)` e exibido na tela.