Claro! Aqui está um exemplo de código complexo em Python. Este código é um programa que gera sequências de Fibonacci até um determinado número fornecido pelo usuário:

```python
def fibonacci(n):
    if n <= 0:
        return "Número inválido"
    elif n == 1:
        return [0]
    elif n == 2:
        return [0, 1]
    else:
        fib_seq = [0, 1]
        while len(fib_seq) < n:
            next_fib = fib_seq[-1] + fib_seq[-2]
            fib_seq.append(next_fib)
        return fib_seq

def main():
    try:
        numero = int(input("Digite um número inteiro positivo: "))
        sequencia = fibonacci(numero)
        print(f"A sequência de Fibonacci até o {numero}º número é:")
        print(sequencia)
    except ValueError:
        print("Entrada inválida. Por favor, digite um número inteiro positivo.")

if __name__ == "__main__":
    main()
```

Neste código, temos duas funções principais: `fibonacci()` e `main()`. A função `fibonacci()` recebe um número inteiro `n` como parâmetro e retorna uma lista contendo a sequência de Fibonacci até o `n`-ésimo número. A função `main()` é responsável por receber a entrada do usuário, chamar a função `fibonacci()` e exibir o resultado na tela.

Na função `fibonacci()`, primeiro verificamos se o número fornecido é inválido (menor ou igual a zero) e retornamos uma mensagem de erro. Em seguida, tratamos os casos base em que `n` é igual a 1 ou 2, retornando a sequência inicial da série de Fibonacci.

Caso `n` seja maior que 2, inicializamos uma lista `fib_seq` com os dois primeiros números da sequência (0 e 1). Através de um loop while, continuamos a adicionar novos números à lista `fib_seq` até que seu tamanho seja igual a `n`. Cada novo número é calculado somando os dois últimos números da lista `fib_seq` e adicionado à lista.

A função `main()` lida com a entrada do usuário, verificando se um número inteiro positivo foi fornecido. Em seguida, chama a função `fibonacci()` passando o número digitado como argumento e exibe a sequência resultante na tela.

Por fim, usando `if __name__ == "__main__":`, garantimos que o bloco de código dentro dele só será executado se o arquivo Python for executado diretamente, evitando que seja executado se o arquivo for importado como um módulo em outro código.