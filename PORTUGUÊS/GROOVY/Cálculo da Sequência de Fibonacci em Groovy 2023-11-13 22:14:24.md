Claro! Aqui está um código complexo em Groovy que irá calcular a sequência de Fibonacci até um determinado número:

```groovy
def calcularFibonacci(int n) {
    def fibonacci = [0, 1]
    
    if (n <= 0) {
        println "Por favor, informe um número maior que zero."
        return
    }
    
    if (n == 1) {
        println fibonacci[0]
        return
    }
    
    if (n == 2) {
        println "${fibonacci[0]}, ${fibonacci[1]}"
        return
    }
    
    for (int i = 2; i < n; i++) {
        def fib = fibonacci[i-1] + fibonacci[i-2]
        fibonacci << fib
    }
    
    println fibonacci.join(", ")
}

def numeroMaximo = 10
calcularFibonacci(numeroMaximo)
```

Explicação do código:

1. Definimos uma função `calcularFibonacci` que recebe um parâmetro `n` representando o número máximo da sequência de Fibonacci a ser gerada.

2. Inicializamos a lista `fibonacci` com os primeiros dois números da sequência.

3. Em seguida, verificamos alguns casos especiais:
   - Se `n` for menor ou igual a zero, exibimos uma mensagem informando que o número deve ser maior que zero.
   - Se `n` for igual a 1, exibimos apenas o primeiro número da sequência e retornamos.
   - Se `n` for igual a 2, exibimos os dois primeiros números da sequência separados por vírgula e retornamos.

4. Utilizamos um loop `for` para gerar os próximos números da sequência de Fibonacci. Começamos a iteração a partir do terceiro número (índice 2) e continuamos até o número máximo fornecido.

5. Em cada iteração, calculamos o próximo número da sequência somando os dois últimos números calculados e adicionamos esse número à lista `fibonacci`.

6. Por fim, exibimos todos os números da lista `fibonacci` separados por vírgula chamando `println fibonacci.join(", ")`.

7. Na última linha, definimos um valor para `numeroMaximo` (neste caso, 10) e chamamos a função `calcularFibonacci` com esse valor. Você pode alterar o valor de `numeroMaximo` para obter uma sequência de Fibonacci maior.