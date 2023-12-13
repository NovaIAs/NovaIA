Claro! Aqui está um código em Groovy que gera uma sequência de Fibonacci invertida:

```groovy
def fibonacciInvertida(n) {
    def fibonacci = []
    fibonacci << 0
    fibonacci << 1
    
    for (int i = 2; i < n; i++) {
        fibonacci << fibonacci[i-1] + fibonacci[i-2]
    }
    
    fibonacci.reverse()
    return fibonacci
}

def n = 10
def resultado = fibonacciInvertida(n)
println "Sequência de Fibonacci invertida até o número ${n}:"
println resultado
```

Neste código, definimos uma função chamada `fibonacciInvertida` que recebe um argumento `n`, representando o número de elementos da sequência de Fibonacci que queremos gerar.

Dentro da função, criamos uma lista vazia chamada `fibonacci` e inserimos os dois primeiros elementos da sequência (`0` e `1`) utilizando o operador `<<`.

Em seguida, utilizamos um loop `for` para gerar os elementos restantes da sequência. Começamos no índice `2` e percorremos até `n-1`. A cada iteração, adicionamos à lista `fibonacci` a soma dos dois elementos anteriores.

Após gerar a sequência completa, utilizamos o método `reverse` para inverter a ordem dos elementos na lista.

Por fim, retornamos a lista `fibonacci` como resultado e a imprimimos na tela utilizando `println`.

Neste exemplo, o código gera a sequência de Fibonacci invertida até o número `10`, mas você pode modificar o valor de `n` para gerar uma sequência maior ou menor.