Claro! Aqui está um código complexo em Groovy que utiliza recursão para calcular o fatorial de um número:

```groovy
def calcularFatorial(numero) {
    if (numero == 0 || numero == 1) {
        return 1
    } else {
        return numero * calcularFatorial(numero - 1)
    }
}

def numero = 5
def resultado = calcularFatorial(numero)
println("O fatorial de $numero é $resultado.")
```

Neste código, temos uma função chamada `calcularFatorial` que recebe um parâmetro `numero` e retorna o fatorial desse número. 

Primeiro, verificamos se o número é igual a 0 ou 1. Se for, retornamos 1, pois o fatorial de 0 e 1 é sempre 1. Caso contrário, calculamos o fatorial do número subtraindo 1 e multiplicando pelo próprio número. Essa é a essência do cálculo do fatorial utilizando recursão.

Em seguida, definimos uma variável `numero` com o valor 5 (você pode alterar para qualquer outro número que desejar). Chamamos a função `calcularFatorial` passando o `numero` como argumento e armazenamos o resultado na variável `resultado`.

Por fim, imprimimos uma mensagem que exibe o valor de `numero` e `resultado` utilizando interpolação de strings.

Espero que este código diferenciado atenda às suas expectativas! Se você tiver mais alguma solicitação, estou à disposição.