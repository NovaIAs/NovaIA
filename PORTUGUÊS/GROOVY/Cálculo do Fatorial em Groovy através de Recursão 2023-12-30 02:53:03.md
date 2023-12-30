Claro! Aqui está um código complexo em Groovy que calcula o fatorial de um número de forma recursiva:

```groovy
def fatorial(numero) {
    if (numero == 0 || numero == 1) {
        return 1
    } else {
        return numero * fatorial(numero - 1)
    }
}

def numero = 5
def resultado = fatorial(numero)

println "O fatorial de $numero é $resultado."
```

Neste código, a função `fatorial` recebe um número como parâmetro e retorna o fatorial desse número. O fatorial de um número é o produto de todos os números inteiros positivos menores ou iguais a ele. A função utiliza recursão para calcular o fatorial.

No início da função, verificamos se o número é igual a 0 ou 1. Se sim, retornamos 1, pois o fatorial de 0 e 1 é sempre 1.

Caso contrário, multiplicamos o número pelo fatorial do número anterior. Isso é feito chamando a própria função `fatorial` com o parâmetro `numero - 1`. Essa chamada recursiva é repetida até que o número seja igual a 0 ou 1, quando a função começará a retornar os resultados calculados.

No final do código, definimos um valor para a variável `numero` (neste caso, 5) e chamamos a função `fatorial` passando esse valor como argumento. O resultado é armazenado na variável `resultado` e, em seguida, é exibida uma mensagem indicando o número e seu fatorial correspondente.

Espero que este código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.