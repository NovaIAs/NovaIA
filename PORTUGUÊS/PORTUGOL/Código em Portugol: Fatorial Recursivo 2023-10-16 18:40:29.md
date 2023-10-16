Claro! Aqui está um exemplo de um código complexo em Portugol que utiliza recursão para calcular o fatorial de um número:

```
algoritmo FatorialRecursivo

funcao fatorial(numero: inteiro): inteiro
inicio
    se numero igual a 0 entao
        retorne 1
    senao
        retorne numero * fatorial(numero - 1)
    fimse
fimfuncao

funcao principal()
inicio
    escreva("Digite um número inteiro positivo: ")
    leia(numero)
    
    se numero < 0 entao
        escreva("Número inválido. Digite um número inteiro positivo.")
    senao
        resultado <- fatorial(numero)
        escreva("O fatorial de ", numero, " é: ", resultado)
    fimse
fimfuncao

```

Explicação do código:

1. Primeiramente, definimos uma função chamada `fatorial` que recebe um parâmetro `numero` do tipo inteiro e retorna o fatorial desse número.
2. Dentro da função `fatorial`, verificamos se o número é igual a zero. Se for, retornamos 1, pois o fatorial de 0 é igual a 1.
3. Caso contrário, utilizamos a recursão para calcular o fatorial do número. Chamamos a função `fatorial` novamente, passando como argumento o valor do número decrementado em 1, e multiplicamos esse valor pelo número atual.
4. Na função principal, solicitamos ao usuário que digite um número inteiro positivo.
5. Verificamos se o número é menor que zero. Se for, exibimos uma mensagem de erro informando que o número é inválido.
6. Caso contrário, chamamos a função `fatorial` passando o número digitado como argumento e armazenamos o resultado em uma variável chamada `resultado`.
7. Por fim, exibimos uma mensagem informando o valor do fatorial do número digitado.

Esse código utiliza a recursão para calcular o fatorial de um número, o que significa que a função `fatorial` chama a si mesma várias vezes até atingir a condição de parada (quando o número é igual a zero). Isso permite calcular o fatorial de forma eficiente e elegante.