Claro, vou criar um código em PORTUGOL para resolver o problema das raízes de uma equação quadrática. Aqui está o código:

```
algoritmo raizes_equacao_quadratica
var
   a, b, c, delta, x1, x2: real

inicio
   escreva("Digite o coeficiente a: ")
   leia(a)
   escreva("Digite o coeficiente b: ")
   leia(b)
   escreva("Digite o coeficiente c: ")
   leia(c)

   delta <- (b * b) - (4 * a * c)

   se delta < 0 entao
      escreva("A equação não possui raízes reais.")
   senao se delta = 0 entao
      x1 <- (-b) / (2 * a)
      escreva("A equação possui uma raiz real: ", x1)
   senao
      x1 <- ((-b) + sqrt(delta)) / (2 * a)
      x2 <- ((-b) - sqrt(delta)) / (2 * a)
      escreva("A equação possui duas raízes reais: ", x1, " e ", x2)
   fimse
fimalgoritmo
```

Esse código permite que o usuário insira os coeficientes a, b e c de uma equação quadrática e, em seguida, calcula o valor do delta. Com base no valor do delta, o código determina se a equação possui raízes reais e, caso positivo, mostra essas raízes.