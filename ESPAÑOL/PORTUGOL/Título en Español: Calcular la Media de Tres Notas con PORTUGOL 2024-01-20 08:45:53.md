```portuguol
programa CalcularMedia {
    função inicio() {
        real nota1, nota2, nota3, media
        escreva("Informe a primeira nota: ")
        leia(nota1)
        escreva("Informe a segunda nota: ")
        leia(nota2)
        escreva("Informe a terceira nota: ")
        leia(nota3)
        media = (nota1 + nota2 + nota3) / 3
        se (media >= 7) {
            escreva("Aprovado com média ", media)
        } senão se (media >= 5) {
            escreva("Recuperação com média ", media)
        } senão {
            escreva("Reprovado com média ", media)
        }
    }
}
```

Este código calcula a média de três notas e imprime o resultado na tela.

O programa começa com a função `inicio()` que é o ponto de entrada do programa.

Dentro da função `inicio()`, são declaradas as variáveis `nota1`, `nota2`, `nota3` e `media`.

As variáveis `nota1`, `nota2` e `nota3` são usadas para armazenar as três notas que serão calculadas.

A variável `media` é usada para armazenar o valor da média.

O programa usa a função `escreva()` para imprimir uma mensagem na tela solicitando que o usuário informe a primeira nota.

Em seguida, o programa usa a função `leia()` para ler a primeira nota informada pelo usuário.

O mesmo procedimento é repetido para as notas 2 e 3.

Depois de ler as três notas, o programa calcula a média usando a seguinte fórmula:

```
media = (nota1 + nota2 + nota3) / 3
```

O resultado da média é armazenado na variável `media`.

O programa usa a função `se` para verificar se a média é maior ou igual a 7.

Se a média for maior ou igual a 7, o programa imprime uma mensagem na tela informando que o aluno foi aprovado com a média calculada.

Se a média for maior ou igual a 5, o programa imprime uma mensagem na tela informando que o aluno está em recuperação com a média calculada.

Se a média for menor que 5, o programa imprime uma mensagem na tela informando que o aluno foi reprovado com a média calculada.