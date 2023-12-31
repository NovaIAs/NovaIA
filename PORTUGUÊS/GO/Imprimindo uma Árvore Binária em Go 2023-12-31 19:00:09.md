Claro! Aqui está um código complexo em Go que utiliza a biblioteca "fmt" para imprimir uma árvore binária de forma recursiva:

```go
package main

import "fmt"

type No struct {
    Valor   int
    Esquerda *No
    Direita  *No
}

func main() {
    raiz := &No{
        Valor: 1,
        Esquerda: &No{
            Valor: 2,
            Esquerda: &No{
                Valor: 4,
                Esquerda: nil,
                Direita:  nil,
            },
            Direita: &No{
                Valor: 5,
                Esquerda: nil,
                Direita:  nil,
            },
        },
        Direita: &No{
            Valor: 3,
            Esquerda: &No{
                Valor: 6,
                Esquerda: nil,
                Direita:  nil,
            },
            Direita: &No{
                Valor: 7,
                Esquerda: nil,
                Direita:  nil,
            },
        },
    }

    imprimirArvore(raiz)
}

func imprimirArvore(no *No) {
    imprimirArvoreAux(no, 0)
}

func imprimirArvoreAux(no *No, nivel int) {
    if no == nil {
        return
    }

    imprimirArvoreAux(no.Direita, nivel+1)

    for i := 0; i < nivel; i++ {
        fmt.Print("   ")
    }

    fmt.Println(no.Valor)

    imprimirArvoreAux(no.Esquerda, nivel+1)
}
```

Neste código, criamos uma estrutura `No` que representa um nó em uma árvore binária. Cada nó tem um valor e ponteiros para os nós esquerda e direita. Em seguida, definimos uma função `imprimirArvore` que imprime a árvore binária a partir do nó raiz.

A função `imprimirArvoreAux` é uma função auxiliar que é chamada recursivamente para percorrer a árvore de forma inversa. Isso é necessário para imprimir os nós da árvore em ordem decrescente.

No `main`, criamos uma árvore com valores de 1 a 7 e chamamos a função `imprimirArvore` para exibir a árvore na saída padrão.